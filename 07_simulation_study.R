# =====================================================================
# 07_simulation_study.R
# ---------------------------------------------------------------------
# Simulation that varies compatibility regimes around a latent severity
# process and quantifies how much variance in performance is attributable
# to the regime versus to the model class.
#
# Latent model:
#   eta_i = X_shared %*% beta_s + X_country %*% beta_c
#         + alpha[country_i] + epsilon_i
# Ordinal labels:
#   y5 = k iff tau_{k-1} < eta <= tau_k,    k in 1..5
#   y2 = 1{y5 in {4,5}}
#
# Observation layer (per country):
#   * p_shared        proportion of truly shared predictors observed
#   * sigma_measure   country-specific measurement-error sd
#   * p_missing       per-cell missingness probability
#   * label_noise     P(label flipped to adjacent grade)
#   * sample_ratio    ratio of country sample sizes
#   * country_shift   mean shift in eta between countries
#
# Output:
#   * R/outputs/sim/simulation_long.rds
#   * R/outputs/sim/variance_decomp.csv
# =====================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(ranger)
  library(nnet)
  library(MASS)
  library(yardstick)
})

source("R/04_country_specific_models.R", local = TRUE)

out_dir <- "~/Documents/Claude/Projects/Damage Data Publication/R/outputs/sim"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

set.seed(20260430)

# ---- Generators -----------------------------------------------------
make_dgp <- function(n, p_shared = 1.0, sigma_measure = 0,
                     country_shift = 0, label_noise = 0,
                     n_predictors = 5) {
  X_true   <- matrix(rnorm(n * n_predictors), nrow = n)
  beta_s   <- rnorm(n_predictors, sd = 1) * 0.7
  alpha    <- country_shift
  eta      <- as.numeric(X_true %*% beta_s) + alpha + rnorm(n, sd = 1.0)
  taus     <- quantile(eta, probs = c(0.2, 0.4, 0.6, 0.8))
  y5       <- as.integer(cut(eta, breaks = c(-Inf, taus, Inf), labels = 1:5))
  if (label_noise > 0) {
    flip <- runif(n) < label_noise
    shift <- sample(c(-1L, 1L), n, replace = TRUE)
    y5_noisy <- pmin(pmax(y5 + ifelse(flip, shift, 0L), 1L), 5L)
    y5 <- y5_noisy
  }
  y2 <- as.integer(y5 %in% c(4L, 5L))

  # Observation layer: drop a random subset of predictors per country
  k_obs <- max(1L, floor(p_shared * n_predictors))
  obs_idx <- sort(sample(seq_len(n_predictors), k_obs))
  X_obs <- X_true[, obs_idx, drop = FALSE]
  if (sigma_measure > 0) {
    X_obs <- X_obs + matrix(rnorm(length(X_obs), sd = sigma_measure),
                            nrow = nrow(X_obs))
  }
  colnames(X_obs) <- paste0("x", obs_idx)

  tibble::as_tibble(X_obs) |>
    mutate(y5 = y5, y2 = y2)
}

make_two_country <- function(n_a = 1000, n_b = 1000,
                             p_shared = 1.0,
                             sigma_measure = 0,
                             country_shift = 0,
                             label_noise = 0,
                             n_predictors = 5) {
  dplyr::bind_rows(
    make_dgp(
      n = n_a,
      p_shared = p_shared,
      sigma_measure = sigma_measure,
      country_shift = 0,
      label_noise = label_noise,
      n_predictors = n_predictors
    ) |>
      dplyr::mutate(country = "A"),
    
    make_dgp(
      n = n_b,
      p_shared = p_shared,
      sigma_measure = sigma_measure,
      country_shift = country_shift,
      label_noise = label_noise,
      n_predictors = n_predictors
    ) |>
      dplyr::mutate(country = "B")
  )
}

# ---- Scenario grid --------------------------------------------------
grid <- expand_grid(
  p_shared      = c(1.0, 0.8, 0.6, 0.4, 0.2),
  sigma_measure = c(0.0, 0.3, 0.6),
  label_noise   = c(0.0, 0.1, 0.2),
  rep           = 1:3
)

# ---- Run scenarios --------------------------------------------------
results <- list()
for (i in seq_len(nrow(grid))) {
  cfg <- grid[i, ]
  df  <- make_two_country(
    n_a = 800, n_b = 800,
    p_shared      = cfg$p_shared,
    sigma_measure = cfg$sigma_measure,
    label_noise   = cfg$label_noise,
    country_shift = 0.5
  )
  preds <- setdiff(names(df), c("y5", "y2", "country"))

  if (length(preds) == 0) next

  # within-pool 80/20 split
  idx   <- sample(seq_len(nrow(df)), size = floor(0.8 * nrow(df)))
  train <- df[idx,  ]
  test  <- df[-idx, ]

  for (task in c("5class", "binary")) {
    target <- if (task == "binary") "y2" else "y5"
    models <- if (task == "binary") c("logistic", "rf", "nnet") else c("polr", "rf", "nnet")
    for (mn in models) {
      out <- tryCatch(
        fit_and_evaluate(train, test, mn, task, target, preds),
        error = function(e) tibble(metric = NA, value = NA_real_)
      )
      out$model         <- mn
      out$task          <- task
      out$p_shared      <- cfg$p_shared
      out$sigma_measure <- cfg$sigma_measure
      out$label_noise   <- cfg$label_noise
      out$rep           <- cfg$rep
      out$n_preds_obs   <- length(preds)
      results[[length(results) + 1]] <- out
    }
  }
}
results <- bind_rows(results)
saveRDS(results, file.path(out_dir, "simulation_long.rds"))

# ---- Variance decomposition -----------------------------------------
# For each (task, metric), partition variance of value across:
#   * compatibility regime (p_shared * sigma_measure * label_noise)
#   * model family
# Use a simple 2-way ANOVA on the cross-classified factors.
do_anova <- function(df, metric_name) {
  d <- df |> filter(metric == metric_name) |>
    mutate(regime = paste(p_shared, sigma_measure, label_noise, sep = "/"),
           model  = factor(model))
  if (nrow(d) < 4) return(NULL)
  fit <- aov(value ~ regime + model, data = d)
  ss <- summary(fit)[[1]][, "Sum Sq"]
  tibble(metric = metric_name,
         var_regime = ss[1] / sum(ss),
         var_model  = ss[2] / sum(ss),
         var_resid  = ss[3] / sum(ss))
}

var_decomp <- map_dfr(c("auc", "macro_f1", "accuracy"),
                      ~ do_anova(results, .x))
write_csv(var_decomp, file.path(out_dir, "variance_decomp.csv"))

message("07_simulation_study: ", nrow(results),
        " sim rows written; variance decomposition in variance_decomp.csv")
