# =====================================================================
# 10_subgroup_robustness.R
# ---------------------------------------------------------------------
# Gap-filling robustness analyses recommended after the diagnostic
# pass. Implements:
#
#   (1) RC-only Nepal subgroup transfer:
#       Restrict Nepal to buildings whose dominant superstructure
#       indicator is reinforced concrete, then re-run cross-country
#       transfer. If negative transfer disappears under building-stock
#       matching, the mechanism is "different stock" rather than
#       "different physics." If it persists, the mechanism survives
#       the most plausible confound.
#
#   (2) F2 transfer experiment:
#       Re-run cross-country transfer using F2 (= F1 + coarse
#       structural family) to test whether adding the structural
#       descriptor heals negative transfer.
#
#   (3) Bootstrap CIs on the transfer-matrix off-diagonal cells:
#       For Nepal -> Türkiye and Türkiye -> Nepal, bootstrap the
#       test set 200 times and report a 95% CI on AUC. Fixes the
#       point-estimate weakness of the headline transfer numbers.
#
#   (4) Hazard-proxy-removed transfer:
#       Drop hazard_proxy_distance_km from F1 and re-run transfer.
#       Tests whether negative transfer is a consequence of relying
#       on a weakly harmonised hazard proxy, or whether it persists
#       on geometry alone.
#
#   (5) Bivariate empirical damage curves:
#       For each F1 predictor, plot the observed P(severe) by decile
#       of the predictor, separately by country. Provides model-free
#       evidence for the opposing-relationship interpretation.
#
# Outputs (under R/outputs/diagnostics/):
#   * rc_only_transfer.csv
#   * f2_transfer.csv
#   * transfer_bootstrap_cis.csv
#   * hazard_removed_transfer.csv
#   * bivariate_damage_curves.rds
#   * fig08_bivariate_damage_curves.pdf  (also written to paper/figures/)
# =====================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(rsample)
  library(yardstick)
  library(ranger)
})

source("~/Documents/Claude/Projects/Damage Data Publication/R/04_country_specific_models.R", local = TRUE)

feat_dir <- "~/Documents/Claude/Projects/Damage Data Publication/R/outputs/features"
out_dir  <- "~/Documents/Claude/Projects/Damage Data Publication/R/outputs/diagnostics"
fig_dir  <- "~/Documents/Claude/Projects/Damage Data Publication/paper/figures"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

set.seed(20260601)

CORE_PREDICTORS <- c("age", "no_stories", "floor_area_m2",
                     "total_height_m", "hazard_proxy_distance_km")

F1_pooled <- readRDS(file.path(feat_dir, "F1_pooled.rds"))
F2_pooled <- readRDS(file.path(feat_dir, "F2_pooled.rds"))
F3_Nepal  <- readRDS(file.path(feat_dir, "F3_Nepal.rds"))

# ---------------------------------------------------------------------
# Helper: train rf, score on test, return AUC + standard binary metrics
# ---------------------------------------------------------------------
fit_rf_binary <- function(train, test, predictors,
                          target = "y2") {
  if (nrow(train) < 5 || nrow(test) < 1 ||
      dplyr::n_distinct(train[[target]], na.rm = TRUE) < 2 ||
      dplyr::n_distinct(test[[target]],  na.rm = TRUE) < 2) {
    return(tibble::tibble(metric = NA_character_, value = NA_real_,
                          note = "insufficient data"))
  }
  train[[target]] <- factor(train[[target]], levels = c(0, 1))
  test[[target]]  <- factor(test[[target]],  levels = c(0, 1))
  m <- ranger::ranger(
    reformulate(predictors, response = target),
    data = train, num.trees = 300, probability = TRUE,
    min.node.size = 1, replace = TRUE
  )
  prob <- predict(m, data = test)$predictions[, "1"]
  cls  <- as.integer(prob >= 0.5)
  truth <- factor(test[[target]], levels = c(0, 1))
  pred  <- factor(cls, levels = c(0, 1))
  safe <- function(x) suppressWarnings(tryCatch(x, error = function(e) NA_real_))
  tibble::tibble(
    metric = c("accuracy", "auc", "brier", "f1"),
    value  = c(
      safe(mean(pred == truth)),
      safe(yardstick::roc_auc_vec(truth, prob, event_level = "second")),
      safe(mean((as.numeric(as.character(truth)) - prob)^2)),
      safe(yardstick::f_meas_vec(truth, pred, event_level = "second"))
    )
  )
}

# ---------------------------------------------------------------------
# (1) RC-only Nepal subgroup transfer
# ---------------------------------------------------------------------
# Identify RC-only Nepal subset using the superstructure indicators in
# F3_Nepal and join the F1 harmonised features to it.

rc_subset_ids <- F3_Nepal |>
  dplyr::filter(has_superstructure_rc_engineered == 1 |
                  has_superstructure_rc_non_engineered == 1) |>
  dplyr::pull(asset_id) |>
  as.character()

nepal_rc_F1 <- F1_pooled |>
  dplyr::filter(country == "Nepal", asset_id %in% rc_subset_ids)
turkiye_F1  <- F1_pooled |> dplyr::filter(country == "Türkiye")

message(sprintf("RC-only Nepal subset: %d buildings (of %d Nepal in F1)",
                nrow(nepal_rc_F1),
                sum(F1_pooled$country == "Nepal")))

rc_results <- bind_rows(
  fit_rf_binary(nepal_rc_F1, turkiye_F1,  CORE_PREDICTORS) |>
    mutate(train = "Nepal_RC_only", test = "Türkiye"),
  fit_rf_binary(turkiye_F1,  nepal_rc_F1, CORE_PREDICTORS) |>
    mutate(train = "Türkiye",       test = "Nepal_RC_only"),
  fit_rf_binary(nepal_rc_F1, nepal_rc_F1[sample(nrow(nepal_rc_F1),
                                                floor(0.2 * nrow(nepal_rc_F1))), ],
                CORE_PREDICTORS) |>
    mutate(train = "Nepal_RC_only", test = "Nepal_RC_only_holdout")
)
write_csv(rc_results, file.path(out_dir, "rc_only_transfer.csv"))

# ---------------------------------------------------------------------
# (2) F2 transfer experiment (F1 + coarse structural family)
# ---------------------------------------------------------------------
F2_predictors <- c(CORE_PREDICTORS, "structural_family")

nepal_F2 <- F2_pooled |> dplyr::filter(country == "Nepal") |>
  dplyr::mutate(structural_family = factor(structural_family))
tur_F2   <- F2_pooled |> dplyr::filter(country == "Türkiye") |>
  dplyr::mutate(structural_family = factor(structural_family,
                                           levels = levels(nepal_F2$structural_family)))

f2_results <- bind_rows(
  fit_rf_binary(nepal_F2, tur_F2,  F2_predictors) |>
    mutate(train = "Nepal", test = "Türkiye", feature_set = "F2"),
  fit_rf_binary(tur_F2,   nepal_F2, F2_predictors) |>
    mutate(train = "Türkiye", test = "Nepal", feature_set = "F2")
)
write_csv(f2_results, file.path(out_dir, "f2_transfer.csv"))

# ---------------------------------------------------------------------
# (3) Bootstrap 95% CIs on transfer-matrix off-diagonal AUC
# ---------------------------------------------------------------------
nepal_F1   <- F1_pooled |> dplyr::filter(country == "Nepal")
turkiye_F1 <- F1_pooled |> dplyr::filter(country == "Türkiye")

bootstrap_auc <- function(train, test, predictors, B = 200) {
  train_clean <- train |> drop_na(all_of(c("y2", predictors)))
  test_clean  <- test  |> drop_na(all_of(c("y2", predictors)))
  if (nrow(train_clean) < 5 || nrow(test_clean) < 5) {
    return(tibble(auc = NA_real_, lo = NA_real_, hi = NA_real_, B = 0L))
  }
  train_clean$y2 <- factor(train_clean$y2, levels = c(0, 1))
  m <- ranger::ranger(
    reformulate(predictors, response = "y2"),
    data = train_clean, num.trees = 300, probability = TRUE,
    min.node.size = 1, replace = TRUE
  )
  prob <- predict(m, data = test_clean)$predictions[, "1"]
  truth <- factor(test_clean$y2, levels = c(0, 1))

  point <- suppressWarnings(yardstick::roc_auc_vec(truth, prob,
                                                   event_level = "second"))
  boot_aucs <- replicate(B, {
    idx <- sample(seq_along(prob), replace = TRUE)
    suppressWarnings(yardstick::roc_auc_vec(truth[idx], prob[idx],
                                            event_level = "second"))
  })
  tibble(auc = point,
         lo  = quantile(boot_aucs, 0.025, na.rm = TRUE),
         hi  = quantile(boot_aucs, 0.975, na.rm = TRUE),
         B   = B)
}

ci_results <- bind_rows(
  bootstrap_auc(nepal_F1,   turkiye_F1, CORE_PREDICTORS) |>
    mutate(train = "Nepal", test = "Türkiye"),
  bootstrap_auc(turkiye_F1, nepal_F1,   CORE_PREDICTORS) |>
    mutate(train = "Türkiye", test = "Nepal")
)
write_csv(ci_results, file.path(out_dir, "transfer_bootstrap_cis.csv"))

# ---------------------------------------------------------------------
# (4) Hazard-proxy-removed transfer
# ---------------------------------------------------------------------
GEOMETRY_ONLY <- c("age", "no_stories", "floor_area_m2", "total_height_m")

hazard_removed <- bind_rows(
  fit_rf_binary(nepal_F1,   turkiye_F1, GEOMETRY_ONLY) |>
    mutate(train = "Nepal", test = "Türkiye", feature_set = "F1_no_hazard"),
  fit_rf_binary(turkiye_F1, nepal_F1,   GEOMETRY_ONLY) |>
    mutate(train = "Türkiye", test = "Nepal", feature_set = "F1_no_hazard")
)
write_csv(hazard_removed, file.path(out_dir, "hazard_removed_transfer.csv"))

# ---------------------------------------------------------------------
# (5) Bivariate empirical damage curves: P(severe) by predictor decile
# ---------------------------------------------------------------------
df <- F1_pooled |>
  drop_na(all_of(c("y2", CORE_PREDICTORS)))

curves <- map_dfr(CORE_PREDICTORS, function(v) {
  df |>
    group_by(country) |>
    mutate(decile = ntile(.data[[v]], 10)) |>
    group_by(country, decile) |>
    summarise(
      x_mean   = mean(.data[[v]], na.rm = TRUE),
      severe   = mean(y2, na.rm = TRUE),
      n        = n(),
      .groups  = "drop"
    ) |>
    mutate(predictor = v)
})

saveRDS(curves, file.path(out_dir, "bivariate_damage_curves.rds"))

p_curves <- ggplot(curves, aes(x_mean, severe, color = country)) +
  geom_line(aes(group = country), linewidth = 0.6, alpha = 0.8) +
  geom_point(aes(size = n), alpha = 0.7) +
  facet_wrap(~ predictor, scales = "free_x") +
  scale_color_viridis_d(option = "C", end = 0.7) +
  scale_size_continuous(
    trans = "log10",
    range = c(1.2, 5),
    breaks = c(200, 1000, 10000, 100000),
    labels = c("200", "1k", "10k", "100k"),
    name = "Sample size"
  ) +
  labs(
    x = NULL,
    y = "Observed P(severe)",
    title = "Bivariate empirical damage curves by predictor decile",
    subtitle = "Diverging country curves indicate predictors with opposite operational meaning (model-free)"
  ) +
  theme_minimal(base_size = 11)

ggsave(file.path(fig_dir, "fig08_bivariate_damage_curves.pdf"),
       p_curves, width = 9, height = 5)

message("10_subgroup_robustness: outputs written under ", out_dir,
        " and ", fig_dir)
