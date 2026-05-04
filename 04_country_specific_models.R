# =====================================================================
# 04_country_specific_models.R
# ---------------------------------------------------------------------
# Within-country benchmarks on F4_Nepal and F4_Türkiye for both the
# 5-class and binary tasks. Uses repeated stratified train/test splits
# and the same evaluation metrics that downstream scripts will use.
#
# Models:
#   * Logistic / multinomial baseline   (nnet::multinom or glm)
#   * Proportional-odds ordinal model   (MASS::polr) -- 5-class only
#   * Random Forest                     (ranger)
#   * Neural Network                    (nnet)
#
# Outputs:
#   * R/outputs/results/within_country_long.rds
#       columns: train_domain, test_domain, feature_set, model, task,
#                metric, value, split_id
# =====================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(rsample)
  library(yardstick)
  library(ranger)
  library(nnet)
  library(MASS)
})

feat_dir <- "~/Documents/Claude/Projects/Damage Data Publication/R/outputs/features"
out_dir  <- "~/Documents/Claude/Projects/Damage Data Publication/R/outputs/results"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

set.seed(202604)

# Null-coalescing operator (used by fit_and_evaluate)
`%||%` <- function(a, b) if (is.null(a)) b else a

# ---- Config ---------------------------------------------------------
N_SPLITS       <- 5
TRAIN_FRACTION <- 0.8

# ---- Helper: repeated stratified splits -----------------------------
make_splits <- function(df, target, n = N_SPLITS, prop = TRAIN_FRACTION) {
  rsample::vfold_cv(df, v = n, strata = !!sym(target)) |>
    mutate(prop = prop)
}

# ---- Helper: evaluation metric bundles ------------------------------
eval_5class <- function(truth, pred) {
  # Use the union of observed levels (1..5 union actual values) so the
  # confusion matrix is always a valid square table even when one or
  # more classes never appear in `truth` for this fold.
  all_levels <- sort(unique(c(1:5, as.integer(as.character(truth)),
                              as.integer(as.character(pred)))))
  truth <- factor(truth, levels = all_levels)
  pred  <- factor(pred,  levels = all_levels)
  safe <- function(expr) {
    suppressWarnings(tryCatch(expr, error = function(e) NA_real_))
  }
  cm <- safe(caret::confusionMatrix(pred, truth))
  bal_acc <- if (is.list(cm) && !is.null(cm$table)) {
    safe(mean(diag(cm$table) / rowSums(cm$table), na.rm = TRUE))
  } else NA_real_
  tibble::tibble(
    metric = c("accuracy", "balanced_accuracy", "macro_f1"),
    value  = c(
      safe(mean(pred == truth)),
      bal_acc,
      safe(yardstick::f_meas_vec(truth, pred, estimator = "macro"))
    )
  )
}

eval_binary <- function(truth, pred_class, pred_prob) {
  truth      <- factor(truth, levels = c(0, 1))
  pred_class <- factor(pred_class, levels = c(0, 1))
  # NA-safe wrapper so undefined metrics (e.g. test set with only one
  # class) become NA instead of warnings + non-finite values.
  safe <- function(expr) {
    suppressWarnings(tryCatch(expr, error = function(e) NA_real_))
  }
  tibble::tibble(
    metric = c("accuracy", "sensitivity", "specificity",
               "precision", "f1", "auc", "brier"),
    value = c(
      safe(mean(pred_class == truth)),
      safe(yardstick::sens_vec(truth, pred_class, event_level = "second")),
      safe(yardstick::spec_vec(truth, pred_class, event_level = "second")),
      safe(yardstick::precision_vec(truth, pred_class, event_level = "second")),
      safe(yardstick::f_meas_vec(truth, pred_class, event_level = "second")),
      safe(yardstick::roc_auc_vec(truth, pred_prob, event_level = "second")),
      safe(mean((as.numeric(as.character(truth)) - pred_prob)^2))
    )
  )
}

# ---- Helper: fit + evaluate one (model, split) ----------------------
fit_and_evaluate <- function(train, test, model_name, task, target,
                             predictors) {

  # ---- Input validation: skip degenerate splits ---------------------
  na_row <- function(reason) {
    tibble::tibble(metric = NA_character_, value = NA_real_,
                   note = reason)
  }
  if (is.null(train) || nrow(train) < 5 || is.null(test) || nrow(test) < 1) {
    return(na_row(sprintf("skipped: train=%s test=%s",
                          nrow(train) %||% 0, nrow(test) %||% 0)))
  }
  if (dplyr::n_distinct(train[[target]], na.rm = TRUE) < 2) {
    return(na_row("skipped: <2 classes in train"))
  }

  form <- reformulate(predictors, response = target)

  if (task == "binary") {
    train[[target]] <- factor(train[[target]], levels = c(0, 1))
    test[[target]]  <- factor(test[[target]],  levels = c(0, 1))
  } else {
    # 5-class: factor with explicit ordinal levels so nnet/rf treat it as
    # classification rather than regression. (Was producing NA,NA,NA for
    # nnet 5-class because integer y triggered regression mode.)
    train[[target]] <- factor(train[[target]], levels = 1:5)
    test[[target]]  <- factor(test[[target]],  levels = 1:5)
  }

  switch(model_name,
    "logistic" = {
      if (task == "binary") {
        m <- glm(form, data = train, family = binomial())
        prob  <- predict(m, newdata = test, type = "response")
        cls   <- as.integer(prob >= 0.5)
        eval_binary(as.numeric(as.character(test[[target]])), cls, prob)
      } else {
        m <- nnet::multinom(form, data = train, trace = FALSE)
        cls <- predict(m, newdata = test)
        eval_5class(test[[target]], cls)
      }
    },
    "polr" = {
      stopifnot(task == "5class")
      m <- MASS::polr(form, data = train |>
                        mutate(!!target := factor(!!sym(target), ordered = TRUE)),
                      Hess = TRUE)
      cls <- predict(m, newdata = test)
      eval_5class(test[[target]], cls)
    },
    "rf" = {
      # min.node.size=1 + sample.fraction=min(1, ...) guards against the
      # "sample_fraction too small" abort when classes are very rare.
      m <- ranger::ranger(form, data = train, num.trees = 300,
                          probability   = (task == "binary"),
                          classification = (task == "5class"),
                          min.node.size = 1,
                          replace       = TRUE)
      if (task == "binary") {
        prob <- predict(m, data = test)$predictions[, "1"]
        cls  <- as.integer(prob >= 0.5)
        eval_binary(as.numeric(as.character(test[[target]])), cls, prob)
      } else {
        cls <- predict(m, data = test)$predictions
        eval_5class(test[[target]], cls)
      }
    },
    "nnet" = {
      m <- nnet::nnet(form, data = train, size = 8, maxit = 200,
                      trace = FALSE,
                      linout = FALSE,
                      MaxNWts = 2000)
      if (task == "binary") {
        prob <- as.numeric(predict(m, newdata = test, type = "raw"))
        cls  <- as.integer(prob >= 0.5)
        eval_binary(as.numeric(as.character(test[[target]])), cls, prob)
      } else {
        cls <- predict(m, newdata = test, type = "class")
        eval_5class(test[[target]], cls)
      }
    },
    stop("Unknown model: ", model_name)
  )
}

# ---- Run within-country models for one country/task -----------------
run_within_country <- function(df, country, task, predictors) {
  target <- if (task == "binary") "y2" else "y5"
  df <- df |>
    drop_na(all_of(c(target, predictors)))

  splits <- make_splits(df, target)

  models <- if (task == "binary") c("logistic", "rf", "nnet") else c("polr", "rf", "nnet")

  out <- map_dfr(seq_len(nrow(splits)), function(i) {
    sp    <- splits$splits[[i]]
    train <- analysis(sp)
    test  <- assessment(sp)

    map_dfr(models, function(mn) {
      tryCatch({
        m <- fit_and_evaluate(train, test, mn, task, target, predictors)
        m |> mutate(model = mn, split_id = i)
      }, error = function(e) {
        tibble::tibble(metric = NA, value = NA_real_,
                       model = mn, split_id = i,
                       note = paste0("error: ", conditionMessage(e)))
      })
    })
  })

  out |>
    mutate(train_domain = country, test_domain = country,
           feature_set  = paste0("F4_", country),
           task         = task)
}

# ---- Execute --------------------------------------------------------
F3_Nepal   <- readRDS(file.path(feat_dir, "F3_Nepal.rds"))
F3_Türkiye <- readRDS(file.path(feat_dir, "F3_Türkiye.rds"))

# ---------------------------------------------------------------------
# Country-specific predictor lists
# ---------------------------------------------------------------------

predictors_nepal <- c(
  # Basic geometry / exposure
  "age_building",
  "count_floors_pre_eq",
  "plinth_area_sq_ft",
  "per_height_ft_pre_eq",
  "district_distance_to_earthquakecenter_mi",
  
  # Site / typology
  "land_surface_condition",
  "foundation_type",
  "roof_type",
  "ground_floor_type",
  "position",
  
  # Superstructure indicators
  "has_superstructure_adobe_mud",
  "has_superstructure_mud_mortar_stone",
  "has_superstructure_stone_flag",
  "has_superstructure_cement_mortar_stone",
  "has_superstructure_mud_mortar_brick",
  "has_superstructure_cement_mortar_brick",
  "has_superstructure_timber",
  "has_superstructure_bamboo",
  "has_superstructure_rc_non_engineered",
  "has_superstructure_rc_engineered",
  "has_superstructure_other"
)

predictors_turkiye <- c(
  # Basic geometry
  "age",
  "no_stories",
  "no_stories_above_critical_floor_cf",
  "total_height",
  "floor_area",
  "total_floor_area_above_c_f",
  
  # Structural wall / column quantities
  "c_f_column_area",
  "c_f_concrete_wall_area_ns",
  "c_f_concrete_wall_area_ew",
  "c_f_masonry_wall_area_ns",
  "c_f_masonry_wall_area_ew",
  "critical_aac_masonry_wall_area_ns",
  "critical_aac_masonry_wall_area_ew",
  "c_f_concrete_wall_area_max",
  "c_f_concrete_wall_area_min",
  "c_f_masonry_wall_area_max",
  "c_f_masonry_wall_area_min",
  
  # Engineering indices
  "cd",
  "wd",
  "captive_columns_presence",
  "captive_column_encoding",
  "column_index_percent",
  "wall_index_ns_percent",
  "wall_index_ew_percent",
  "min_wi_percent",
  "priority_index_percent",
  "soft_story",
  "soft_story_encoding",
  
  # Location / site
  "latitude",
  "longitude",
  "v_s30",
  "t_1_n_10",
  "t_1_0_0466h_0_9",
  
  # Distance metrics
  "m7_8_repi",
  "m7_8_rrup",
  "m6_7_repi",
  "m6_7_rrup",
  "m6_3_repi",
  "m6_3_rrup",
  
  # Main hazard intensity measures
  "m7_8_pga",
  "m7_8_pgv",
  "m7_8_psa0_3",
  "m7_8_psa0_6",
  "m7_8_psa1_0",
  "m7_8_ai",
  "m7_8_cav",
  
  # Alternative event/intensity measures
  "m6_7_pga",
  "m6_7_pgv",
  "m6_7_psa0_3",
  "m6_7_psa0_6",
  "m6_7_psa1_0",
  "m6_7_ai",
  "m6_7_cav",
  "m7_5_pga",
  "m7_5_pgv",
  "m7_7_psa0_3",
  "m7_7_psa0_6",
  "m7_7_psa1_0",
  "m7_7_ai",
  "m7_7_cav",
  "m6_3_pga",
  "m6_3_pgv",
  "m6_3_psa0_3",
  "m6_3_psa0_6",
  "m6_3_psa1_0",
  "m6_3_ai",
  "m6_3_cav",
  
  # Structure-period-specific / max hazard summaries
  "m7_8_sa_t1",
  "m6_8_sa_t1",
  "m7_5_sa_t1",
  "m6_4_sa_t1",
  "pga_max",
  "pgv_max",
  "sa_t1_max",
  "ai_max",
  "cav_max"
)

results <- bind_rows(
  run_within_country(F3_Nepal,   "Nepal",   "5class", predictors_nepal),
  run_within_country(F3_Nepal,   "Nepal",   "binary", predictors_nepal),
  run_within_country(F3_Türkiye, "Türkiye", "5class", predictors_turkiye),
  run_within_country(F3_Türkiye, "Türkiye", "binary", predictors_turkiye)
)

saveRDS(results, file.path(out_dir, "within_country_long.rds"))
message("04_country_specific_models: ",
        nrow(results), " result rows written.")
