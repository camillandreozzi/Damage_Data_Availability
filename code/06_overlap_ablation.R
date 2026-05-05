# =====================================================================
# 06_overlap_ablation.R
# ---------------------------------------------------------------------
# Constructs the nested predictor sets K5 -> K1 by sequentially removing
# one variable from F1, in a pre-registered order, and re-evaluates all
# model classes at each level.
#
# Pre-registered removal order (most-to-first-removed):
#   K5 -> K4: drop floor_area_m2     (most likely to be ambiguous unit-wise)
#   K4 -> K3: drop total_height_m    (rarely available outside engineering surveys)
#   K3 -> K2: drop age               (definitionally inconsistent across protocols)
#   K2 -> K1: drop no_stories        (sometimes only footprint area is recorded)
#   K1: hazard_proxy_distance_km only (kept until last; survives in any data regime)
#
# Output:
#   * R/data/outputs/results/overlap_ablation_long.rds
# =====================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(rsample)
  library(yardstick)
  library(ranger)
  library(nnet)
  library(MASS)
})

source("~/Documents/Claude/Projects/Damage Data Publication/R/code/04_country_specific_models.R", local = TRUE)

feat_dir <- "~/Documents/Claude/Projects/Damage Data Publication/R/data/outputs/features"
out_dir  <- "~/Documents/Claude/Projects/Damage Data Publication/R/data/outputs/results"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

set.seed(20260429)

F1_pooled <- readRDS(file.path(feat_dir, "F1_pooled.rds"))

# ---- Nested feature sets --------------------------------------------
nested_sets <- list(
  K5 = c("age", "no_stories", "floor_area_m2",
         "total_height_m", "hazard_proxy_distance_km"),
  K4 = c("age", "no_stories",
         "total_height_m", "hazard_proxy_distance_km"),
  K3 = c("age", "no_stories", "hazard_proxy_distance_km"),
  K2 = c("no_stories", "hazard_proxy_distance_km"),
  K1 = c("hazard_proxy_distance_km")
)

# ---- Run ablation ---------------------------------------------------
splits <- rsample::vfold_cv(F1_pooled, v = 5, strata = country)

results <- list()
for (k_name in names(nested_sets)) {
  preds <- nested_sets[[k_name]]

  for (task in c("5class", "binary")) {
    for (i in seq_len(nrow(splits))) {
      sp    <- splits$splits[[i]]
      train <- analysis(sp)
      test  <- assessment(sp)

      models <- if (task == "binary") c("logistic", "rf", "nnet") else c("polr", "rf", "nnet")

      for (mn in models) {
        target <- if (task == "binary") "y2" else "y5"
        train_i <- train |> drop_na(all_of(c(target, preds)))
        test_i  <- test  |> drop_na(all_of(c(target, preds)))

        # Guard: empty after drop_na, or only one class left.
        # Without this, ranger aborts the whole call with
        # "sample_fraction too small, no observations sampled".
        if (nrow(train_i) < 5 || nrow(test_i) < 1 ||
            dplyr::n_distinct(train_i[[target]], na.rm = TRUE) < 2) {
          out <- tibble::tibble(
            metric = NA_character_, value = NA_real_,
            note = sprintf("skipped: train=%d test=%d classes=%d",
                           nrow(train_i), nrow(test_i),
                           dplyr::n_distinct(train_i[[target]], na.rm = TRUE)))
        } else {
          out <- tryCatch(
            fit_and_evaluate(train_i, test_i, mn, task, target, preds),
            error = function(e) tibble::tibble(
              metric = NA_character_, value = NA_real_,
              note = paste0("error: ", conditionMessage(e)))
          )
        }
        out$model       <- mn
        out$split_id    <- i
        out$feature_set <- k_name
        out$predictors  <- length(preds)
        out$task        <- task
        out$train_domain <- "Pooled"
        out$test_domain  <- "Pooled_holdout"
        results[[length(results) + 1]] <- out
      }
    }
  }
}

results <- bind_rows(results)
saveRDS(results, file.path(out_dir, "overlap_ablation_long.rds"))

# ---- Summary: model-vs-overlap variance decomposition ---------------
ablation_summary <- results |>
  filter(metric %in% c("auc", "macro_f1")) |>
  group_by(task, metric, feature_set, model) |>
  summarise(mean_value = mean(value, na.rm = TRUE),
            sd_value   = sd(value, na.rm = TRUE),
            .groups = "drop")

write_csv(ablation_summary,
          file.path(out_dir, "overlap_ablation_summary.csv"))

message("06_overlap_ablation: ", nrow(results), " result rows written; ",
        "summary written to overlap_ablation_summary.csv")
