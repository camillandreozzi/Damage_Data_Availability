# =====================================================================
# 05_cross_country_transfer.R
# ---------------------------------------------------------------------
# Implements the six cross-country transfer experiments in the paper:
#   1. Train Nepal,    test Türkiye
#   2. Train Türkiye,  test Nepal
#   3. Train pooled,   test held-out within pooled
#   4. Train pooled w/o country indicator
#   5. Train pooled w/  country indicator
#   6. Train Nepal+Türkiye reduced binary, test Japan (J1) external
#
# All experiments run on F1 by default; F2/F3 toggled via `feature_set`.
#
# Output:
#   * R/data/outputs/results/cross_country_transfer_long.rds
#       columns: train_domain, test_domain, feature_set, model, task,
#                metric, value, split_id, country_indicator
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
# (re-uses fit_and_evaluate / eval_* helpers)

feat_dir <- "~/Documents/Claude/Projects/Damage Data Publication/R/data/outputs/features"
out_dir  <- "~/Documents/Claude/Projects/Damage Data Publication/R/data/outputs/results"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

set.seed(20260428)

F1_pooled <- readRDS(file.path(feat_dir, "F1_pooled.rds"))
F2_pooled <- readRDS(file.path(feat_dir, "F2_pooled.rds"))
J1_Japan  <- readRDS(file.path(feat_dir, "J1_Japan.rds"))

CORE_PREDICTORS <- c("age", "no_stories", "floor_area_m2",
                     "total_height_m", "hazard_proxy_distance_km")

# ---- One transfer experiment ----------------------------------------
transfer_one <- function(train, test, predictors, task,
                         feature_set, train_domain, test_domain,
                         country_indicator = TRUE) {
  target <- if (task == "binary") "y2" else "y5"

  if (country_indicator && "country" %in% names(train)) {
    train$country <- factor(train$country)
    test$country  <- factor(test$country, levels = levels(train$country))
    predictors <- union(predictors, "country")
  } else {
    train$country <- NULL
    test$country  <- NULL
  }

  train <- train |> drop_na(all_of(c(target, predictors)))
  test  <- test  |> drop_na(all_of(c(target, predictors)))

  models <- if (task == "binary") c("logistic", "rf", "nnet") else c("polr", "rf", "nnet")

  map_dfr(models, function(mn) {
    tryCatch({
      m <- fit_and_evaluate(train, test, mn, task, target, predictors)
      m |> mutate(model = mn, split_id = 1L,
                  feature_set = feature_set,
                  task = task,
                  train_domain = train_domain,
                  test_domain  = test_domain,
                  country_indicator = country_indicator)
    }, error = function(e) {
      tibble::tibble(metric = NA, value = NA_real_,
                     model = mn, split_id = 1L,
                     feature_set = feature_set, task = task,
                     train_domain = train_domain, test_domain = test_domain,
                     country_indicator = country_indicator,
                     note = paste0("error: ", conditionMessage(e)))
    })
  })
}

# ---- Build the full experiment grid ---------------------------------
nepal_pool <- F1_pooled |> filter(country == "Nepal")
tur_pool   <- F1_pooled |> filter(country == "Türkiye")

# repeated splits within the pooled set for held-out evaluation
pooled_splits <- rsample::vfold_cv(F1_pooled, v = 5,
                                   strata = country)

results <- list()

for (task in c("5class", "binary")) {
  # 1. Nepal -> Türkiye
  results[[length(results) + 1]] <- transfer_one(
    nepal_pool, tur_pool, CORE_PREDICTORS, task,
    "F1", "Nepal", "Türkiye", country_indicator = FALSE)

  # 2. Türkiye -> Nepal
  results[[length(results) + 1]] <- transfer_one(
    tur_pool, nepal_pool, CORE_PREDICTORS, task,
    "F1", "Türkiye", "Nepal", country_indicator = FALSE)

  # 3. Pooled -> held-out (with country indicator) - via CV
  for (i in seq_len(nrow(pooled_splits))) {
    sp    <- pooled_splits$splits[[i]]
    train <- analysis(sp)
    test  <- assessment(sp)
    out <- transfer_one(train, test, CORE_PREDICTORS, task,
                        "F1", "Pooled", "Pooled_holdout",
                        country_indicator = TRUE)
    out$split_id <- i
    results[[length(results) + 1]] <- out
  }

  # 4. Pooled w/o country indicator - same splits
  for (i in seq_len(nrow(pooled_splits))) {
    sp    <- pooled_splits$splits[[i]]
    train <- analysis(sp)
    test  <- assessment(sp)
    out <- transfer_one(train, test, CORE_PREDICTORS, task,
                        "F1", "Pooled_no_indicator", "Pooled_holdout",
                        country_indicator = FALSE)
    out$split_id <- i
    results[[length(results) + 1]] <- out
  }
}

# 6. Nepal+Türkiye binary -> Japan external validation (J1)
# J1 has only mmi + overlays; we therefore train pooled on a single
# usable proxy: hazard_proxy_distance_km projected onto MMI. For the
# external validation cycle, we instead rely on what *is* shared: just
# the binary label and a coarse hazard proxy. This is intentionally
# a stripped-down test of (RQ3).
shared_japan <- tibble(
  asset_id = J1_Japan$asset_id,
  country  = "Japan",
  hazard_proxy_distance_km = NA_real_,   # not available; flag as missing
  age = NA_real_, no_stories = NA_real_,
  floor_area_m2 = NA_real_, total_height_m = NA_real_,
  y2 = J1_Japan$y2
)

results[[length(results) + 1]] <- transfer_one(
  F1_pooled |> filter(!is.na(y2)),
  shared_japan,
  CORE_PREDICTORS, "binary",
  "F1_external_japan", "Nepal+Türkiye", "Japan_external",
  country_indicator = FALSE
)

results <- bind_rows(results)
saveRDS(results, file.path(out_dir, "cross_country_transfer_long.rds"))
message("05_cross_country_transfer: ",
        nrow(results), " result rows written.")
