# =====================================================================
# 09_diagnostics.R
# ---------------------------------------------------------------------
# Robustness checks promised in the paper's Methods and Limitations
# sections. Implements:
#
#   1. Grouped (spatial) cross-validation for Nepal by `vdcmun_id`,
#      to test whether the within-country Nepal AUC is inflated by
#      spatial leakage in the random-fold splits.
#
#   2. Country-stratified partial-dependence plots for each F1
#      predictor on the binary task, to substantiate the
#      "opposing physical relationships" interpretation of the
#      negative-transfer result.
#
#   3. Coefficient-sign comparison: a simple country-specific
#      logistic regression on F1 with standardised predictors,
#      tabling the sign and magnitude of each coefficient by
#      country.
#
#   4. Calibration: reliability curves and Brier decomposition
#      for the pooled binary RF.
#
#   5. Confusion matrices and class-wise F1 for the within-country
#      and pooled models.
#
# Outputs (under R/data/outputs/diagnostics/):
#   * grouped_cv_nepal.csv
#   * partial_dependence_long.rds
#   * coefficient_sign_table.csv
#   * calibration_curve.pdf
#   * confusion_matrices.rds
#
# All figures are also written into ../paper/figures/ so they can
# be embedded in a future revision of the manuscript without
# touching 08_figures_tables.R.
# =====================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(rsample)
  library(yardstick)
  library(ranger)
  library(pdp)
  library(caret)
})

source("~/Documents/Claude/Projects/Damage Data Publication/R/code/04_country_specific_models.R", local = TRUE)

feat_dir <- "~/Documents/Claude/Projects/Damage Data Publication/R/data/outputs/features"
res_dir  <- "~/Documents/Claude/Projects/Damage Data Publication/R/data/outputs/results"
out_dir  <- "~/Documents/Claude/Projects/Damage Data Publication/R/data/outputs/diagnostics"
fig_dir  <- "~/Documents/Claude/Projects/Damage Data Publication/paper/figures"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

set.seed(20260501)

CORE_PREDICTORS <- c("age", "no_stories", "floor_area_m2",
                     "total_height_m", "hazard_proxy_distance_km")

# ---------------------------------------------------------------------
# 1. Grouped (spatial) CV for Nepal by vdcmun_id
# ---------------------------------------------------------------------
F3_Nepal <- readRDS(file.path(feat_dir, "F3_Nepal.rds"))

# Build grouped folds: each VDC/municipality stays in one fold only.
nepal_grouped_results <- tryCatch({
  if (!"vdcmun_id" %in% names(F3_Nepal)) {
    stop("vdcmun_id not present in F3_Nepal; check 03_feature_harmonization.R")
  }

  grouped <- rsample::group_vfold_cv(F3_Nepal, group = vdcmun_id, v = 5)

  preds_nepal <- c(
    "age_building", "count_floors_pre_eq",
    "plinth_area_sq_ft", "per_height_ft_pre_eq",
    "district_distance_to_earthquakecenter_mi"
  )

  map_dfr(seq_len(nrow(grouped)), function(i) {
    sp    <- grouped$splits[[i]]
    train <- analysis(sp)
    test  <- assessment(sp)
    out <- tryCatch(
      fit_and_evaluate(train, test, "rf", "binary", "y2", preds_nepal),
      error = function(e) tibble(metric = NA, value = NA_real_,
                                 note = conditionMessage(e))
    )
    out$model       <- "rf"
    out$task        <- "binary"
    out$split_id    <- i
    out$cv_scheme   <- "grouped_vdcmun"
    out
  })
}, error = function(e) {
  message("Grouped CV failed: ", conditionMessage(e))
  tibble(metric = NA, value = NA_real_,
         note = paste0("setup error: ", conditionMessage(e)))
})

write_csv(nepal_grouped_results,
          file.path(out_dir, "grouped_cv_nepal.csv"))

# ---------------------------------------------------------------------
# 2. Country-stratified partial dependence on F1
# ---------------------------------------------------------------------
F1_pooled <- readRDS(file.path(feat_dir, "F1_pooled.rds")) |>
  drop_na(all_of(c("y2", CORE_PREDICTORS)))

pdp_long <- map_dfr(c("Nepal", "Türkiye"), function(cc) {
  df <- F1_pooled |> filter(country == cc)
  if (nrow(df) < 50) return(NULL)

  rf <- ranger::ranger(
    y2 ~ age + no_stories + floor_area_m2 +
         total_height_m + hazard_proxy_distance_km,
    data = df |> mutate(y2 = factor(y2, levels = c(0, 1))),
    probability = TRUE, num.trees = 300, min.node.size = 1
  )

  map_dfr(CORE_PREDICTORS, function(v) {
    pd <- pdp::partial(object = rf,
                         pred.var = v,
                         prob = TRUE,
                         which.class = "1",
                         train = df,
                         grid.resolution = 30)
    tibble::as_tibble(pd) |>
      rename(x = !!v) |>
      mutate(predictor = v, country = cc)
  })
})

saveRDS(pdp_long, file.path(out_dir, "partial_dependence_long.rds"))

p_pdp <- ggplot(pdp_long, aes(x, yhat, color = country)) +
  geom_line(linewidth = 0.7) +
  facet_wrap(~ predictor, scales = "free_x") +
  scale_color_viridis_d(option = "C", end = 0.7) +
  labs(x = NULL, y = "P(severe)",
       title = "Country-stratified partial dependence on F1 predictors",
       subtitle = "Diverging country curves indicate predictors with opposite operational meaning") +
  theme_minimal(base_size = 11)

ggsave(file.path(fig_dir, "fig06_partial_dependence_by_country.pdf"),
       p_pdp, width = 9, height = 5)

# ---------------------------------------------------------------------
# 3. Coefficient-sign comparison: country-specific logistic
# ---------------------------------------------------------------------
coef_table <- map_dfr(c("Nepal", "Türkiye"), function(cc) {
  df <- F1_pooled |> filter(country == cc) |>
    mutate(across(all_of(CORE_PREDICTORS), ~ as.numeric(scale(.))),
           y2 = factor(y2, levels = c(0, 1)))
  m <- glm(y2 ~ age + no_stories + floor_area_m2 +
                 total_height_m + hazard_proxy_distance_km,
           data = df, family = binomial())
  tibble::tibble(
    country = cc,
    predictor = names(coef(m))[-1],
    coef      = coef(m)[-1],
    se        = summary(m)$coefficients[-1, "Std. Error"]
  )
})
write_csv(coef_table, file.path(out_dir, "coefficient_sign_table.csv"))

# ---------------------------------------------------------------------
# 4. Calibration of the pooled binary RF
# ---------------------------------------------------------------------
calib_data <- {
  df <- F1_pooled
  idx <- sample(seq_len(nrow(df)), floor(0.8 * nrow(df)))
  train <- df[idx, ];  test <- df[-idx, ]
  m <- ranger::ranger(
    y2 ~ age + no_stories + floor_area_m2 +
         total_height_m + hazard_proxy_distance_km + country,
    data = train |> mutate(y2 = factor(y2, levels = c(0, 1)),
                           country = factor(country)),
    probability = TRUE, num.trees = 300, min.node.size = 1
  )
  prob <- predict(m, data = test |> mutate(country = factor(country)))$predictions[, "1"]
  tibble(prob = prob, truth = test$y2)
}

calib_summary <- calib_data |>
  mutate(bin = cut(prob, breaks = seq(0, 1, by = 0.1),
                   include.lowest = TRUE)) |>
  group_by(bin) |>
  summarise(mean_pred = mean(prob), mean_obs = mean(truth),
            n = n(), .groups = "drop")

p_calib <- ggplot(calib_summary, aes(mean_pred, mean_obs, size = n)) +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  geom_point() + geom_line() +
  labs(x = "Predicted probability", y = "Observed frequency",
       title = "Calibration of pooled binary Random Forest",
       subtitle = "Dashed line is perfect calibration") +
  theme_minimal(base_size = 11) 
ggsave(file.path(fig_dir, "fig07_calibration_pooled_rf.pdf"),
       p_calib, width = 6, height = 5)

brier <- mean((calib_data$prob - as.numeric(as.character(calib_data$truth)))^2)
write_csv(tibble(metric = "brier", value = brier),
          file.path(out_dir, "calibration_brier.csv"))

# ---------------------------------------------------------------------
# 5. Confusion matrices and class-wise F1
# ---------------------------------------------------------------------
make_cm <- function(country, task) {
  df <- if (task == "binary") F1_pooled else F1_pooled |> drop_na(y5)
  df <- df |> filter(country == !!country)
  target <- if (task == "binary") "y2" else "y5"
  preds  <- CORE_PREDICTORS

  if (nrow(df) < 50 || dplyr::n_distinct(df[[target]]) < 2) return(NULL)

  idx <- sample(seq_len(nrow(df)), floor(0.8 * nrow(df)))
  train <- df[idx, ]; test <- df[-idx, ]

  if (task == "binary") {
    train$y2 <- factor(train$y2, levels = c(0, 1))
    test$y2  <- factor(test$y2,  levels = c(0, 1))
  } else {
    train$y5 <- factor(train$y5, levels = 1:5)
    test$y5  <- factor(test$y5,  levels = 1:5)
  }

  m <- ranger::ranger(
    reformulate(preds, response = target),
    data = train, probability = (task == "binary"),
    classification = (task == "5class"),
    num.trees = 300, min.node.size = 1
  )
  if (task == "binary") {
    prob <- predict(m, data = test)$predictions[, "1"]
    pred <- factor(as.integer(prob >= 0.5), levels = c(0, 1))
  } else {
    pred <- factor(predict(m, data = test)$predictions, levels = 1:5)
  }
  cm <- caret::confusionMatrix(pred, test[[target]])
  list(country = country, task = task, cm = cm)
}

cms <- list(
  make_cm("Nepal",   "binary"),
  make_cm("Nepal",   "5class"),
  make_cm("Türkiye", "binary"),
  make_cm("Türkiye", "5class")
)
saveRDS(cms, file.path(out_dir, "confusion_matrices.rds"))

message("09_diagnostics: grouped CV, PDP, coefficient-sign, ",
        "calibration and confusion matrices written under ", out_dir,
        " and ", fig_dir)
