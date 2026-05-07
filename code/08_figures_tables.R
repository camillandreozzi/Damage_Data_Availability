# =====================================================================
# 08_figures_tables.R
# ---------------------------------------------------------------------
# Generates the paper's figures and tables from the long-format result
# files written by 04, 05, 06, and 07.
#
# Figures:
#   * fig01_compatibility_heatmap.pdf
#   * fig02_nested_feature_sets.pdf
#   * fig03_transfer_heatmap.pdf      (3-country F0 distance-only)
#   * fig04_overlap_ablation.pdf
#   * fig05a_sim_surface.pdf
#   * fig05b_variance_decomp.pdf
#   * fig06_japan_within_country.pdf  (RF vs logistic; full vs F0)
#   * fig07_distance_damage_curve.pdf (P(severe) vs distance, by country)
#   * fig08_pooled_vs_within_F0.pdf   (within-country F0 vs pooled 3-country F0)
# Note: 09_diagnostics.R and 10_subgroup_robustness.R also write to fig06/
# fig07/fig08 names. The current paper/main_v3.tex references this script's
# Japan figures under those numbers; if you re-introduce the partial-
# dependence and calibration plots, renumber them to fig09+ to avoid
# clobbering Japan outputs.
#
# Tables (CSV; LaTeX-ready stubs printed):
#   * tab_within_country.csv
#   * tab_transfer_matrix.csv         (3x3 F0 train->test AUC)
#   * tab_min_core_summary.csv
#   * tab_japan_within.csv            (Japan within-country full vs F0)
#   * tab_f0_pooled.csv               (pooled 3-country F0 with/without indicator)
# =====================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(scales)
  library(viridis)
})

res_dir <- "~/Documents/Claude/Projects/Damage_Data_Availability/data/outputs/results"
sim_dir <- "~/Documents/Claude/Projects/Damage_Data_Availability/data/outputs/sim"
meta_dir <- "~/Documents/Claude/Projects/Damage_Data_Availability/data/outputs/metadata"
jp_dir  <- "~/Documents/Claude/Projects/Damage_Data_Availability/data/outputs/japan"
fig_dir <- "~/Documents/Claude/Projects/Damage_Data_Availability/paper/figures"
tab_dir <- "~/Documents/Claude/Projects/Damage_Data_Availability/paper/tables"
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(tab_dir, recursive = TRUE, showWarnings = FALSE)

# Three-country palette used by Japan/F0 figures
country_levels <- c("Nepal", "Türkiye", "Japan")
country_palette <- c(Nepal = "#1f78b4", `Türkiye` = "#e31a1c", Japan = "#33a02c")

# =====================================================================
# Figure 1: compatibility heatmap
# =====================================================================
compat <- read_csv(file.path(meta_dir, "compatibility_matrix.csv"),
                   show_col_types = FALSE)
status_levels <- c("not_compatible", "coarse_proxy_only",
                   "convertible", "direct")
compat_long <- compat |>
  pivot_longer(-candidate_harmonized_name,
               names_to = "country", values_to = "status") |>
  mutate(status = factor(status, levels = status_levels))

p1 <- ggplot(compat_long,
             aes(country, candidate_harmonized_name, fill = status)) +
  geom_tile(color = "white") +
  scale_fill_viridis_d(option = "B", direction = -1, drop = FALSE) +
  theme_minimal(base_size = 11) +
  labs(x = NULL, y = NULL, fill = "Status",
       title = "Variable compatibility heatmap")

ggsave(file.path(fig_dir, "fig01_compatibility_heatmap.pdf"), p1,
       width = 7, height = 6)

# =====================================================================
# Figure 2: pooled and within-country feature sets
# =====================================================================
# F1 and F2 are pooled Nepal+Türkiye harmonised feature sets.
# F3_NPL and F3_TUR are the country-specific, within-country feature-rich
# specifications. These should not be labelled F4.

nested <- tibble(
  set = factor(
    c("F1", "F2", "F3_NPL", "F3_TUR"),
    levels = c("F1", "F2", "F3_NPL", "F3_TUR")
  ),
  n_predictors = c(5, 6, 22, 18),
  scope = c(
    "Pooled Nepal+Türkiye",
    "Pooled Nepal+Türkiye",
    "Nepal within-country",
    "Türkiye within-country"
  ),
  description = c(
    "Minimal shared core",
    "Expanded pooled harmonised set",
    "Nepal feature-rich within-country set",
    "Türkiye feature-rich within-country set"
  )
)

p2 <- ggplot(nested, aes(n_predictors, set, fill = scope)) +
  geom_col(width = 0.65) +
  geom_text(aes(label = n_predictors), hjust = -0.25, size = 3.5) +
  coord_cartesian(xlim = c(0, max(nested$n_predictors) + 3)) +
  scale_fill_viridis_d(option = "D") +
  theme_minimal(base_size = 11) +
  labs(
    x = "Number of predictors",
    y = "Feature set",
    fill = "Specification",
    title = "Pooled and within-country feature sets",
    #subtitle = "F1–F2 are pooled harmonised sets; F3_NPL and F3_TUR are within-country feature-rich specifications"
  )

ggsave(file.path(fig_dir, "fig02_nested_feature_sets.pdf"), p2,
       width = 7, height = 4)
# =====================================================================
# Figure 3: 3-country F0 (distance-only) transfer heatmap (binary AUC, RF)
# ---------------------------------------------------------------------
# Replaces the previous 2-country transfer heatmap. F0 = {country (held
# in train/test split), hazard_proxy_distance_km} is the most reductive
# specification we can fit on Nepal, Türkiye and Japan jointly, since
# Japan's Noto inventory does not carry building age, no_storeys, or
# total_height. The diagonal cells are the single 80/20 train-test split
# computed inside 11_japan_modelling.R, off-diagonal cells are pure
# train-on-A / test-on-B transfer.
# =====================================================================
tr_F0 <- read_csv(file.path(jp_dir, "f0_transfer_matrix.csv"),
                  show_col_types = FALSE) |>
  filter(model == "rf", metric == "auc") |>
  transmute(
    train_domain = factor(train_country, levels = country_levels),
    test_domain  = factor(test_country,  levels = country_levels),
    auc          = value
  )

p3 <- ggplot(tr_F0,
             aes(test_domain, train_domain, fill = auc)) +
  geom_tile(color = "white") +
  geom_text(aes(label = sprintf("%.2f", auc)),
            color = "white", fontface = "bold", size = 4.2) +
  scale_fill_viridis_c(option = "C", limits = c(0.4, 0.85),
                       name = "AUC") +
  theme_minimal(base_size = 11) +
  labs(
    x = "Test country",
    y = "Train country",
    title = "Cross-country transfer on F0 (distance-only, RF, binary AUC)",
    subtitle = "F0 = hazard_proxy_distance_km only (no overlays, no structural attributes)"
  )
ggsave(file.path(fig_dir, "fig03_transfer_heatmap.pdf"), p3,
       width = 7, height = 5)

# Companion CSV
write_csv(tr_F0, file.path(tab_dir, "tab_f0_transfer_matrix.csv"))

# =====================================================================
# Figure 4: overlap ablation curve
# =====================================================================
ab <- readRDS(file.path(res_dir, "overlap_ablation_long.rds"))
ab_long <- ab |>
  filter(metric %in% c("auc", "macro_f1")) |>
  group_by(task, metric, feature_set, predictors, model) |>
  summarise(value = mean(value, na.rm = TRUE), .groups = "drop")

p4 <- ggplot(ab_long,
             aes(predictors, value, color = model, group = model)) +
  geom_line() + geom_point() +
  facet_wrap(~ task + metric, scales = "free_y") +
  theme_minimal(base_size = 11) +
  labs(x = "Predictors retained (|K|)", y = "Score",
       title = "Performance vs. predictor overlap")
ggsave(file.path(fig_dir, "fig04_overlap_ablation.pdf"), p4,
       width = 8, height = 5)

# =====================================================================
# Figures 5a / 5b: simulation surfaces and variance decomposition
# =====================================================================
sim <- readRDS(file.path(sim_dir, "simulation_long.rds"))
sim_surf <- sim |>
  filter(metric == "auc", task == "binary") |>
  group_by(p_shared, sigma_measure) |>
  summarise(auc = mean(value, na.rm = TRUE), .groups = "drop")

p5a <- ggplot(sim_surf, aes(p_shared, sigma_measure, fill = auc)) +
  geom_tile() +
  geom_text(aes(label = sprintf("%.2f", auc)), color = "white") +
  scale_fill_viridis_c(option = "B", limits = c(0.5, 1)) +
  theme_minimal(base_size = 11) +
  labs(title = "Simulation: AUC by p_shared and sigma_measure")
ggsave(file.path(fig_dir, "fig05a_sim_surface.pdf"), p5a,
       width = 7, height = 5)

vd <- read_csv(file.path(sim_dir, "variance_decomp.csv"),
               show_col_types = FALSE) |>
  pivot_longer(starts_with("var_"), names_to = "source",
               values_to = "fraction") |>
  mutate(source = factor(source,
                         levels = c("var_resid", "var_model", "var_regime"),
                         labels = c("Residual", "Model family", "Compatibility regime")))
p5b <- ggplot(vd, aes(metric, fraction, fill = source)) +
  geom_col(position = "stack") +
  scale_fill_viridis_d(option = "D") +
  theme_minimal(base_size = 11) +
  labs(title = "Variance decomposition", y = "Fraction of variance")
ggsave(file.path(fig_dir, "fig05b_variance_decomp.pdf"), p5b,
       width = 7, height = 4)

# =====================================================================
# Tables (CSV companions; LaTeX-ready stubs printed below)
# =====================================================================
within <- readRDS(file.path(res_dir, "within_country_long.rds"))
write_csv(within |>
            group_by(train_domain, task, model, metric) |>
            summarise(mean_value = mean(value, na.rm = TRUE),
                      sd_value   = sd(value, na.rm = TRUE),
                      .groups = "drop"),
          file.path(tab_dir, "tab_within_country.csv"))

write_csv(tr_F0, file.path(tab_dir, "tab_transfer_matrix.csv"))

min_core <- tibble::tibble(
  family = c("Identifier + geolocation",
             "Hazard intensity",
             "Geometry",
             "Structural-system descriptor",
             "Site condition",
             "Damage label",
             "Collection-protocol metadata"),
  contents = c("asset_id, lat, lon, footprint polygon",
               "PGA, PGV, SA(T1=1s), MMI",
               "year built, no_storeys, total_height_m, floor_area_m2",
               "GED4ALL category",
               "v_s30 or NEHRP/Eurocode site class",
               "EMS-98 grade + binary severity flag",
               "survey_type, inspection_date, verification_status")
)
write_csv(min_core, file.path(tab_dir, "tab_min_core_summary.csv"))

# =====================================================================
# Figure 6: Japan within-country (RF vs logistic; full vs F0)
# ---------------------------------------------------------------------
# Compares Japan within-country binary AUC across:
#   * Full Japan predictor set (distance + footprint + MMI + 3 GSI
#     overlays), RF and logistic
#   * F0 distance-only, RF (logistic on F0 not run because it is the
#     univariate degenerate case)
# Logistic on the full set collapses (sensitivity ~ 0) because the class
# imbalance and lack of nonlinearity wash out the categorical overlays;
# the bar chart makes the gap to RF immediately visible.
# =====================================================================
jp_full <- read_csv(file.path(jp_dir, "japan_within_country.csv"),
                    show_col_types = FALSE) |>
  filter(metric == "auc") |>
  mutate(spec = "Full (dist + footprint + MMI + overlays)")

jp_f0 <- read_csv(file.path(jp_dir, "f0_within_country.csv"),
                  show_col_types = FALSE) |>
  filter(country == "Japan", metric == "auc") |>
  mutate(spec = "F0 (distance only)")

jp_bar <- bind_rows(jp_full, jp_f0) |>
  group_by(model, spec) |>
  summarise(auc_mean = mean(value, na.rm = TRUE),
            auc_sd   = sd(value, na.rm = TRUE),
            .groups = "drop") |>
  mutate(spec = factor(spec,
                       levels = c("F0 (distance only)",
                                  "Full (dist + footprint + MMI + overlays)")),
         model = factor(model, levels = c("logistic", "rf"),
                        labels = c("Logistic", "Random Forest")))

p6 <- ggplot(jp_bar, aes(spec, auc_mean, fill = model)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_errorbar(aes(ymin = pmax(auc_mean - auc_sd, 0),
                    ymax = pmin(auc_mean + auc_sd, 1)),
                position = position_dodge(width = 0.7),
                width = 0.15, color = "grey40") +
  geom_text(aes(label = sprintf("%.2f", auc_mean)),
            position = position_dodge(width = 0.7),
            vjust = -0.6, size = 3.4) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "grey50") +
  scale_fill_viridis_d(option = "D", end = 0.75) +
  coord_cartesian(ylim = c(0.4, 0.95)) +
  theme_minimal(base_size = 11) +
  labs(x = NULL, y = "Binary AUC (mean over 5-fold CV)",
       fill = "Model",
       title = "Japan within-country: model x specification",
       subtitle = "Bars: mean binary AUC; whiskers: ±1 SD across folds. Dashed line = chance.")

ggsave(file.path(fig_dir, "fig06_japan_within_country.pdf"), p6,
       width = 7.5, height = 4.5)

write_csv(jp_bar, file.path(tab_dir, "tab_japan_within.csv"))

# =====================================================================
# Figure 7: distance-damage curve per country on F0
# ---------------------------------------------------------------------
# Visualises the marginal P(severe damage | distance) for each of the
# three countries on the shared F0 specification. Bins distance into
# adaptive quantile bins per country and shows the empirical positive
# rate. This is the picture that explains *why* F0 transfer fails
# (different countries have very different distance-damage shapes even
# at the same epicentral distance).
# =====================================================================
F0_pooled <- readRDS(file.path(jp_dir, "F0_pooled_three_country.rds")) |>
  mutate(country = factor(country, levels = country_levels))

# Use 12 quantile bins per country, shared visualization
n_bins <- 12
F0_binned <- F0_pooled |>
  dplyr::filter(!is.na(hazard_proxy_distance_km), !is.na(y2)) |>
  dplyr::group_by(country) |>
  dplyr::mutate(
    dist_bin = dplyr::ntile(hazard_proxy_distance_km, n_bins)
  ) |>
  dplyr::group_by(country, dist_bin) |>
  dplyr::summarise(
    dist_mid = mean(hazard_proxy_distance_km, na.rm = TRUE),
    p_severe = mean(y2 == 1, na.rm = TRUE),
    n        = dplyr::n(),
    .groups  = "drop"
  ) |>
  dplyr::filter(!is.na(dist_mid), n >= 20)

p7 <- ggplot(F0_binned,
             aes(dist_mid, p_severe, color = country, group = country)) +
  geom_line(linewidth = 0.9) +
  geom_point(aes(size = n), alpha = 0.8) +
  scale_color_manual(values = country_palette, drop = FALSE) +
  scale_size_continuous(range = c(1.5, 5), guide = "none") +
  scale_x_continuous(trans = "log10",
                     labels = scales::label_number(accuracy = 1)) +
  theme_minimal(base_size = 11) +
  labs(
    x = "Distance to epicentre (km, log scale)",
    y = "Empirical P(severe damage)",
    color = "Country",
    title = "Distance-damage curves on F0",
    subtitle = "Quantile-binned positive rates; point size = bin count"
  )

ggsave(file.path(fig_dir, "fig07_distance_damage_curve.pdf"), p7,
       width = 7.5, height = 4.5)

# =====================================================================
# Figure 8: pooled-3-country F0 vs within-country F0
# ---------------------------------------------------------------------
# Two panels:
#   (a) Within-country F0 RF AUC for Nepal, Türkiye, Japan
#   (b) Pooled F0 with vs without country indicator
# Together, this shows whether pooling buys anything beyond the best
# within-country baseline.
# =====================================================================
within_F0 <- read_csv(file.path(jp_dir, "f0_within_country.csv"),
                      show_col_types = FALSE) |>
  filter(metric == "auc") |>
  group_by(country) |>
  summarise(auc_mean = mean(value),
            auc_sd   = sd(value),
            .groups = "drop") |>
  mutate(spec = "Within-country F0",
         country = factor(country, levels = country_levels))

pooled_F0 <- read_csv(file.path(jp_dir, "f0_pooled_three_country.csv"),
                      show_col_types = FALSE) |>
  filter(metric == "auc") |>
  group_by(country) |>
  summarise(auc_mean = mean(value),
            auc_sd   = sd(value),
            .groups = "drop") |>
  mutate(spec = "Pooled F0",
         country = recode(country,
                          Pooled3        = "Pooled3 (with indicator)",
                          Pooled3_no_ind = "Pooled3 (no indicator)"),
         country = factor(country, levels = c("Pooled3 (with indicator)",
                                              "Pooled3 (no indicator)")))

p8 <- bind_rows(
  within_F0 |> mutate(panel = "(a) Within-country F0"),
  pooled_F0 |> mutate(panel = "(b) Pooled three-country F0")
) |>
  ggplot(aes(reorder(country, auc_mean), auc_mean, fill = panel)) +
  geom_col(width = 0.65) +
  geom_errorbar(aes(ymin = pmax(auc_mean - auc_sd, 0),
                    ymax = pmin(auc_mean + auc_sd, 1)),
                width = 0.15, color = "grey40") +
  geom_text(aes(label = sprintf("%.2f", auc_mean)),
            vjust = -0.6, size = 3.4) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "grey50") +
  facet_wrap(~ panel, scales = "free_x") +
  scale_fill_viridis_d(option = "D", end = 0.75, guide = "none") +
  coord_cartesian(ylim = c(0.45, 0.85)) +
  theme_minimal(base_size = 11) +
  labs(x = NULL, y = "Binary AUC (5-fold CV mean ± 1 SD)",
       title = "F0: within-country baselines vs pooled three-country model")

ggsave(file.path(fig_dir, "fig08_pooled_vs_within_F0.pdf"), p8,
       width = 9, height = 4.5)

write_csv(bind_rows(within_F0, pooled_F0),
          file.path(tab_dir, "tab_f0_pooled.csv"))

message("08_figures_tables: figures written under ", fig_dir,
        ", tables written under ", tab_dir)
