# =====================================================================
# 08_figures_tables.R
# ---------------------------------------------------------------------
# Generates the paper's figures and tables from the long-format result
# files written by 04, 05, 06, and 07.
#
# Figures:
#   * fig01_compatibility_heatmap.pdf
#   * fig02_nested_feature_sets.pdf
#   * fig03_transfer_heatmap.pdf
#   * fig04_overlap_ablation.pdf
#   * fig05a_sim_surface.pdf
#   * fig05b_variance_decomp.pdf
#
# Tables (CSV; LaTeX-ready stubs printed):
#   * tab_within_country.csv
#   * tab_transfer_matrix.csv
#   * tab_min_core_summary.csv
# =====================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(scales)
  library(viridis)
})

res_dir <- "~/Documents/Claude/Projects/Damage Data Publication/R/data/outputs/results"
sim_dir <- "~/Documents/Claude/Projects/Damage Data Publication/R/data/outputs/sim"
meta_dir <- "~/Documents/Claude/Projects/Damage Data Publication/R/data/outputs/metadata"
fig_dir <- "~/Documents/Claude/Projects/Damage Data Publication/paper/figures"
tab_dir <- "~/Documents/Claude/Projects/Damage Data Publication/paper/tables"
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(tab_dir, recursive = TRUE, showWarnings = FALSE)

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
# Figure 2: nested feature sets (illustrative)
# =====================================================================
nested <- tibble(
  set = factor(c("F1", "F2", "F3", "F4_NPL", "F4_TUR", "J1"),
               levels = c("J1", "F1", "F2", "F3", "F4_NPL", "F4_TUR")),
  n_predictors = c(5, 6, 8, 22, 18, 5),
  scope        = c("Nepal+TUR", "Nepal+TUR", "Nepal+TUR",
                   "Nepal", "TUR", "Japan")
)
p2 <- ggplot(nested, aes(n_predictors, set, fill = scope)) +
  geom_col() +
  geom_text(aes(label = n_predictors), hjust = -0.2, size = 3.5) +
  scale_fill_viridis_d(option = "D") +
  theme_minimal(base_size = 11) +
  labs(x = "Number of predictors", y = "Feature set",
       title = "Nested harmonised feature sets")
ggsave(file.path(fig_dir, "fig02_nested_feature_sets.pdf"), p2,
       width = 7, height = 4)

# =====================================================================
# Figure 3: train -> test transfer heatmap (binary AUC, RF)
# =====================================================================
# Pull both off-diagonal transfer cells (cross_country_transfer_long.rds)
# AND within-country diagonal cells (within_country_long.rds) so the
# heatmap shows Nepal->Nepal and Türkiye->Türkiye too.
tr_cross <- readRDS(file.path(res_dir, "cross_country_transfer_long.rds")) |>
  filter(model == "rf", task == "binary", metric == "auc")

tr_within <- readRDS(file.path(res_dir, "within_country_long.rds")) |>
  filter(model == "rf", task == "binary", metric == "auc") |>
  mutate(train_domain = test_domain)   # diagonal cell

tr <- bind_rows(tr_cross, tr_within)
tr_summary <- tr |>
  group_by(train_domain, test_domain) |>
  summarise(auc = mean(value, na.rm = TRUE), .groups = "drop") |>
  filter(!is.nan(auc))

p3 <- ggplot(tr_summary,
             aes(test_domain, train_domain, fill = auc)) +
  geom_tile(color = "white") +
  geom_text(aes(label = sprintf("%.2f", auc)), color = "white") +
  scale_fill_viridis_c(option = "C", limits = c(0.5, 1)) +
  theme_minimal(base_size = 11) +
  labs(x = "Test domain", y = "Train domain",
       title = "Cross-country transfer (Random Forest, binary AUC)")
ggsave(file.path(fig_dir, "fig03_transfer_heatmap.pdf"), p3,
       width = 7, height = 5)

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

write_csv(tr_summary, file.path(tab_dir, "tab_transfer_matrix.csv"))

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

message("08_figures_tables: figures written under ", fig_dir,
        ", tables written under ", tab_dir)
