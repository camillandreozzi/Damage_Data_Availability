# =====================================================================
# 02_target_harmonization.R
# ---------------------------------------------------------------------
# Builds harmonised damage targets for each country and writes a label
# audit table.
#
# Outputs per country: a tibble with at least
#   c(asset_id, country, y5, y2)
# where y5 ∈ {1..5} (NA where not defined) and y2 ∈ {0,1}.
#
# Outputs:
#   * R/data/outputs/targets/<country>_targets.rds
#   * R/data/outputs/targets/label_audit.csv
# =====================================================================

suppressPackageStartupMessages({
  library(tidyverse)
})

raw_dir <- "~/Documents/Claude/Projects/Damage Data Publication/R/data/outputs/raw"
out_dir <- "~/Documents/Claude/Projects/Damage Data Publication/R/data/outputs/targets"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

nepal   <- readRDS(file.path(raw_dir, "nepal_raw.rds"))$data
turkiye <- readRDS(file.path(raw_dir, "turkiye_raw.rds"))$data
japan   <- readRDS(file.path(raw_dir, "japan_raw.rds"))$data

# ---- Helper ---------------------------------------------------------
collapse_to_binary <- function(y5) {
  dplyr::case_when(
    y5 %in% c(1, 2, 3) ~ 0L,
    y5 %in% c(4, 5)    ~ 1L,
    TRUE               ~ NA_integer_
  )
}

# ---- Nepal ----------------------------------------------------------
# Nepal ward_id is not unique, so create a unique row-level asset ID
nepal <- nepal |>
  dplyr::mutate(
    asset_id = paste0("NPL_", dplyr::row_number())
  )

nepal_targets <- nepal |>
  transmute(
    asset_id = asset_id,                
    country  = "Nepal",
    y5       = as.integer(damage_grade),
    y2       = collapse_to_binary(y5)
  )

# ---- Türkiye --------------------------------------------------------
tur_map <- c(N = 1L, L = 2L, M = 3L, S = 4L, C = 5L)
turkiye_targets <- turkiye |>
  transmute(
    asset_id = id,
    country  = "Türkiye",
    y5       = unname(tur_map[as.character(structural_damage_5_class)]),
    y2       = collapse_to_binary(y5)
  )

# ---- Japan (binary only) --------------------------------------------
# `damage_2` in the Vescovo et al. release encodes survived/destroyed +
# obstructed/missing. Treat 0 as not-severe, positive damage codes as
# severe; obstructed/missing dropped.
japan <- japan |>
  dplyr::mutate(
    asset_id = paste0("JPN_", dplyr::row_number())
  )

japan_targets <- japan |>
  sf::st_drop_geometry() |>
  dplyr::transmute(
    asset_id = asset_id,
    country  = "Japan",
    y5       = NA_integer_,
    y2       = dplyr::case_when(
      damage_2 == 0 ~ 0L,
      damage_2 %in% c(9, 99, 1) ~ 1L,
      TRUE ~ NA_integer_
    )
  )

# ---- Save -----------------------------------------------------------
saveRDS(nepal_targets,   file.path(out_dir, "Nepal_targets.rds"))
saveRDS(turkiye_targets, file.path(out_dir, "Türkiye_targets.rds"))
saveRDS(japan_targets,   file.path(out_dir, "Japan_targets.rds"))

# ---- Label audit ----------------------------------------------------
targets_all <- dplyr::bind_rows(
  nepal_targets   |> dplyr::mutate(asset_id = as.character(asset_id)),
  turkiye_targets |> dplyr::mutate(asset_id = as.character(asset_id)),
  japan_targets   |> dplyr::mutate(asset_id = as.character(asset_id))
)

audit <- targets_all |>
  dplyr::group_by(country) |>
  dplyr::summarise(
    n_total          = dplyr::n(),
    n_y5_missing     = sum(is.na(y5)),
    prop_y5_missing  = mean(is.na(y5)),
    n_y2_missing     = sum(is.na(y2)),
    prop_y2_missing  = mean(is.na(y2)),
    prop_severe_y2   = ifelse(
      all(is.na(y2)),
      NA_real_,
      mean(y2 == 1, na.rm = TRUE)
    ),
    .groups = "drop"
  )

print(audit)
write_csv(audit, file.path(out_dir, "label_audit.csv"))

# ---- Per-country class distribution table ---------------------------
class_dist <- targets_all |>
  count(country, y5, y2) |>
  arrange(country, y5, y2)
write_csv(class_dist, file.path(out_dir, "label_class_distribution.csv"))

message("02_target_harmonization: targets saved for Nepal/Türkiye/Japan; ",
        "audit written to ", file.path(out_dir, "label_audit.csv"))

