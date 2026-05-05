# =====================================================================
# 03_feature_harmonization.R
# ---------------------------------------------------------------------
# Builds nested feature sets F1..F4 + J1 used by all subsequent
# modelling scripts.
#
# Conventions:
#   * F1 = minimal common core (Nepal/Türkiye)
#       age, no_stories, floor_area_m2, total_height_m,
#       hazard_proxy_distance_km
#   * F2 = F1 + coarse structural family (shared categorical)
#   * F3_<country> = country-specific full predictor set
#   * J1 = Japan reduced binary set (footprint geometry, MMI, overlays)
#
# Outputs:
#   * R/data/outputs/features/F1_pooled.rds          (Nepal+Türkiye, F1)
#   * R/data/outputs/features/F2_pooled.rds
#   * R/data/outputs/features/F3_Nepal.rds
#   * R/data/outputs/features/F3_Türkiye.rds
#   * R/data/outputs/features/J1_Japan.rds
# =====================================================================

suppressPackageStartupMessages({
  library(tidyverse)
})

raw_dir    <- "~/Documents/Claude/Projects/Damage Data Publication/R/data/outputs/raw"
target_dir <- "~/Documents/Claude/Projects/Damage Data Publication/R/data/outputs/targets"
out_dir    <- "~/Documents/Claude/Projects/Damage Data Publication/R/data/outputs/features"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

nepal   <- readRDS(file.path(raw_dir,"nepal_raw.rds"))$data
turkiye <- readRDS(file.path(raw_dir,"turkiye_raw.rds"))$data
japan   <- readRDS(file.path(raw_dir,"japan_raw.rds"))$data

nepal_y   <- readRDS(file.path(target_dir, "Nepal_targets.rds"))
turkiye_y <- readRDS(file.path(target_dir, "Türkiye_targets.rds"))
japan_y   <- readRDS(file.path(target_dir, "Japan_targets.rds"))

# ---- Unit conversion helpers ----------------------------------------
ft2_to_m2 <- function(x) x * 0.092903
ft_to_m   <- function(x) x * 0.3048
mi_to_km  <- function(x) x * 1.60934

# ---------------------------------------------------------------------
# F1: minimal common core for Nepal + Türkiye
# ---------------------------------------------------------------------
nepal <- nepal |>
  dplyr::mutate(
    asset_id = paste0("NPL_", dplyr::row_number())
  )
nepal_F1 <- nepal |>
  dplyr::transmute(
    asset_id                  = as.character(asset_id),
    country                   = "Nepal",
    age                       = age_building,
    no_stories                = count_floors_pre_eq,
    floor_area_m2             = ft2_to_m2(plinth_area_sq_ft),
    total_height_m            = ft_to_m(`per-height_ft_pre_eq`),
    hazard_proxy_distance_km  = mi_to_km(`district_distance_to_earthquakecenter(mi)`)
  )

turkiye_F1 <- turkiye |>
  dplyr::transmute(
    asset_id                  = as.character(id),
    country                   = "Türkiye",
    age                       = age,
    no_stories                = no_stories,
    floor_area_m2             = floor_area,
    total_height_m            = total_height,
    hazard_proxy_distance_km  = m7_8_repi
  )

F1_y <- dplyr::bind_rows(
  nepal_y   |> dplyr::mutate(asset_id = as.character(asset_id)),
  turkiye_y |> dplyr::mutate(asset_id = as.character(asset_id))
) |>
  dplyr::select(asset_id, country, y5, y2)

F1_pooled <- dplyr::bind_rows(nepal_F1, turkiye_F1) |>
  dplyr::inner_join(
    F1_y,
    by = c("asset_id", "country")
  )

saveRDS(F1_pooled, file.path(out_dir, "F1_pooled.rds"))

# ---------------------------------------------------------------------
# F2: F1 + coarse structural family
#   Nepal -> dominant superstructure category collapsed to:
#     "masonry_low", "masonry_high", "rc", "timber_bamboo", "other"
#   Türkiye -> all RC; encoded as "rc"
# ---------------------------------------------------------------------
collapse_nepal_structure <- function(df) {
  df |>
    mutate(
      structural_family = case_when(
        has_superstructure_rc_engineered == 1     ~ "rc",
        has_superstructure_rc_non_engineered == 1 ~ "rc",
        has_superstructure_cement_mortar_brick == 1 |
          has_superstructure_cement_mortar_stone == 1 ~ "masonry_high",
        has_superstructure_mud_mortar_stone == 1 |
          has_superstructure_mud_mortar_brick == 1 |
          has_superstructure_adobe_mud == 1       ~ "masonry_low",
        has_superstructure_timber == 1 |
          has_superstructure_bamboo == 1          ~ "timber_bamboo",
        TRUE                                      ~ "other"
      )
    )
}

nepal_struct <- nepal |>
  collapse_nepal_structure() |>
  transmute(asset_id = asset_id, country = "Nepal", structural_family)

turkiye_struct <- turkiye |>
  transmute(asset_id = id, country = "Türkiye", structural_family = "rc")

F2_pooled <- F1_pooled |>
  left_join(bind_rows(nepal_struct, turkiye_struct),
            by = c("asset_id", "country"))

saveRDS(F2_pooled, file.path(out_dir, "F2_pooled.rds"))

# ---------------------------------------------------------------------
# F3_<country>: country-specific full predictor sets
# ---------------------------------------------------------------------
F3_Nepal <- nepal |>
  janitor::clean_names() |>
  inner_join(nepal_y, by = c("asset_id" = "asset_id"))

F3_Nepal <- F3_Nepal |>
  dplyr::rename(
    roof_type = roof_type_bamboo_timber_heavy_roof_0_bamboo_timber_light_roof_1_rcc_rb_rbc_2
  ) |>
  dplyr::mutate(
    roof_type = dplyr::case_when(
      roof_type == 0 ~ "bamboo_timber_heavy_roof",
      roof_type == 1 ~ "bamboo_timber_light_roof",
      roof_type == 2 ~ "rcc_rb_rbc",
      TRUE ~ NA_character_
    )
  )

F3_Türkiye <- turkiye |>
  janitor::clean_names() |>
  inner_join(turkiye_y, by = c("id" = "asset_id"))

saveRDS(F3_Nepal,    file.path(out_dir, "F3_Nepal.rds"))
saveRDS(F3_Türkiye,  file.path(out_dir, "F3_Türkiye.rds"))

# ---------------------------------------------------------------------
# J1: Japan reduced binary set
# ---------------------------------------------------------------------
japan <- japan |>
  dplyr::mutate(
    asset_id = paste0("JPN_", dplyr::row_number())
  )

J1_Japan <- japan |>
  sf::st_drop_geometry() |>
  janitor::clean_names() |>
  transmute(
    asset_id        = asset_id,
    country         = "Japan",
    mmi             = usgs_mmi,
    tsunami_overlay = gsi_tsunami,
    fire_overlay    = gsi_fire,
    slope_overlay   = gsi_slope_failure
  ) |>
  dplyr::inner_join(
  japan_y |>
    dplyr::mutate(asset_id = as.character(asset_id)) |>
    dplyr::select(asset_id, y2),
  by = "asset_id",
  relationship = "one-to-one"
)

saveRDS(J1_Japan, file.path(out_dir, "J1_Japan.rds"))

message("03_feature_harmonization: F1/F2/F3/J1 all written under ",
        out_dir)
