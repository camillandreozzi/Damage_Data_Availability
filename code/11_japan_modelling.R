# =====================================================================
# 11_japan_modelling.R
# ---------------------------------------------------------------------
# Brings Japan into the modelling step rather than treating it only as
# a compatibility contrast. Two extensions:
#
#   (A) Japan within-country binary model on geometry-and-overlay-only
#       predictors. The Noto inventory does not carry building age,
#       number of storeys or total height, but it does carry footprint
#       geometry, USGS Modified Mercalli Intensity and three GSI binary
#       overlays (tsunami, fire, slope failure). Combined with a newly
#       computed asset-level distance to the 2024 Noto epicenter, this
#       gives Japan a defensible within-country baseline.
#
#   (B) Three-country distance-only model (F0).
#       F0 = {country, hazard_proxy_distance_km}: a maximally reductive
#       harmonisation that isolates whether even the simplest physical
#       relationship between distance-to-source and severe damage
#       transfers across the three countries. Within-country baselines
#       on F0, a pooled three-country F0 model, and the 3x3 transfer
#       matrix are written for the paper.
#
# Outputs (under R/data/outputs/):
#   * japan_within_country.csv         (RF + logistic on Japan)
#   * f0_within_country.csv            (RF on each country with F0)
#   * f0_pooled_three_country.csv      (pooled 3-country F0 RF)
#   * f0_transfer_matrix.csv           (3x3 train->test transfer)
#   * japan_F1_with_distance.rds       (Japan F1 frame with computed
#                                       distance, for downstream re-use)
#
# Path convention follows the layout documented in README.md
# (R/code/ for scripts, R/data/outputs/ for derived artefacts).
# Existing scripts that still write to R/outputs/ are left untouched.
# =====================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(sf)
  library(rsample)
  library(yardstick)
  library(ranger)
})


raw_dir   <- "~/Documents/Claude/Projects/Damage_Data_Availability/data/outputs/raw"
feat_dir  <- "~/Documents/Claude/Projects/Damage_Data_Availability/data/outputs/features"
target_dir<- "~/Documents/Claude/Projects/Damage_Data_Availability/data/outputs/targets"
out_dir   <- "~/Documents/Claude/Projects/Damage_Data_Availability/data/outputs"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

set.seed(20260701)

# ---------------------------------------------------------------------
# Epicentre coordinates (lon, lat)
# ---------------------------------------------------------------------
EPICENTRES <- list(
  Japan    = c(137.272, 37.498),   # 2024 Noto Peninsula M7.5 (USGS us6000m0xl)
  Nepal    = c(84.731,  28.231),   # 2015 Gorkha M7.8
  Turkiye  = c(37.014,  37.226)    # 2023 Pazarcık M7.81 (Kahramanmaraş sequence)
)

# ---------------------------------------------------------------------
# (A.0) Compute Japan asset-level distance to Noto epicentre
# ---------------------------------------------------------------------
japan_raw <- readRDS(file.path(raw_dir, "japan_raw.rds"))$data
japan_y   <- readRDS(file.path(target_dir, "Japan_targets.rds"))

# Re-derive the row-level synthetic asset_id used in 02/03
japan_sf <- japan_raw |>
  dplyr::mutate(
    asset_id = paste0("JPN_", dplyr::row_number())
  ) |>
  sf::st_as_sf(sf_column_name = "geom")

# Make sure CRS is defined
if (is.na(sf::st_crs(japan_sf))) {
  sf::st_crs(japan_sf) <- 4326
}

# ---------------------------------------------------------------------
# Fix invalid geometries before centroid / distance calculation
# ---------------------------------------------------------------------
old_s2 <- sf::sf_use_s2()
sf::sf_use_s2(FALSE)

japan_sf <- japan_sf |>
  dplyr::mutate(
    geom = sf::st_make_valid(geom)
  )

# Keep track of geometries that could not be repaired
empty_geom <- sf::st_is_empty(japan_sf)

# Representative point per polygon
# point_on_surface is safer than centroid for irregular polygons
japan_pts <- sf::st_point_on_surface(japan_sf)

sf::sf_use_s2(old_s2)

# ---------------------------------------------------------------------
# Epicentre point and distances
# ---------------------------------------------------------------------
noto <- sf::st_sfc(
  sf::st_point(EPICENTRES$Japan),
  crs = 4326
)

distances_m <- as.numeric(sf::st_distance(japan_pts, noto))

japan_sf$distance_to_epi_km <- distances_m / 1000

# Optional: set distance to NA for empty geometries
japan_sf$distance_to_epi_km[empty_geom] <- NA_real_

# Footprint area in m^2 (project to a metric CRS first; UTM 53N covers Noto)
japan_proj <- sf::st_transform(japan_sf, 32653)
japan_sf$footprint_area_m2 <- as.numeric(sf::st_area(japan_proj$geom))

japan_modelling_frame <- japan_sf |>
  sf::st_drop_geometry() |>
  janitor::clean_names() |>
  dplyr::transmute(
    asset_id            = asset_id,
    country             = "Japan",
    distance_to_epi_km  = distance_to_epi_km,
    footprint_area_m2   = footprint_area_m2,
    mmi                 = usgs_mmi,
    tsunami_overlay     = gsi_tsunami,
    fire_overlay        = gsi_fire,
    slope_overlay       = gsi_slope_failure
  ) |>
  dplyr::inner_join(japan_y |>
                      dplyr::mutate(asset_id = as.character(asset_id)) |>
                      dplyr::select(asset_id, y2),
                    by = "asset_id")

saveRDS(japan_modelling_frame,
        file.path(out_dir, "japan_F1_with_distance.rds"))

# ---------------------------------------------------------------------
# (A) Japan within-country binary model (geometry + overlays + distance)
# ---------------------------------------------------------------------
japan_predictors <- c("distance_to_epi_km", "footprint_area_m2", "mmi",
                      "tsunami_overlay", "fire_overlay", "slope_overlay")

japan_split <- rsample::vfold_cv(japan_modelling_frame |>
                                   drop_na(all_of(c("y2", japan_predictors))),
                                 v = 5, strata = y2)

japan_within <- map_dfr(seq_len(nrow(japan_split)), function(i) {
  sp    <- japan_split$splits[[i]]
  train <- analysis(sp);  test <- assessment(sp)
  bind_rows(
    fit_and_evaluate(train, test, "rf",       "binary", "y2", japan_predictors) |>
      mutate(model = "rf",       split_id = i),
    fit_and_evaluate(train, test, "logistic", "binary", "y2", japan_predictors) |>
      mutate(model = "logistic", split_id = i)
  )
})
japan_within$country <- "Japan"
write_csv(japan_within, file.path(out_dir, "japan_within_country.csv"))

# ---------------------------------------------------------------------
# (B) Three-country F0 (distance-only) feature set
# ---------------------------------------------------------------------
# Pull Nepal and Türkiye F1 frames (already in km), then attach the
# Japan distance computed above. Single shared continuous predictor.

F1_pooled <- readRDS(file.path(feat_dir, "F1_pooled.rds"))

F0_NPL_TUR <- F1_pooled |>
  dplyr::transmute(asset_id, country,
                   hazard_proxy_distance_km, y2) |>
  drop_na(hazard_proxy_distance_km, y2)

F0_JPN <- japan_modelling_frame |>
  dplyr::transmute(asset_id, country,
                   hazard_proxy_distance_km = distance_to_epi_km, y2) |>
  drop_na(hazard_proxy_distance_km, y2)

F0_pooled <- bind_rows(F0_NPL_TUR, F0_JPN)
saveRDS(F0_pooled, file.path(out_dir, "F0_pooled_three_country.rds"))

# ---------------------------------------------------------------------
# (B.1) Within-country F0 baselines (single-feature RF)
# ---------------------------------------------------------------------
within_F0 <- map_dfr(c("Nepal", "Türkiye", "Japan"), function(cc) {
  df <- F0_pooled |> filter(country == cc)
  if (nrow(df) < 50) return(NULL)
  sp <- rsample::vfold_cv(df, v = 5, strata = y2)
  map_dfr(seq_len(nrow(sp)), function(i) {
    s <- sp$splits[[i]]
    fit_and_evaluate(analysis(s), assessment(s),
                     "rf", "binary", "y2",
                     "hazard_proxy_distance_km") |>
      mutate(model = "rf", split_id = i, country = cc, feature_set = "F0")
  })
})
write_csv(within_F0, file.path(out_dir, "f0_within_country.csv"))

# ---------------------------------------------------------------------
# (B.2) Pooled three-country F0 model (with country indicator on/off)
# ---------------------------------------------------------------------
fit_pooled_F0 <- function(df, indicator = TRUE) {
  if (indicator) {
    df$country_f <- factor(df$country)
    preds <- c("hazard_proxy_distance_km", "country_f")
  } else {
    preds <- c("hazard_proxy_distance_km")
  }
  sp <- rsample::vfold_cv(df, v = 5, strata = country)
  map_dfr(seq_len(nrow(sp)), function(i) {
    s <- sp$splits[[i]]
    fit_and_evaluate(analysis(s), assessment(s),
                     "rf", "binary", "y2", preds) |>
      mutate(model = "rf", split_id = i,
             feature_set = "F0", indicator = indicator)
  })
}

pooled_F0 <- bind_rows(
  fit_pooled_F0(F0_pooled, indicator = TRUE)  |> mutate(country = "Pooled3"),
  fit_pooled_F0(F0_pooled, indicator = FALSE) |> mutate(country = "Pooled3_no_ind")
)
write_csv(pooled_F0, file.path(out_dir, "f0_pooled_three_country.csv"))

# ---------------------------------------------------------------------
# (B.3) Three-country transfer matrix on F0 (3x3, RF, binary AUC)
# ---------------------------------------------------------------------
countries <- c("Nepal", "Türkiye", "Japan")

transfer_F0 <- map_dfr(countries, function(tr) {
  map_dfr(countries, function(te) {
    train <- F0_pooled |> filter(country == tr)
    test  <- F0_pooled |> filter(country == te)
    if (tr == te) {
      idx <- sample(seq_len(nrow(train)), floor(0.8 * nrow(train)))
      out <- fit_and_evaluate(train[idx, ], train[-idx, ],
                              "rf", "binary", "y2",
                              "hazard_proxy_distance_km")
    } else {
      out <- fit_and_evaluate(train, test, "rf", "binary", "y2",
                              "hazard_proxy_distance_km")
    }
    out$train_country <- tr
    out$test_country  <- te
    out$feature_set   <- "F0"
    out$model         <- "rf"
    out
  })
})
write_csv(transfer_F0, file.path(out_dir, "f0_transfer_matrix.csv"))

message("11_japan_modelling: outputs written to ", out_dir,
        "; key files: japan_within_country.csv, ",
        "f0_pooled_three_country.csv, f0_transfer_matrix.csv")
