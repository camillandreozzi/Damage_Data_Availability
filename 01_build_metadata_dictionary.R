# =====================================================================
# 01_build_metadata_dictionary.R
# ---------------------------------------------------------------------
# Builds the variable metadata dictionary that underpins the
# compatibility matrix in the paper.
#
# One row per (country, raw_variable) tuple, with the columns:
#   country, raw_name, concept_family, unit_raw, unit_standard,
#   measurement_level, collection_protocol, candidate_harmonized_name,
#   compatibility_status, notes.
#
# concept_family ∈ {hazard_intensity, hazard_proxy, geometry, structure,
#                    site, damage_label, geo_id, collection_metadata}
# compatibility_status ∈ {direct, convertible, coarse_proxy_only,
#                          not_compatible}
#
# Outputs:
#   * R/outputs/metadata/variable_dictionary.csv
#   * R/outputs/metadata/compatibility_matrix.csv  (concept × country)
# =====================================================================

suppressPackageStartupMessages({
  library(tidyverse)
})

raw_dir <- "~/Documents/Claude/Projects/Damage Data Publication/R/outputs/raw"
out_dir <- "~/Documents/Claude/Projects/Damage Data Publication/R/outputs/metadata"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# ---- Load raw bundles to enumerate columns --------------------------
nepal   <- readRDS(file.path(raw_dir, "nepal_raw.rds"))
turkiye <- readRDS(file.path(raw_dir, "turkiye_raw.rds"))
japan   <- readRDS(file.path(raw_dir, "japan_raw.rds"))

# ---- Helper: skeleton dictionary rows -------------------------------
make_skeleton <- function(country, df) {
  tibble::tibble(
    country                  = country,
    raw_name                 = names(df),
    concept_family           = NA_character_,
    unit_raw                 = NA_character_,
    unit_standard            = NA_character_,
    measurement_level        = NA_character_,
    collection_protocol      = dplyr::case_when(
      country == "Nepal"   ~ "household_survey_post_reconstruction",
      country == "Türkiye" ~ "engineering_field_reconnaissance",
      country == "Japan"   ~ "remote_sensing_plus_validation"
    ),
    candidate_harmonized_name = NA_character_,
    compatibility_status     = NA_character_,
    notes                    = NA_character_
  )
}

dict_skeleton <- bind_rows(
  make_skeleton("Nepal",   nepal$data),
  make_skeleton("Türkiye", turkiye$data),
  make_skeleton("Japan",   janitor::clean_names(sf::st_drop_geometry(japan$data)))
)

# ---- Manual fill-in (hand-curated) ----------------------------------
# This block encodes the analyst's mapping decisions. It is the single
# source of truth for everything downstream. Treat as a pre-registration.
manual_overrides <- tribble(
  # --- Nepal -------------------------------------------------------------
  ~country,   ~raw_name,                                  ~concept_family,    ~unit_raw, ~unit_standard, ~measurement_level, ~candidate_harmonized_name,        ~compatibility_status, ~notes,
  "Nepal",    "age_building",                             "geometry",         "years",   "years",        "ratio",            "age",                              "direct",              "Year built; reconstruction-period inventory",
  "Nepal",    "count_floors_pre_eq",                      "geometry",         "count",   "count",        "ratio",            "no_stories",                       "direct",              "Pre-EQ floor count",
  "Nepal",    "plinth_area_sq_ft",                        "geometry",         "ft^2",    "m^2",          "ratio",            "floor_area_m2",                    "convertible",         "Convert ft^2 -> m^2 via 0.092903",
  "Nepal",    "per-height_ft_pre_eq",                     "geometry",         "ft",      "m",            "ratio",            "total_height_m",                   "convertible",         "Convert ft -> m via 0.3048",
  "Nepal",    "district_distance_to_earthquakecenter(mi)", "hazard_proxy",    "mi",      "km",           "ratio",            "hazard_proxy_distance_km",         "convertible",         "Convert mi -> km via 1.60934",
  "Nepal",    "damage_grade",                             "damage_label",     "1-5",     "1-5",          "ordinal",          "y5",                                "direct",              "EMS-98 ordinal",
  # --- Türkiye -----------------------------------------------------------
  "Türkiye",  "age",                                      "geometry",         "years",   "years",        "ratio",            "age",                              "direct",              "",
  "Türkiye",  "no_stories",                               "geometry",         "count",   "count",        "ratio",            "no_stories",                       "direct",              "",
  "Türkiye",  "floor_area",                               "geometry",         "m^2",     "m^2",          "ratio",            "floor_area_m2",                    "direct",              "",
  "Türkiye",  "total_height",                             "geometry",         "m",       "m",            "ratio",            "total_height_m",                   "direct",              "",
  "Türkiye",  "m7_8_repi",                                "hazard_proxy",     "km",      "km",           "ratio",            "hazard_proxy_distance_km",         "direct",              "Epicentral distance, Pazarcık main shock",
  "Türkiye",  "v_s30",                                    "site",             "m/s",     "m/s",          "ratio",            "v_s30",                            "not_compatible",      "Not present in Nepal/Japan",
  "Türkiye",  "structural_damage_5_class",                "damage_label",     "N/L/M/S/C", "1-5",        "ordinal",          "y5",                                "convertible",         "N->1, L->2, M->3, S->4, C->5",
  # --- Japan -------------------------------------------------------------
  "Japan",    "USGS_MMI",                                 "hazard_intensity", "MMI",     "MMI",          "interval",         "mmi",                              "not_compatible",      "Only macroseismic; no PGA/PGV/SA available",
  "Japan",    "GSI_tsunami",                              "hazard_intensity", "0/1",     "0/1",          "binary",           "tsunami_overlay",                  "not_compatible",      "Multi-hazard overlay specific to Noto",
  "Japan",    "GSI_fire",                                 "hazard_intensity", "0/1",     "0/1",          "binary",           "fire_overlay",                     "not_compatible",      "",
  "Japan",    "GSI_slope_failure",                        "hazard_intensity", "0/1",     "0/1",          "binary",           "slope_overlay",                    "not_compatible",      "",
  "Japan",    "damage_2",                                 "damage_label",     "0/1",     "0/1",          "binary",           "y2",                                "direct",              "Use as binary external-validation target"
)

# ---- Merge ----------------------------------------------------------
# ---- Fix Japan manual names to match cleaned skeleton names ----------

manual_overrides_fixed <- manual_overrides |>
  dplyr::mutate(
    raw_name = dplyr::if_else(
      country == "Japan",
      janitor::make_clean_names(raw_name),
      raw_name
    )
  )

# ---- Check which manual rows still do not match the skeleton ----------

missing_manual_keys <- manual_overrides_fixed |>
  dplyr::anti_join(
    dict_skeleton,
    by = c("country", "raw_name")
  )

print(missing_manual_keys)

if (nrow(missing_manual_keys) > 0) {
  stop("Some manual override rows do not match the raw dictionary keys.")
}

# ---- Merge ----------------------------------------------------------

dictionary <- dict_skeleton |>
  dplyr::rows_update(
    manual_overrides_fixed,
    by = c("country", "raw_name")
  ) |>
  dplyr::arrange(country, raw_name)

write_csv(dictionary, file.path(out_dir, "variable_dictionary.csv"))

# ---- Build compatibility matrix (concept × country) -----------------
# Each cell: best status across the variables in that family for that
# country (rank: direct > convertible > coarse_proxy_only > not_compatible).
status_rank <- c("direct" = 4, "convertible" = 3,
                 "coarse_proxy_only" = 2, "not_compatible" = 1)

compat_matrix <- dictionary |>
  filter(!is.na(candidate_harmonized_name)) |>
  mutate(rank = status_rank[compatibility_status]) |>
  group_by(candidate_harmonized_name, country) |>
  summarise(status = compatibility_status[which.max(rank)],
            .groups = "drop") |>
  pivot_wider(names_from = country, values_from = status,
              values_fill = "not_compatible")

write_csv(compat_matrix, file.path(out_dir, "compatibility_matrix.csv"))

message("01_build_metadata_dictionary: dictionary written with ",
        nrow(dictionary), " rows; compatibility matrix has ",
        nrow(compat_matrix), " conceptual variables.")
