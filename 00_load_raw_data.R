# =====================================================================
# 00_load_raw_data.R
# ---------------------------------------------------------------------
# Loads raw country datasets into memory and saves canonical raw copies
# under R/outputs/raw/.
#
# Inputs (configure paths below):
#   * Nepal building-damage CSV     (post-Gorkha reconstruction inventory)
#   * Türkiye reconnaissance CSV    (ACI 133 RC building survey)
#   * Japan Noto-peninsula GeoJSON  (Vescovo et al. 2025 ESSD)
#
# Outputs:
#   * R/outputs/raw/nepal_raw.rds
#   * R/outputs/raw/turkiye_raw.rds
#   * R/outputs/raw/japan_raw.rds
#
# Notes:
#   - This script does NOT clean or harmonise.
#   - It records source-file checksums so downstream scripts can detect
#     stale inputs.
# =====================================================================

# ---- Packages -------------------------------------------------------
suppressPackageStartupMessages({
  library(tidyverse)
  library(janitor)
  library(sf)        # for Japan polygons
  library(digest)
  library(readxl)# for source checksums
})

# ---- Config ---------------------------------------------------------
# TODO: replace with your local paths or read from a config.yaml.
cfg <- list(
  paths = list(
    nepal   = "~/Documents/Claude/Projects/Damage Data Publication/R/inputs/Nepal.xlsx",
    turkiye = "~/Documents/Claude/Projects/Damage Data Publication/R/inputs/turkey.csv",
    japan   = "~/Documents/Claude/Projects/Damage Data Publication/R/inputs/japan.gpkg"
  ),
  out_dir = "~/Documents/Claude/Projects/Damage Data Publication/R/outputs/raw"
)

dir.create(cfg$out_dir, recursive = TRUE, showWarnings = FALSE)

# ---- Helpers --------------------------------------------------------
read_with_log <- function(path, reader = read_csv, ...) {
  if (!file.exists(path)) {
    stop("Missing input file: ", path)
  }
  obj <- reader(path, ...)
  list(
    data    = obj,
    n_rows  = nrow(obj),
    n_cols  = ncol(obj),
    sha     = digest::digest(file = path, algo = "sha1"),
    path    = path,
    loaded  = Sys.time()
  )
}

# ---- Nepal ----------------------------------------------------------
nepal <- read_with_log(cfg$paths$nepal, read_excel,
                       guess_max = 50000) |>
  modifyList(list(country = "Nepal"))

# ---- Türkiye --------------------------------------------------------
turkiye <- read_with_log(cfg$paths$turkiye, read_csv,
                         show_col_types = FALSE) |>
  modifyList(list(country = "Türkiye"))

# ---- Japan ----------------------------------------------------------
japan_sf <- sf::st_read(cfg$paths$japan, quiet = TRUE)
japan <- list(
  data    = japan_sf,
  n_rows  = nrow(japan_sf),
  n_cols  = ncol(japan_sf),
  sha     = digest::digest(file = cfg$paths$japan, algo = "sha1"),
  path    = cfg$paths$japan,
  loaded  = Sys.time(),
  country = "Japan"
)

# ---- Save -----------------------------------------------------------
saveRDS(nepal,   file.path(cfg$out_dir, "nepal_raw.rds"))
saveRDS(turkiye, file.path(cfg$out_dir, "turkiye_raw.rds"))
saveRDS(japan,   file.path(cfg$out_dir, "japan_raw.rds"))

message("00_load_raw_data: loaded Nepal (",   nepal$n_rows,   " rows), ",
        "Türkiye (", turkiye$n_rows, " rows), ",
        "Japan (",   japan$n_rows,   " rows).")
