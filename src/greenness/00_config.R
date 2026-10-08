# Shared settings for the calibrated Landsat greenness pipeline.
# This pipeline replaces Robinson et al. (2018) Landsat GPP (gpp_CONUS_30m_median)
# with growing-season NDVI/NIRv built from Collection 2 surface reflectance and
# cross-calibrated among sensors with LandsatTS (Berner et al. 2023).
# See src/greenness/README.md for run order and rationale.

library(here)
library(sf)
library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)

gr_dir <- Sys.getenv('GR_DIR', here('data_working', 'greenness'))
gr_fig_dir <- Sys.getenv('GR_FIG_DIR', here('figures', 'greenness'))
dir.create(gr_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(gr_fig_dir, showWarnings = FALSE, recursive = TRUE)

ms_root <- here('data_raw', 'ms')

# sampling ####
seed_master <- 20261006
pts_per_ws <- 30        # random points per watershed (all pixel centers if fewer exist)
min_pt_spacing_m <- 45  # no two points closer than ~1.5 Landsat pixels

# export ####
gee_drive_dir <- 'ms_greenness_export'
export_start_date <- '1984-01-01'
export_end_date <- '2025-12-31'
export_doy <- c(1, 366)  # whole year; Mediterranean sites (e.g. santa_barbara) green up in winter
export_chunk_size <- 250 # LandsatTS recommendation

# cleaning (LandsatTS defaults) ####
clean_args <- list(geom.max = 15, cloud.max = 80, sza.max = 60,
                   filter.cfmask.snow = TRUE, filter.cfmask.water = TRUE,
                   filter.jrc.water = TRUE)

# sensors ####
# Landsat 7 drifted to earlier overpass times from ~2017 onward (and was lowered in 2022).
# Post-drift L7 observations are excluded from both calibration training and the final series.
l7_last_year <- 2017
# Landsat 9 (OLI-2) is treated as Landsat 8 (OLI); LandsatTS doesn't recognize LANDSAT_9.
l9_as_l8 <- TRUE

# calibration ####
xcal_doy_rng <- 121:273  # May-Sep; wider than the LandsatTS (Arctic) default of 152:243
xcal_min_obs <- 5
xcal_frac_train <- 0.75

# phenology / growing season ####
pheno_window_yrs <- 5
pheno_window_min_obs <- 10
pheno_si_min <- c(ndvi = 0.15, nirv = 0.02) # LandsatTS default (0.15) would discard most NIRv obs
gs_min_frac_of_max <- 0.75
min_obs_per_pt_year <- 2 # growing-season obs needed for a point-year value

# analysis period (Robinson GPP began in 1986; LandsatTS export starts 1984) ####
first_year <- 1984
last_year <- 2025
min_frac_pts_per_year <- 0.5 # watershed-year needs this fraction of points with a valid value

# helpers ####
water_year <- function(date){
    ifelse(lubridate::month(date) >= 10, lubridate::year(date) + 1, lubridate::year(date))
}
