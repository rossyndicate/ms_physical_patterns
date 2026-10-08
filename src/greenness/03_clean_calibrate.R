# Format, clean, and cross-calibrate exported Landsat observations.
#
# Produces observation-level tables (one row per point x scene) for:
#   xcal   - L5 + L7 (<= l7_last_year) + L8/L9, NDVI and NIRv calibrated to the L7 scale (primary)
#   raw    - the same observations, uncalibrated (for the before/after comparison)
#   tmetm  - L5 + L7 only, through 2021 (TM/ETM+ sensitivity run; L5 calibrated to L7)
#
# Diagnostics from lsat_calibrate_rf() (cross-validation scatterplots, eval tables,
# fitted models) are written to figures/greenness/xcal_<si>_<variant>/.

source(here::here('src', 'greenness', '00_config.R'))
library(LandsatTS)

set.seed(seed_master)

export_dir <- file.path(gr_dir, 'export')
indices <- c('ndvi', 'nirv')

# read + format ####
files <- list.files(export_dir, pattern = '\\.csv$', full.names = TRUE)
if(!length(files)) stop('no exported CSVs in ', export_dir)

lsat_raw <- rbindlist(lapply(files, fread), fill = TRUE)
message(nrow(lsat_raw), ' rows from ', length(files), ' files')

# LandsatTS only knows LANDSAT_5/7/8. OLI-2 (L9) has the same band layout as OLI (L8),
# so relabel it; the original sensor is recoverable from LANDSAT_SCENE_ID ('LC9...').
lsat_raw <- lsat_raw[!is.na(SPACECRAFT_ID)] # drops the placeholder empty image added per point
if(l9_as_l8) lsat_raw[SPACECRAFT_ID == 'LANDSAT_9', SPACECRAFT_ID := 'LANDSAT_8']

lsat <- lsat_format_data(lsat_raw)
rm(lsat_raw); gc()

lsat[, sensor := substr(landsat.scene.id, 1, 3)] # LT5, LE7, LC8, LC9 (scene ids look like 'LC80140302015...')

# clean ####
n0 <- nrow(lsat)
lsat <- do.call(lsat_clean_data, c(list(dt = lsat), clean_args))
message('cleaning kept ', nrow(lsat), ' of ', n0, ' observations')

lsat <- lsat[year >= first_year & year <= last_year]

for(si in indices) lsat <- lsat_calc_spectral_index(lsat, si = si)

# the sample id encodes the watershed: <site_code>__<nnn>
lsat[, site_code := sub('__[0-9]+$', '', sample.id)]

# L8 vs L9 check (both uncalibrated, same points, overlapping years) ####
# If OLI-2 differs materially from OLI, relabeling L9 as L8 isn't safe.
l89 <- lsat[sensor %in% c('LC8', 'LC9') & year >= 2022 & doy %in% xcal_doy_rng,
            .(ndvi = median(ndvi), nirv = median(nirv), n = .N),
            by = .(sample.id, year, win = doy %/% 16, sensor)]
l89 <- dcast(l89, sample.id + year + win ~ sensor, value.var = c('ndvi', 'nirv'))
l89 <- na.omit(l89)
l89_summary <- l89[, .(n_pairs = .N,
                       ndvi_bias = median(ndvi_LC9 - ndvi_LC8),
                       ndvi_r = cor(ndvi_LC9, ndvi_LC8),
                       nirv_bias = median(nirv_LC9 - nirv_LC8),
                       nirv_r = cor(nirv_LC9, nirv_LC8))]
print(l89_summary)
fwrite(l89_summary, file.path(gr_dir, 'diag_l8_vs_l9.csv'))

# calibrate ####
calibrate_variant <- function(dt, variant){
    for(si in indices){
        outdir <- file.path(gr_fig_dir, paste0('xcal_', si, '_', variant))
        dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
        dt <- lsat_calibrate_rf(dt,
                                band.or.si = si,
                                doy.rng = xcal_doy_rng,
                                min.obs = xcal_min_obs,
                                frac.train = xcal_frac_train,
                                train.with.highlat.data = FALSE,
                                overwrite.col = FALSE,
                                write.output = TRUE,
                                outdir = outdir)
    }
    dt
}

keep_cols <- c('sample.id', 'site_code', 'latitude', 'longitude', 'satellite', 'sensor',
               'year', 'doy', indices, paste0(indices, '.xcal'))

## primary: L7 trimmed to pre-drift years ####
obs_main <- lsat[!(satellite == 'LANDSAT_7' & year > l7_last_year)]
obs_main <- calibrate_variant(obs_main, 'main')
obs_main <- obs_main[, intersect(keep_cols, names(obs_main)), with = FALSE]
saveRDS(obs_main, file.path(gr_dir, 'obs_main.rds'))

## TM/ETM+ only, 1984-2021 (L7 drift years retained by design; flagged in output) ####
obs_tmetm <- lsat[satellite %in% c('LANDSAT_5', 'LANDSAT_7') & year <= 2021]
obs_tmetm <- calibrate_variant(obs_tmetm, 'tmetm')
obs_tmetm <- obs_tmetm[, intersect(keep_cols, names(obs_tmetm)), with = FALSE]
saveRDS(obs_tmetm, file.path(gr_dir, 'obs_tmetm.rds'))

# observation density by sensor and year (LandsatTS summary figure) ####
data_summary <- lsat_summarize_data(obs_main)
ggsave(file.path(gr_fig_dir, 'obs_density_by_sensor.png'), last_plot(), width = 7, height = 4)
