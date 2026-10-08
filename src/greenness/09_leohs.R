# Alternative Landsat 8/9 -> Landsat 7 calibration using LEOHS linear band
# harmonization (Richardson et al. 2025, Geocarto International,
# doi:10.1080/10106049.2025.2538108), for comparison with the LandsatTS random forest.
#
# LEOHS fits per-band linear regressions to Landsat 7 / Landsat 8 pixel pairs imaged
# one day apart where adjacent orbit paths overlap. We apply its "L8toL7" surface
# reflectance equations to the red and NIR bands of every Landsat 8 and 9 observation,
# then recompute NDVI and NIRv. Landsat 7 is unchanged; Landsat 5 keeps its LandsatTS
# calibration (LEOHS doesn't cover TM), so only the OLI side differs from the main series.
#
# Red and NIR are recovered exactly from the stored indices (see leohs_apply()).
#
# Usage: Rscript src/greenness/09_leohs.R <coefficient set>   (a `set` in leohs_coefficients.csv)
# Then the comparison section runs over every set fitted so far.

source(here::here('src', 'greenness', '00_config.R'))
library(LandsatTS)
source(here::here('src', 'greenness', 'functions.R'))

coef_file <- here('src', 'greenness', 'leohs_coefficients.csv')
coefs <- read.csv(coef_file)

coef_set <- commandArgs(trailingOnly = TRUE)[1]

# harmonize and summarize ####
if(!is.na(coef_set)){

    cf <- coefs[coefs$set == coef_set, ]
    if(!nrow(cf)) stop('no coefficients for set ', coef_set)
    obs <- readRDS(file.path(gr_dir, 'obs_main.rds'))
    leohs_apply(obs, cf)

    out <- list()
    for(si in c('ndvi', 'nirv')){
        message('fitting ', si, ' / leohs_', coef_set)
        pt <- point_gs(obs, si, paste0(si, '.leohs'))
        varname <- paste0(si, '_gs_leohs_', coef_set)
        out[[varname]] <- ws_aggregate(pt, 'gs.med')[, var := varname]
    }

    res <- rbindlist(out) %>% as_tibble() %>% select(site_code, water_year, var, val, n_pts)
    saveRDS(res, file.path(gr_dir, paste0('greenness_annual_leohs_', coef_set, '.rds')))

    # observation-level overlap check: summer L8 minus L7, 2013-2017, per site then median
    ov <- obs[year %in% 2013:2017 & doy %in% 152:243 & sensor %in% c('LE7', 'LC8'),
              .(raw = median(ndvi), xcal = median(ndvi.xcal), leohs = median(ndvi.leohs, na.rm = TRUE)),
              by = .(site_code, sensor)]
    ov <- dcast(ov, site_code ~ sensor, value.var = c('raw', 'xcal', 'leohs'))
    print(ov[, .(raw = median(raw_LC8 - raw_LE7, na.rm = TRUE),
                 landsatts_rf = median(xcal_LC8 - xcal_LE7, na.rm = TRUE),
                 leohs = median(leohs_LC8 - leohs_LE7, na.rm = TRUE))])
}

# compare all calibrations ####
suppressMessages(source(here::here('src', 'setup.R'))) # detect_trends(), add_flags()

ga <- readRDS(file.path(gr_dir, 'greenness_annual.rds')) %>%
    filter(var %in% c('ndvi_gs_raw', 'ndvi_gs_xcal', 'ndvi_gs_tmetm', 'nirv_gs_xcal', 'nirv_gs_tmetm'))
leohs_files <- list.files(gr_dir, '^greenness_annual_leohs_.*\\.rds$', full.names = TRUE)
ga <- bind_rows(ga, lapply(leohs_files, readRDS)) %>% select(-n_pts)
sites <- unique(ga$site_code)

veg <- feather::read_feather(here('data_raw', 'ms', 'v2', 'spatial_timeseries_vegetation.feather')) %>%
    filter(site_code %in% sites)
ndvi_modis <- veg %>%
    filter(var == 'ndvi_median', !is.na(val)) %>%
    mutate(year = lubridate::year(date), comp = lubridate::yday(date) %/% 16) %>%
    group_by(site_code, comp) %>% mutate(clim = mean(val)) %>%
    group_by(site_code) %>% filter(clim >= gs_min_frac_of_max * max(clim)) %>%
    group_by(site_code, water_year = year) %>%
    summarize(val = median(val), .groups = 'drop') %>%
    filter(water_year %in% 2000:2022) %>%
    mutate(var = 'ndvi_modis')
d <- bind_rows(ga, ndvi_modis)

# 1. series minus L5+L7-only series, 2013-2017 (0 = calibration adds no offset)
# 2. series minus MODIS: change from 2001-2012 to 2013-2021 (0 = no relative step)
wide <- d %>% pivot_wider(names_from = var, values_from = val)
ndvi_series <- grep('^ndvi_gs_(raw|xcal|leohs)', names(wide), value = TRUE)
nirv_series <- grep('^nirv_gs_(xcal|leohs)', names(wide), value = TRUE)

diag <- bind_rows(lapply(c(ndvi_series, nirv_series), function(v){
    ref <- ifelse(grepl('^ndvi', v), 'ndvi_gs_tmetm', 'nirv_gs_tmetm')
    x <- wide[[v]]
    tibble(series = v,
           minus_l57only_2013_2017 = median(tapply((x - wide[[ref]])[wide$water_year %in% 2013:2017],
                                                   wide$water_year[wide$water_year %in% 2013:2017],
                                                   median, na.rm = TRUE)),
           minus_modis_shift = if(grepl('^ndvi', v)){
               dm <- tapply(x - wide$ndvi_modis, wide$water_year, median, na.rm = TRUE)
               yr <- as.numeric(names(dm))
               mean(dm[yr %in% 2013:2021]) - mean(dm[yr %in% 2001:2012])
           } else NA_real_)
}))
cat('\nOffsets in index units (lower is better in absolute value):\n')
print(diag, width = Inf)

trend_counts <- function(yrs, label){
    d %>%
        filter(water_year %in% yrs) %>%
        select(site_code, water_year, var, val) %>%
        detect_trends() %>%
        add_flags() %>%
        count(var, flag) %>%
        pivot_wider(names_from = flag, values_from = n, values_fill = 0) %>%
        mutate(window = label, .before = 1)
}
counts <- bind_rows(trend_counts(2001:2021, '2001-2021'),
                    trend_counts(first_year:last_year, 'full record') %>% filter(var != 'ndvi_modis'))
print(counts, n = Inf, width = Inf)

write.csv(diag, file.path(gr_dir, 'leohs_comparison_offsets.csv'), row.names = FALSE)
write.csv(counts, file.path(gr_dir, 'leohs_comparison_trends.csv'), row.names = FALSE)
