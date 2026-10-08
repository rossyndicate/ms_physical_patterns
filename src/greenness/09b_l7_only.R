# Landsat 7 only, uncalibrated: growing-season NDVI and NIRv from Landsat 7 ETM+ alone,
# 1999-2017. Nothing is cross-calibrated, so this series can show whether the drop
# relative to MODIS around 2013 is a calibration artifact. It cannot be one here.

source(here::here('src', 'greenness', '00_config.R'))
library(LandsatTS)
source(here::here('src', 'greenness', 'functions.R'))

obs <- readRDS(file.path(gr_dir, 'obs_main.rds'))[satellite == 'LANDSAT_7']

out <- list()
for(si in c('ndvi', 'nirv')){
    message('fitting ', si, ' / Landsat 7 only')
    pt <- point_gs(obs, si, si)
    varname <- paste0(si, '_gs_l7only')
    out[[varname]] <- ws_aggregate(pt, 'gs.med')[, var := varname]
}

res <- rbindlist(out) %>% as_tibble() %>% select(site_code, water_year, var, val, n_pts)
saveRDS(res, file.path(gr_dir, 'greenness_annual_l7only.rds'))
