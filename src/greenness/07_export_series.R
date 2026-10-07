# Collaborator deliverable: annual watershed greenness series that replace Robinson
# Landsat GPP (gpp_CONUS_30m_median) in water-year analyses.
#
# One row per site x water year. ndvi and nirv are the cross-calibrated
# growing-season medians (ndvi_gs_xcal, nirv_gs_xcal from 04_*). Use them where
# the annual (water-year mean) GPP was used. There is no seasonal or monthly
# equivalent; these are one value per growing season.

source(here::here('src', 'greenness', '00_config.R'))

out_dir <- file.path(gr_dir, 'deliverable')
dir.create(out_dir, showWarnings = FALSE)

ga <- readRDS(file.path(gr_dir, 'greenness_annual.rds'))
pts <- read.csv(file.path(gr_dir, 'sample_points_summary.csv'))

site_info <- macrosheds::ms_load_sites() %>%
    distinct(network, domain, site_code)

series <- ga %>%
    filter(var %in% c('ndvi_gs_xcal', 'nirv_gs_xcal')) %>%
    mutate(var = sub('_gs_xcal$', '', var)) %>%
    pivot_wider(names_from = var, values_from = c(val, n_pts)) %>%
    rename(ndvi = val_ndvi, nirv = val_nirv,
           n_points_ndvi = n_pts_ndvi, n_points_nirv = n_pts_nirv) %>%
    left_join(select(pts, site_code, n_points_site = n_pts), by = 'site_code') %>%
    left_join(site_info, by = 'site_code') %>%
    select(network, domain, site_code, water_year, ndvi, nirv,
           n_points_ndvi, n_points_nirv, n_points_site) %>%
    arrange(site_code, water_year)

write.csv(series, file.path(out_dir, 'ms_landsat_greenness_annual.csv'), row.names = FALSE)

message(nrow(series), ' site-years; ', n_distinct(series$site_code), ' sites; water years ',
        paste(range(series$water_year), collapse = '-'))
