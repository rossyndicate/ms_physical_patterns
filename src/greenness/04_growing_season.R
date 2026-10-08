# Annual growing-season greenness per point, then per watershed.
#
# Growing season: per point, observations falling where the multi-year phenology
# spline is >= gs_min_frac_of_max of its seasonal peak (LandsatTS definition).
# This adapts to each site's timing (e.g. winter-spring green-up at santa_barbara)
# rather than imposing a fixed calendar window.
#
# Point -> watershed: watershed level = mean of point long-term means; year-to-year
# signal = median of point anomalies among points with data that year. This keeps
# changes in which points are cloud-free (or SLC-off gaps) from leaking into the series.
#
# Calendar-year growing seasons are assigned to the water year of the same number.

source(here::here('src', 'greenness', '00_config.R'))
library(LandsatTS)

obs_main <- readRDS(file.path(gr_dir, 'obs_main.rds'))
obs_tmetm <- readRDS(file.path(gr_dir, 'obs_tmetm.rds'))

# series definitions: which observations and which column feed each annual series
series <- tribble(
    ~series,   ~obs,        ~si,    ~col,
    'xcal',    'obs_main',  'ndvi', 'ndvi.xcal',
    'raw',     'obs_main',  'ndvi', 'ndvi',
    'tmetm',   'obs_tmetm', 'ndvi', 'ndvi.xcal',
    'xcal',    'obs_main',  'nirv', 'nirv.xcal',
    'raw',     'obs_main',  'nirv', 'nirv',
    'tmetm',   'obs_tmetm', 'nirv', 'nirv.xcal')

source(here::here('src', 'greenness', 'functions.R'))

out <- list()
pt_out <- list()

for(i in seq_len(nrow(series))){

    s <- series[i, ]
    message('fitting ', s$si, ' / ', s$series)
    pt <- point_gs(get(s$obs), s$si, s$col)
    pt_out[[paste(s$si, s$series, sep = '_')]] <- pt

    for(metric in c('gs.med', 'max')){
        varname <- paste(s$si, ifelse(metric == 'gs.med', 'gs', 'max'), s$series, sep = '_')
        out[[varname]] <- ws_aggregate(pt, metric)[, var := varname]
    }
}

greenness_annual <- rbindlist(out) %>%
    as_tibble() %>%
    select(site_code, water_year, var, val, n_pts)

saveRDS(pt_out, file.path(gr_dir, 'greenness_point_annual.rds'))
saveRDS(greenness_annual, file.path(gr_dir, 'greenness_annual.rds'))

# wide, drop-in style table (one row per site-water year), primary variable first
greenness_wide <- greenness_annual %>%
    select(-n_pts) %>%
    pivot_wider(names_from = var, values_from = val) %>%
    relocate(site_code, water_year, ndvi_gs_xcal)
saveRDS(greenness_wide, file.path(gr_dir, 'greenness_annual_wide.rds'))
