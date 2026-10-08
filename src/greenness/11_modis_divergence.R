# Why does MODIS NDVI rise after 2013 while calibrated Landsat NDVI falls?
#
# Hypothesis: MacroSheds MODIS NDVI (MOD13Q1 Collection 6, 250 m) is an area-weighted
# median over every MODIS pixel touching the watershed, so in small watersheds much of
# what MODIS sees lies outside the boundary (e.g. developed or agricultural land),
# whereas the Landsat points all fall inside.
#
# Part 1 (this section): per-site divergence and its relation to watershed size and to
# the share of the MODIS footprint lying outside the watershed.

source(here::here('src', 'greenness', '00_config.R'))
library(macrosheds)
library(terra)

# per-site divergence ####
ga <- readRDS(file.path(gr_dir, 'greenness_annual.rds')) %>%
    filter(var == 'ndvi_gs_xcal') %>%
    bind_rows(readRDS(file.path(gr_dir, 'greenness_annual_l7only.rds')) %>% filter(var == 'ndvi_gs_l7only')) %>%
    select(site_code, water_year, var, val)
sites <- unique(ga$site_code)

ndvi_modis <- feather::read_feather(here('data_raw', 'ms', 'v2', 'spatial_timeseries_vegetation.feather')) %>%
    filter(site_code %in% sites, var == 'ndvi_median', !is.na(val)) %>%
    mutate(year = lubridate::year(date), comp = lubridate::yday(date) %/% 16) %>%
    group_by(site_code, comp) %>% mutate(clim = mean(val)) %>%
    group_by(site_code) %>% filter(clim >= gs_min_frac_of_max * max(clim)) %>%
    group_by(site_code, water_year = year) %>%
    summarize(val = median(val), .groups = 'drop') %>%
    filter(water_year %in% 2000:2022) %>%
    mutate(var = 'ndvi_modis')

# divergence = change in (Landsat - MODIS) from 2001-2012 to the post-2013 period;
# negative = Landsat falls relative to MODIS
shift <- function(x, yr, post) mean(x[yr %in% post], na.rm = TRUE) - mean(x[yr %in% 2001:2012], na.rm = TRUE)
div <- bind_rows(ga, ndvi_modis) %>%
    pivot_wider(names_from = var, values_from = val) %>%
    group_by(site_code) %>%
    summarize(landsat_shift = shift(ndvi_gs_xcal, water_year, 2013:2021),
              modis_shift = shift(ndvi_modis, water_year, 2013:2021),
              div = shift(ndvi_gs_xcal - ndvi_modis, water_year, 2013:2021),
              div_l7only = shift(ndvi_gs_l7only - ndvi_modis, water_year, 2013:2017),
              ndvi_level = mean(ndvi_gs_xcal[water_year %in% 2001:2012], na.rm = TRUE),
              .groups = 'drop')

# MODIS footprint ####
ms_sites <- ms_load_sites() %>% filter(site_code %in% sites)
ws <- ms_load_spatial_product(ms_root, spatial_product = 'ws_boundary',
                              domains = unique(ms_sites$domain)) %>%
    filter(site_code %in% sites) %>%
    st_make_valid()

# MODIS sinusoidal grid (MOD13Q1 250 m product: 4800 x 4800 cells per 10-degree tile)
sinu <- '+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +R=6371007.181 +units=m +no_defs'
modis_res <- 231.656358263958
modis_x0 <- -20015109.354
modis_y0 <- 10007554.677

footprint <- function(poly){
    p <- vect(st_transform(poly, sinu))
    e <- ext(p)
    snap <- function(v, o, f) o + f((v - o) / modis_res) * modis_res
    r <- rast(xmin = snap(e$xmin, modis_x0, floor), xmax = snap(e$xmax, modis_x0, ceiling),
              ymin = snap(e$ymin, modis_y0, floor), ymax = snap(e$ymax, modis_y0, ceiling),
              resolution = modis_res, crs = sinu)
    cover <- rasterize(p, r, cover = TRUE)
    v <- values(cover, mat = FALSE)
    v <- v[!is.na(v) & v > 0]
    # GEE weights each pixel by the fraction inside; the outside share of what the
    # weighted median sees is sum(w * (1 - w)) / sum(w)
    tibble(area_km2 = as.numeric(st_area(poly)) / 1e6,
           n_modis_px = length(v),
           outside_share = sum(v * (1 - v)) / sum(v))
}
fp <- bind_rows(lapply(seq_len(nrow(ws)), function(i) footprint(ws[i, ]) %>% mutate(site_code = ws$site_code[i])))
div <- left_join(div, fp, by = 'site_code') %>% left_join(select(ms_sites, site_code, domain), by = 'site_code')

saveRDS(div, file.path(gr_dir, 'modis_divergence_sites.rds'))

print(summary(div %>% select(div, div_l7only, area_km2, outside_share)))
cat('\nSpearman correlations with divergence:\n')
print(sapply(c('area_km2', 'outside_share', 'n_modis_px', 'ndvi_level', 'modis_shift', 'landsat_shift'),
             function(v) cor(div$div, div[[v]], method = 'spearman', use = 'complete')))
cat('\nDivergence by size class:\n')
print(div %>% mutate(size = cut(area_km2, c(0, 1, 10, 100, Inf))) %>%
          group_by(size) %>% summarize(n = n(), median_div = median(div, na.rm = TRUE),
                                       median_div_l7 = median(div_l7only, na.rm = TRUE),
                                       median_modis_shift = median(modis_shift, na.rm = TRUE),
                                       median_landsat_shift = median(landsat_shift, na.rm = TRUE)))
cat('\nStrongest negative divergence:\n')
print(div %>% arrange(div) %>% select(site_code, domain, div, div_l7only, landsat_shift, modis_shift,
                                      area_km2, n_modis_px, outside_share), n = 15, width = Inf)

# geometries for Earth Engine (11a_modis_gee.py) ####
# all watersheds, with each site's growing-season window (the MODIS composites used above)
gs_window <- feather::read_feather(here('data_raw', 'ms', 'v2', 'spatial_timeseries_vegetation.feather')) %>%
    filter(site_code %in% sites, var == 'ndvi_median', !is.na(val)) %>%
    mutate(comp = lubridate::yday(date) %/% 16) %>%
    group_by(site_code, comp) %>% summarize(clim = mean(val), .groups = 'drop_last') %>%
    filter(clim >= gs_min_frac_of_max * max(clim)) %>%
    summarize(doys = paste(sort(comp) * 16 + 1, collapse = ',')) # composite start days
gee_dir <- file.path(gr_dir, 'modis_check')
dir.create(gee_dir, showWarnings = FALSE)
ws %>% select(site_code) %>% left_join(gs_window, by = 'site_code') %>%
    st_transform(4326) %>%
    st_simplify(dTolerance = 30) %>% # metres; keeps the request to Earth Engine small
    st_write(file.path(gee_dir, 'watersheds.geojson'), delete_dsn = TRUE, quiet = TRUE)

# MODIS cell centers in a 1.5 km buffer around the four most divergent watersheds
map_sites <- div %>% filter(!is.na(div)) %>% arrange(div) %>% slice(1:4) %>% pull(site_code)
cells <- bind_rows(lapply(map_sites, function(s){
    poly <- st_transform(ws[ws$site_code == s, ], sinu)
    p <- vect(poly)
    e <- ext(vect(st_buffer(poly, 1500)))
    snap <- function(v, o, f) o + f((v - o) / modis_res) * modis_res
    r <- rast(xmin = snap(e$xmin, modis_x0, floor), xmax = snap(e$xmax, modis_x0, ceiling),
              ymin = snap(e$ymin, modis_y0, floor), ymax = snap(e$ymax, modis_y0, ceiling),
              resolution = modis_res, crs = sinu)
    cover <- rasterize(p, r, cover = TRUE, background = 0)
    xy <- as.data.frame(cover, xy = TRUE, na.rm = FALSE)
    names(xy) <- c('x', 'y', 'cover')
    st_as_sf(xy, coords = c('x', 'y'), crs = sinu) %>% mutate(site_code = s, cell = seq_len(n()))
}))
cells %>% left_join(gs_window, by = 'site_code') %>% st_transform(4326) %>%
    st_write(file.path(gee_dir, 'map_cells.geojson'), delete_dsn = TRUE, quiet = TRUE)
saveRDS(cells, file.path(gee_dir, 'map_cells_sinu.rds'))
print(map_sites)
