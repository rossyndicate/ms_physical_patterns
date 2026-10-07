# Sample random points within each non-experimental CONUS MacroSheds watershed.
# Points are drawn from the 30 m grid of candidate locations inside each polygon,
# so no two points fall in the same Landsat pixel. Watersheds with fewer candidate
# pixels than pts_per_ws contribute all of them.

source(here::here('src', 'greenness', '00_config.R'))
library(macrosheds)

set.seed(seed_master)

# sites ####
sites <- ms_load_sites() %>%
    filter(site_type == 'stream_gauge',
           ws_status == 'non-experimental',
           latitude > 24, latitude < 50,
           longitude > -125, longitude < -66)

ws <- ms_load_spatial_product(ms_root, spatial_product = 'ws_boundary',
                              domains = unique(sites$domain)) %>%
    filter(site_code %in% sites$site_code) %>%
    left_join(select(sites, site_code, domain), by = 'site_code')

missing_ws <- setdiff(sites$site_code, ws$site_code)
if(length(missing_ws)) warning('no boundary for: ', paste(missing_ws, collapse = ', '))

# sample ####
utm_epsg <- function(lon, lat){
    ifelse(lat >= 0, 32600, 32700) + floor((lon + 180) / 6) + 1
}

sample_ws <- function(poly){

    cen <- suppressWarnings(st_coordinates(st_centroid(st_geometry(poly))))
    poly_utm <- st_transform(poly, utm_epsg(cen[1], cen[2]))
    area_m2 <- as.numeric(st_area(poly_utm))

    if(area_m2 / 900 < 20 * pts_per_ws){
        # small/medium watershed: enumerate all 30 m cells, then choose
        cand <- st_make_grid(poly_utm, cellsize = 30, what = 'centers')
        cand <- cand[lengths(st_intersects(cand, poly_utm)) > 0]
        pts <- cand[sample(length(cand), min(pts_per_ws, length(cand)))]
    } else {
        # large watershed: random draw, thinned so points occupy distinct pixels
        cand <- st_sample(poly_utm, size = pts_per_ws * 3, type = 'random')
        keep <- c()
        for(i in seq_along(cand)){
            if(length(keep) == pts_per_ws) break
            if(!length(keep) || min(as.numeric(st_distance(cand[i], cand[keep]))) > min_pt_spacing_m){
                keep <- c(keep, i)
            }
        }
        pts <- cand[keep]
    }

    st_sf(site_code = poly$site_code,
          domain = poly$domain,
          ws_area_ha = area_m2 / 1e4,
          n_cand_pixels = floor(area_m2 / 900),
          geometry = st_transform(pts, 4326))
}

pts <- lapply(split(ws, ws$site_code), sample_ws) %>%
    bind_rows() %>%
    group_by(site_code) %>%
    mutate(sample_id = paste0(site_code, '__', sprintf('%03d', row_number()))) %>%
    ungroup()

# summary ####
pt_summary <- pts %>%
    st_drop_geometry() %>%
    count(domain, site_code, ws_area_ha, n_cand_pixels, name = 'n_pts')

message(nrow(pts), ' points across ', n_distinct(pts$site_code), ' watersheds')
print(summary(pt_summary$n_pts))
print(filter(pt_summary, n_pts < pts_per_ws))

st_write(pts, file.path(gr_dir, 'sample_points.gpkg'), delete_dsn = TRUE, quiet = TRUE)
write.csv(pt_summary, file.path(gr_dir, 'sample_points_summary.csv'), row.names = FALSE)
