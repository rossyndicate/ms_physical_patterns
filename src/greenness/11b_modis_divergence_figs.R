# Figures for the Landsat-vs-MODIS divergence check (11_modis_divergence.R, 11a_modis_gee.py).
#
# modis_fig1_sites.png: (a) per-site NDVI change after 2013 for Landsat and three MODIS
#     products; (b) Landsat minus MODIS divergence vs watershed area.
# modis_fig2_maps.png: the four most divergent watersheds on satellite imagery, with
#     MODIS cells and Landsat sample points colored by their own NDVI change.
#
# Change = 2013-2021 mean minus 2003-2012 mean (Aqua starts mid-2002), NDVI units.

source(here::here('src', 'greenness', '00_config.R'))
library(terra)
library(maptiles)

chk_dir <- file.path(gr_dir, 'modis_check')
out_dirs <- c(gr_fig_dir, file.path(gr_dir, 'deliverable'))
div <- readRDS(file.path(gr_dir, 'modis_divergence_sites.rds'))
sites <- div$site_code

shift <- function(x, yr, post, pre = 2003:2012){
    if(sum(!is.na(x[yr %in% post])) < 3 || sum(!is.na(x[yr %in% pre])) < 5) return(NA_real_)
    mean(x[yr %in% post], na.rm = TRUE) - mean(x[yr %in% pre], na.rm = TRUE)
}

# site-level change ####
landsat <- bind_rows(readRDS(file.path(gr_dir, 'greenness_annual.rds')) %>% filter(var == 'ndvi_gs_xcal'),
                     readRDS(file.path(gr_dir, 'greenness_annual_l7only.rds')) %>% filter(var == 'ndvi_gs_l7only')) %>%
    group_by(site_code, var) %>%
    summarize(chg = shift(val, water_year, if(first(var) == 'ndvi_gs_l7only') 2013:2017 else 2013:2021),
              .groups = 'drop') %>%
    rename(product = var)
modis <- read.csv(file.path(chk_dir, 'site_shift.csv')) %>%
    transmute(site_code, product, chg = post - pre)
chg <- bind_rows(landsat, modis) %>% filter(site_code %in% sites, !is.na(chg))

prod_labs <- c(ndvi_gs_xcal = 'Landsat, calibrated\n(delivered)',
               ndvi_gs_l7only = 'Landsat 7 only,\nuncalibrated (to 2017)',
               mod_c6 = 'MODIS Terra C6\n(MacroSheds)',
               mod_c61 = 'MODIS Terra C6.1',
               myd_c61 = 'MODIS Aqua C6.1')
prod_cols <- c(ndvi_gs_xcal = '#000000', ndvi_gs_l7only = '#CC79A7', mod_c6 = '#0072B2',
               mod_c61 = '#56B4E9', myd_c61 = '#009E73')
chg <- chg %>% mutate(product = factor(product, names(prod_labs)))
med <- chg %>% group_by(product) %>% summarize(m = median(chg), n = n())
print(med)

# same-site check that the Earth Engine C6 numbers reproduce the MacroSheds series
chk <- inner_join(modis %>% filter(product == 'mod_c6'), div, by = 'site_code')
cat('Spearman, MacroSheds C6 shift vs Earth Engine C6 shift:',
    round(cor(chk$chg, chk$modis_shift, method = 'spearman', use = 'complete'), 2), '\n')

p1a <- ggplot(chg, aes(product, chg, color = product)) +
    geom_hline(yintercept = 0, color = 'grey60') +
    geom_boxplot(outlier.shape = NA, width = 0.55) +
    geom_jitter(width = 0.12, size = 0.8, alpha = 0.5) +
    geom_label(data = med, aes(y = 0.085, label = sprintf('median %+.3f', m)), size = 3, linewidth = 0) +
    scale_color_manual(values = prod_cols, guide = 'none') +
    scale_x_discrete(labels = prod_labs) +
    coord_cartesian(ylim = c(-0.1, 0.09)) +
    labs(x = NULL, y = 'NDVI change, 2013-2021 minus 2003-2012',
         title = '(a) Only the MODIS product in MacroSheds (Terra Collection 6) rises after 2013',
         subtitle = 'One point per site. The reprocessed MODIS products (C6.1, Terra and Aqua) barely change') +
    theme_bw() + theme(panel.grid = element_blank())

rho <- function(v) cor(div$div, div[[v]], method = 'spearman', use = 'complete')
p1b <- ggplot(div, aes(area_km2, div)) +
    geom_hline(yintercept = 0, color = 'grey60') +
    geom_point(aes(color = outside_share), size = 1.8) +
    geom_smooth(method = 'loess', formula = y ~ x, se = FALSE, color = 'black', linewidth = 0.6) +
    scale_x_log10(labels = scales::label_number(drop0trailing = TRUE)) +
    scale_color_viridis_c(name = 'Share of MODIS\nfootprint outside\nwatershed', limits = c(0, 0.9)) +
    labs(x = 'Watershed area (km², log scale)',
         y = 'Change in Landsat minus MODIS C6 NDVI\n(2013-2021 vs 2001-2012)',
         title = '(b) The divergence does not depend on watershed size',
         subtitle = sprintf('Spearman correlation with area %.2f; with share of MODIS footprint outside the watershed %.2f',
                            rho('area_km2'), rho('outside_share'))) +
    theme_bw() + theme(panel.grid = element_blank())

fig1 <- patchwork::wrap_plots(p1a, p1b, ncol = 1)
for(od in out_dirs) ggsave(file.path(od, 'modis_fig1_sites.png'), fig1, width = 10, height = 10, dpi = 200)

# maps ####
cells <- readRDS(file.path(chk_dir, 'map_cells_sinu.rds'))
map_sites <- unique(cells$site_code)
modis_res <- 231.656358263958
cell_chg <- read.csv(file.path(chk_dir, 'cell_shift.csv')) %>%
    filter(product == 'mod_c6') %>%
    transmute(site_code, cell, chg = post - pre)
cell_poly <- cells %>%
    left_join(cell_chg, by = c('site_code', 'cell')) %>%
    mutate(geometry = st_buffer(geometry, modis_res / 2, endCapStyle = 'SQUARE')) %>%
    st_transform(3857)

pts <- st_read(file.path(gr_dir, 'sample_points.gpkg'), quiet = TRUE) %>% filter(site_code %in% map_sites)
pt_chg <- readRDS(file.path(gr_dir, 'greenness_point_annual.rds'))$ndvi_xcal %>%
    as_tibble() %>%
    filter(sub('__[0-9]+$', '', sample.id) %in% map_sites) %>%
    group_by(sample_id = sample.id) %>%
    summarize(chg = shift(gs.med, year, 2013:2021), .groups = 'drop')
pts <- left_join(pts, pt_chg, by = 'sample_id') %>% st_transform(3857)

ws <- st_read(file.path(chk_dir, 'watersheds.geojson'), quiet = TRUE) %>%
    filter(site_code %in% map_sites) %>% st_transform(3857)

lim <- 0.15
chg_scale <- scale_fill_gradient2(low = '#8B4513', mid = 'white', high = '#1B7837', midpoint = 0,
                                  limits = c(-lim, lim), oob = scales::squish,
                                  name = 'NDVI change,\n2013-2021 minus\n2003-2012')

# watershed-level change, same windows as the maps
site_chg <- function(p) with(filter(chg, product == p), setNames(chg, site_code))
ls_chg <- site_chg('ndvi_gs_xcal'); mod_chg <- site_chg('mod_c6'); mod61_chg <- site_chg('mod_c61')

site_maps <- function(s){
    d <- div[div$site_code == s, ]
    cp <- filter(cell_poly, site_code == s)
    w <- filter(ws, site_code == s)
    p <- filter(pts, site_code == s)
    # frame: watershed plus a margin of at least ~3 MODIS cells
    wb <- st_bbox(w)
    pad <- max(750, 0.3 * max(wb$xmax - wb$xmin, wb$ymax - wb$ymin))
    bb <- wb + c(-pad, -pad, pad, pad)
    tiles <- get_tiles(st_as_sfc(bb), provider = 'Esri.WorldImagery', crop = TRUE,
                       zoom = ifelse(bb$xmax - bb$xmin < 4000, 16, 15))
    img <- as.data.frame(tiles, xy = TRUE) %>% setNames(c('x', 'y', 'r', 'g', 'b')) %>%
        mutate(rgb = rgb(r, g, b, maxColorValue = 255))
    base <- list(coord_sf(crs = 3857, xlim = bb[c(1, 3)], ylim = bb[c(2, 4)], expand = FALSE, datum = NA),
                 theme_void(),
                 theme(plot.title = element_text(size = 10, face = 'bold'),
                       plot.subtitle = element_text(size = 8)))
    m1 <- ggplot() +
        geom_raster(data = img, aes(x, y, fill = rgb)) + scale_fill_identity() +
        geom_sf(data = cp, fill = NA, color = 'white', linewidth = 0.15, alpha = 0.5) +
        geom_sf(data = w, fill = NA, color = 'yellow', linewidth = 0.8) +
        geom_sf(data = p, color = 'yellow', size = 0.6) +
        base +
        labs(title = sprintf('%s (%s), %.2f km²', s, d$domain, d$area_km2),
             subtitle = 'Imagery with MODIS 250 m cells, watershed, Landsat sample points')
    m2 <- ggplot() +
        geom_sf(data = cp, aes(fill = chg), color = 'grey80', linewidth = 0.1) +
        geom_sf(data = w, fill = NA, color = 'black', linewidth = 0.8) +
        geom_sf(data = p, aes(fill = chg), shape = 21, size = 2, color = 'black', stroke = 0.3) +
        chg_scale + base +
        labs(title = sprintf('Landsat %+.3f   MODIS C6 %+.3f   (C6.1 %+.3f)', ls_chg[s], mod_chg[s], mod61_chg[s]),
             subtitle = 'Cells: MODIS Terra C6 change. Dots: Landsat change at each sample point')
    list(m1, m2)
}
fig2 <- patchwork::wrap_plots(unlist(lapply(map_sites, site_maps), recursive = FALSE), ncol = 2,
                              guides = 'collect') +
    patchwork::plot_annotation(
        title = 'The four watersheds where Landsat falls furthest relative to MODIS after 2013',
        subtitle = 'Same color scale for MODIS cells and Landsat points; titles give watershed-level change') &
    theme(plot.background = element_rect(fill = 'white', color = NA))
for(od in out_dirs) ggsave(file.path(od, 'modis_fig2_maps.png'), fig2, width = 11, height = 18, dpi = 200, bg = 'white')
