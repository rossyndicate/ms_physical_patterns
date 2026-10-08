# Collaborator figures for the LEOHS comparison: the same three views as
# 08_collaborator_figures.R, with the Landsat 8/9 -> Landsat 7 step done by the
# LandsatTS random forest (our series) or by LEOHS linear band equations fitted to
# our watersheds (09a_run_leohs.py, 09_leohs.R; OLS, RMA and Theil-Sen fits).
#
# leohs_fig1: NDVI anomaly from each series' own 2001-2012 mean, in NDVI units, with
#             Landsat 7 alone (uncalibrated) and MODIS NDVI for reference.
# leohs_fig2: Sen's slope trend flags, NDVI and NIRv, per calibration.
# leohs_fig3: summer NDVI by sensor under each calibration.
# leohs_fig4: one-plot summary: each calibration's agreement with Landsat 7 on two tests.
#
# Requires outputs of 04_growing_season.R, 09_leohs.R (regional_* sets) and 09b_l7_only.R.

source(here::here('src', 'greenness', '00_config.R'))
source(here::here('src', 'greenness', 'functions.R'))
suppressMessages(source(here::here('src', 'setup.R'))) # detect_trends(), add_flags()

out_dirs <- c(gr_fig_dir, file.path(gr_dir, 'deliverable'))
leohs_sets <- c('regional_ols', 'regional_rma', 'regional_ts')

ga <- bind_rows(readRDS(file.path(gr_dir, 'greenness_annual.rds')) %>%
                    filter(var %in% c('ndvi_gs_raw', 'ndvi_gs_xcal', 'nirv_gs_raw', 'nirv_gs_xcal')),
                lapply(file.path(gr_dir, paste0('greenness_annual_leohs_', leohs_sets, '.rds')), readRDS),
                readRDS(file.path(gr_dir, 'greenness_annual_l7only.rds'))) %>%
    select(-n_pts)
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
d <- bind_rows(ga, ndvi_modis)

cal_labs <- c(xcal = 'LandsatTS random forest (delivered series)',
              leohs_regional_ols = 'LEOHS, OLS',
              leohs_regional_rma = 'LEOHS, RMA',
              leohs_regional_ts = 'LEOHS, Theil-Sen')

# fig1: NDVI anomaly series ####
anom <- d %>%
    group_by(site_code, var) %>%
    mutate(a = val - mean(val[water_year %in% 2001:2012], na.rm = TRUE)) %>%
    ungroup() %>%
    filter(is.finite(a)) %>%
    group_by(var, water_year) %>%
    summarize(med = median(a), q25 = quantile(a, 0.25), q75 = quantile(a, 0.75),
              n = n(), .groups = 'drop') %>%
    filter(n >= 0.5 * length(sites))

# drop after 2013: 2013-2017 mean minus 2001-2012 mean (all series cover 2013-2017)
shift <- anom %>%
    group_by(var) %>%
    summarize(shift = mean(med[water_year %in% 2013:2017]) - mean(med[water_year %in% 2001:2012]))
print(shift)
sh <- setNames(shift$shift, shift$var)

era_bars <- anom %>%
    mutate(era = cut(water_year, c(-Inf, 1998, 2012, Inf), labels = c('TM', 'TM + ETM+', 'OLI era'))) %>%
    filter(!(var == 'ndvi_modis' & water_year < 2001)) %>%
    group_by(var, era) %>%
    summarize(mean = mean(med), start = min(water_year) - 0.4, end = max(water_year) + 0.4, .groups = 'drop')

events <- tibble(x = c(1999, 2011, 2013, 2017.5),
                 lab = c('Landsat 7 launched', 'Landsat 5 retired', 'Landsat 8 launched',
                         'Landsat 7 dropped (orbit drift)'))

ref_cols <- c(focal = '#000000', ndvi_gs_l7only = '#CC79A7', ndvi_modis = '#0072B2')
ref_labs <- c(focal = 'Landsat NDVI, calibrated as titled',
              ndvi_gs_l7only = 'Landsat 7 only, uncalibrated (1999-2017)',
              ndvi_modis = 'MODIS NDVI (250 m)')

panel <- function(cal, letter){
    v <- paste0('ndvi_gs_', cal)
    keep <- c(v, 'ndvi_gs_l7only', 'ndvi_modis')
    recode <- function(x) factor(ifelse(x == v, 'focal', x), names(ref_cols))
    s <- filter(anom, var %in% keep) %>% mutate(var = recode(var))
    e <- filter(era_bars, var %in% keep) %>% mutate(var = recode(var))
    sub <- sprintf('2013-2017 minus 2001-2012:  this series %+.3f   |   Landsat 7 only %+.3f   |   MODIS %+.3f',
                   sh[v], sh['ndvi_gs_l7only'], sh['ndvi_modis'])
    ggplot(s, aes(water_year, med, color = var, fill = var)) +
        geom_hline(yintercept = 0, color = 'grey70') +
        geom_vline(data = events, aes(xintercept = x), linetype = 'dashed', color = 'grey50') +
        geom_text(data = events, aes(x = x - 0.4, y = 0.045, label = lab), inherit.aes = FALSE,
                  angle = 90, hjust = 1, size = 2.5, color = 'grey35') +
        geom_ribbon(data = filter(s, var == 'focal'), aes(ymin = q25, ymax = q75), alpha = 0.12, color = NA) +
        geom_line() + geom_point(size = 1.1) +
        geom_segment(data = e, aes(x = start, xend = end, y = mean, yend = mean, color = var),
                     linewidth = 1.5, inherit.aes = FALSE, alpha = 0.6) +
        scale_color_manual(values = ref_cols, labels = ref_labs, drop = FALSE) +
        scale_fill_manual(values = ref_cols, labels = ref_labs, drop = FALSE) +
        coord_cartesian(xlim = c(1984, 2025.5), ylim = c(-0.08, 0.05)) +
        labs(x = NULL, y = 'NDVI minus 2001-2012 mean', color = NULL, fill = NULL,
             title = paste0('(', letter, ') ', cal_labs[cal]), subtitle = sub) +
        theme_bw() +
        theme(legend.position = 'bottom', panel.grid = element_blank(),
              plot.subtitle = element_text(size = 9))
}

fig1 <- patchwork::wrap_plots(mapply(panel, names(cal_labs), letters[seq_along(cal_labs)], SIMPLIFY = FALSE),
                              ncol = 2, guides = 'collect') +
    patchwork::plot_annotation(
        title = 'Growing-season NDVI under four Landsat 8/9 calibrations',
        subtitle = paste0('Cross-site median of each site\'s departure from its own 2001-2012 mean, ', length(sites),
                          ' non-experimental CONUS sites; band = interquartile range; bar = sensor-era mean')) &
    theme(legend.position = 'bottom')
for(od in out_dirs) ggsave(file.path(od, 'leohs_fig1_series.png'), fig1, width = 14, height = 9, dpi = 200)

# fig2: trend flags ####
trend_flags <- function(yrs, vars, label){
    d %>%
        filter(var %in% vars, water_year %in% yrs) %>%
        select(site_code, water_year, var, val) %>%
        detect_trends() %>%
        add_flags() %>%
        mutate(window = label)
}
ndvi_vars <- c('ndvi_gs_raw', paste0('ndvi_gs_', names(cal_labs)), 'ndvi_modis')
nirv_vars <- paste0('nirv_gs_', names(cal_labs))
w1 <- trend_flags(2001:2021, c(ndvi_vars, nirv_vars), '2001-2021 (MODIS overlap)')
w2 <- trend_flags(first_year:last_year, c(setdiff(ndvi_vars, 'ndvi_modis'), nirv_vars), 'Full Landsat record')

bar_labs <- c(raw = 'Uncalibrated', xcal = 'LandsatTS\nrandom forest',
              leohs_regional_ols = 'LEOHS\nOLS', leohs_regional_rma = 'LEOHS\nRMA',
              leohs_regional_ts = 'LEOHS\nTheil-Sen', modis = 'MODIS')
flag_cols <- c(increasing = '#1B9E1B', 'non-significant' = 'grey75',
               decreasing = '#8B2323', 'insufficient data' = 'black')
counts <- bind_rows(w1, w2) %>%
    right_join(bind_rows(expand_grid(site_code = sites, var = c(ndvi_vars, nirv_vars),
                                     window = unique(w1$window)),
                         expand_grid(site_code = sites, var = c(setdiff(ndvi_vars, 'ndvi_modis'), nirv_vars),
                                     window = unique(w2$window))),
               by = c('site_code', 'var', 'window')) %>%
    mutate(flag = ifelse(is.na(flag) | !flag %in% names(flag_cols), 'insufficient data', flag),
           flag = factor(flag, rev(names(flag_cols))),
           index = toupper(sub('_.*', '', var)),
           cal = sub('^(ndvi|nirv)_(gs_)?', '', var),
           cal = factor(cal, names(bar_labs), bar_labs)) %>%
    count(window, index, cal, flag)
print(counts %>% pivot_wider(names_from = flag, values_from = n, values_fill = 0), n = Inf, width = Inf)
write.csv(counts, file.path(gr_dir, 'deliverable', 'leohs_fig2_trend_counts.csv'), row.names = FALSE)

fig2 <- ggplot(counts, aes(cal, n, fill = flag)) +
    geom_col(width = 0.7) +
    geom_text(aes(label = ifelse(n >= 6, n, '')), position = position_stack(vjust = 0.5),
              color = 'white', fontface = 'bold', size = 3.2) +
    facet_grid(index ~ window, scales = 'free_x', space = 'free_x') +
    scale_fill_manual(values = flag_cols, breaks = names(flag_cols)) +
    labs(x = NULL, y = 'Sites', fill = "Trend\n(Sen's slope, 95% CI)",
         title = 'Sites flagged as greening or browning, by Landsat 8/9 calibration',
         subtitle = paste0(length(sites), ' non-experimental CONUS sites, same sites and years in each panel')) +
    theme_bw() +
    theme(axis.text.x = element_text(size = 8), panel.grid = element_blank())
for(od in out_dirs) ggsave(file.path(od, 'leohs_fig2_trend_counts.png'), fig2, width = 12, height = 8, dpi = 200)

# fig3: summer NDVI by sensor ####
# Jun-Aug median per pixel, year and sensor; then site median; then departure from the
# site's Landsat 7 (1999-2017) mean; then cross-site median. As in 08_*, fig3.
obs <- readRDS(file.path(gr_dir, 'obs_main.rds'))[doy %in% 152:243]
coefs <- read.csv(here('src', 'greenness', 'leohs_coefficients.csv'))
for(cs in leohs_sets){
    leohs_apply(obs, coefs[coefs$set == cs, ])
    setnames(obs, c('ndvi.leohs', 'nirv.leohs'), paste0(c('ndvi', 'nirv'), '_leohs_', cs))
}
versions <- c('raw', names(cal_labs))

summer_by_sensor <- function(si){
    cols <- setNames(c(si, paste0(si, '.xcal'), paste0(si, '_', names(cal_labs)[-1])), versions)
    x <- obs[, c('site_code', 'sample.id', 'sensor', 'year', cols), with = FALSE]
    setnames(x, cols, names(cols))
    x <- melt(x, id.vars = c('site_code', 'sample.id', 'sensor', 'year'), variable.name = 'version')[
        !is.na(value), .(value = median(value)), by = .(site_code, sample.id, sensor, year, version)][
        , .(value = median(value), n_px = .N), by = .(site_code, sensor, year, version)][n_px >= 5]
    l7_ref <- x[sensor == 'LE7' & version == 'raw', .(ref = mean(value)), by = site_code]
    x <- x[l7_ref, on = 'site_code'][, dep := value - ref]
    x[, .(med = median(dep), n_sites = .N), by = .(sensor, version, year)][n_sites >= 0.5 * length(sites)]
}
overlap_gap <- function(ss){
    ov <- ss[year %in% 2013:2017 & sensor %in% c('LE7', 'LC8'), .(med = mean(med)), by = .(version, sensor)]
    dcast(ov, version ~ sensor, value.var = 'med')[, gap := LC8 - LE7]
}

sensor_series <- summer_by_sensor('ndvi')
overlap <- overlap_gap(sensor_series)
print(overlap)

panel_labs <- setNames(paste0('(', letters[seq_along(versions)], ') ',
                              c('Uncalibrated', cal_labs)), versions)
sensor_series[, version := factor(version, versions, panel_labs)]
ann <- overlap[, .(version = factor(version, versions, panel_labs),
                   lab = sprintf('Landsat 8 minus Landsat 7, 2013-2017: %+.3f NDVI', gap))]

sensor_cols <- c(LT5 = '#1B9E77', LE7 = '#D95F02', LC8 = '#7570B3', LC9 = '#E7298A')
sensor_labs <- c(LT5 = 'Landsat 5 (TM)', LE7 = 'Landsat 7 (ETM+)', LC8 = 'Landsat 8 (OLI)', LC9 = 'Landsat 9 (OLI-2)')

fig3 <- ggplot(sensor_series, aes(year, med, color = sensor)) +
    geom_hline(yintercept = 0, color = 'grey70') +
    geom_line() + geom_point(size = 1.2) +
    geom_text(data = ann, aes(x = 1984, y = 0.045, label = lab), inherit.aes = FALSE, hjust = 0, size = 3) +
    facet_wrap(~version, ncol = 2) +
    scale_color_manual(values = sensor_cols, labels = sensor_labs, breaks = names(sensor_cols)) +
    labs(x = 'Year', y = "Summer NDVI minus site's Landsat 7 mean", color = NULL,
         title = 'Summer (Jun-Aug) NDVI by sensor under each Landsat 8/9 calibration, cross-site median',
         subtitle = 'Landsat 5 uses the LandsatTS calibration in every panel; where sensors overlap, calibrated values should coincide') +
    theme_bw() +
    theme(legend.position = 'bottom', panel.grid = element_blank(),
          strip.background = element_blank(), strip.text = element_text(hjust = 0, face = 'bold'))
for(od in out_dirs) ggsave(file.path(od, 'leohs_fig3_ndvi_by_sensor.png'), fig3, width = 12, height = 10, dpi = 200)

# fig4: summary of both tests against Landsat 7 ####
# x: Landsat 8 minus Landsat 7 where they overlap (fig3); y: drop after 2013 beyond
# that of uncalibrated Landsat 7 alone (fig1). (0, 0) = no offset relative to Landsat 7.
tests <- rbindlist(lapply(c('ndvi', 'nirv'), function(si){
    ov <- if(si == 'ndvi') overlap else overlap_gap(summer_by_sensor(si))
    ov[, .(index = toupper(si), version = as.character(version), gap,
           extra_drop = sh[paste0(si, '_gs_', version)] - sh[paste0(si, '_gs_l7only')])]
}))
tests[, lab := c(raw = 'Uncalibrated', cal_labs)[version]]
tests[version == 'xcal', lab := 'LandsatTS random forest\n(delivered)']
print(tests)
write.csv(tests, file.path(gr_dir, 'deliverable', 'leohs_fig4_summary.csv'), row.names = FALSE)

fig4 <- ggplot(tests, aes(gap, extra_drop)) +
    geom_hline(yintercept = 0, color = 'grey60') +
    geom_vline(xintercept = 0, color = 'grey60') +
    geom_point(aes(shape = version == 'raw', color = version == 'xcal'), size = 3.2, stroke = 1.1) +
    ggrepel::geom_text_repel(aes(label = lab), size = 3.2, min.segment.length = 0, seed = 1,
                             box.padding = 0.5, lineheight = 0.9) +
    scale_shape_manual(values = c(`TRUE` = 1, `FALSE` = 16), guide = 'none') +
    scale_color_manual(values = c(`TRUE` = '#D55E00', `FALSE` = 'black'), guide = 'none') +
    facet_wrap(~index, scales = 'free') +
    labs(x = 'Landsat 8 minus Landsat 7 where both flew (summer 2013-2017)',
         y = 'Change after 2013 beyond that of\nuncalibrated Landsat 7 alone',
         title = 'How closely each Landsat 8/9 calibration agrees with Landsat 7',
         subtitle = paste0('Index units; (0, 0) = no offset relative to Landsat 7, which no calibration modifies. ',
                           'Change after 2013 = 2013-2017 mean minus 2001-2012 mean.\n',
                           'Cross-site medians, ', length(sites), ' non-experimental CONUS sites; ',
                           'Landsat 5 uses the LandsatTS calibration throughout')) +
    theme_bw() +
    theme(panel.grid = element_blank(), plot.subtitle = element_text(size = 9),
          strip.background = element_blank(), strip.text = element_text(face = 'bold', size = 11),
          plot.margin = margin(5.5, 20, 5.5, 5.5))
for(od in out_dirs) ggsave(file.path(od, 'leohs_fig4_summary.png'), fig4, width = 11, height = 5.5, dpi = 200)
