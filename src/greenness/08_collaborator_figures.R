# Figures for collaborators showing that the cross-calibration removed the sensor
# steps. Styled after the Landsat-vs-MODIS GPP figure they have already seen.
#
# fig1: cross-site median of site-standardized annual series. Top: Landsat GPP vs
#       MODIS GPP (the problem). Bottom: Landsat NDVI uncalibrated vs calibrated vs
#       MODIS NDVI (single sensor, independent reference).
# fig2: Sen's slope trend flags for each series, same sites, same years.
# fig3: summer NDVI by Landsat sensor, before and after calibration.
#
# MODIS growing-season NDVI is built the same way as the Landsat metric: per site,
# the 16-day composites whose multi-year mean is >= 75% of the seasonal peak, median
# per year.

source(here::here('src', 'greenness', '00_config.R'))
suppressMessages(source(here::here('src', 'setup.R'))) # detect_trends(), add_flags()

out_dirs <- c(gr_fig_dir, file.path(gr_dir, 'deliverable'))

ga <- readRDS(file.path(gr_dir, 'greenness_annual.rds'))
sites <- unique(ga$site_code)

veg <- feather::read_feather(here('data_raw', 'ms', 'v2', 'spatial_timeseries_vegetation.feather')) %>%
    filter(site_code %in% sites)

gpp_landsat <- veg %>%
    filter(var == 'gpp_CONUS_30m_median') %>%
    mutate(water_year = water_year(date)) %>%
    group_by(site_code, water_year) %>%
    summarize(val = mean(val, na.rm = TRUE), .groups = 'drop') %>%
    filter(water_year %in% 1987:2021) %>% # WY1986 and WY2022 are partial years
    mutate(var = 'gpp_landsat')

gpp_modis <- veg %>%
    filter(var == 'gpp_global_500m_median') %>%
    transmute(site_code, water_year = year, val, var = 'gpp_modis')

ndvi_modis <- veg %>%
    filter(var == 'ndvi_median', !is.na(val)) %>%
    mutate(year = lubridate::year(date), doy = lubridate::yday(date),
           comp = doy %/% 16) %>%
    group_by(site_code, comp) %>%
    mutate(clim = mean(val)) %>%
    group_by(site_code) %>%
    filter(clim >= gs_min_frac_of_max * max(clim)) %>%
    group_by(site_code, water_year = year) %>%
    summarize(val = median(val), n = n(), .groups = 'drop') %>%
    # drop partial first/last years (MODIS record starts Feb 2000, ends early 2023)
    filter(water_year %in% 2000:2022) %>%
    select(-n) %>%
    mutate(var = 'ndvi_modis')

d <- bind_rows(ga %>% filter(var %in% c('ndvi_gs_raw', 'ndvi_gs_xcal', 'nirv_gs_xcal', 'ndvi_gs_tmetm')) %>% select(-n_pts),
               gpp_landsat, gpp_modis, ndvi_modis)

labs_var <- c(gpp_landsat = 'Landsat GPP (NTSG, 30 m)',
              gpp_modis = 'MODIS GPP (MOD17A3HGF, 500 m)',
              ndvi_gs_raw = 'Landsat NDVI, uncalibrated',
              ndvi_gs_xcal = 'Landsat NDVI, calibrated',
              ndvi_gs_tmetm = 'Landsat NDVI, Landsat 5 + 7 only (no OLI)',
              nirv_gs_xcal = 'Landsat NIRv, calibrated',
              ndvi_modis = 'MODIS NDVI (250 m)')
cols_var <- c(gpp_landsat = '#D55E00', gpp_modis = '#0072B2',
              ndvi_gs_raw = '#D55E00', ndvi_gs_xcal = '#000000', ndvi_gs_tmetm = '#009E73',
              ndvi_modis = '#0072B2')

# fig1: standardized series ####
std <- d %>%
    group_by(site_code, var) %>%
    mutate(ref_mean = mean(val[water_year %in% 2001:2021], na.rm = TRUE),
           ref_sd = sd(val[water_year %in% 2001:2021], na.rm = TRUE),
           z = (val - ref_mean) / ref_sd) %>%
    ungroup() %>%
    filter(is.finite(z)) %>%
    group_by(var, water_year) %>%
    summarize(med = median(z), q25 = quantile(z, 0.25), q75 = quantile(z, 0.75),
              n = n(), .groups = 'drop') %>%
    filter(n >= 0.5 * length(sites)) # drop years where most sites lack data (partial years)

eras <- tribble(~era, ~start, ~end,
                'TM', 1984, 1998,
                'TM + ETM+', 1999, 2012,
                'OLI era', 2013, 2025)
era_means <- std %>%
    filter(var %in% c('gpp_landsat', 'ndvi_gs_raw', 'ndvi_gs_xcal', 'gpp_modis', 'ndvi_modis')) %>%
    mutate(era = cut(water_year, c(-Inf, 1998, 2012, Inf), labels = eras$era)) %>%
    group_by(var, era) %>%
    summarize(mean = mean(med), start = min(water_year) - 0.4, end = max(water_year) + 0.4,
              .groups = 'drop')
# MODIS has no sensor eras; split at 2013 only for comparison with Landsat
era_means <- era_means %>% filter(!(var %in% c('gpp_modis', 'ndvi_modis') & era == 'TM'))

events <- tibble(x = c(1999, 2003, 2011, 2013, 2017.5),
                 lab = c('Landsat 7 launched', 'Landsat 7 SLC failure', 'Landsat 5 retired',
                         'Landsat 8 launched', 'Landsat 7 dropped (orbit drift)'))

panel_series <- function(vars, title, subtitle, show_drift = FALSE){
    s <- filter(std, var %in% vars) %>% mutate(var = factor(var, vars))
    e <- filter(era_means, var %in% vars)
    ev <- if(show_drift) events else filter(events, x != 2017.5)
    ggplot(s, aes(water_year, med, color = var, fill = var)) +
        geom_hline(yintercept = 0, color = 'grey70') +
        geom_vline(data = ev, aes(xintercept = x), linetype = 'dashed', color = 'grey50') +
        geom_text(data = ev, aes(x = x - 0.4, y = 2.2, label = lab), inherit.aes = FALSE,
                  angle = 90, hjust = 1, size = 2.8, color = 'grey35') +
        geom_ribbon(aes(ymin = q25, ymax = q75), alpha = 0.15, color = NA) +
        geom_line() + geom_point(size = 1.3) +
        geom_segment(data = e, aes(x = start, xend = end, y = mean, yend = mean),
                     linewidth = 1.6, inherit.aes = FALSE,
                     color = cols_var[as.character(e$var)]) +
        scale_color_manual(values = cols_var, labels = labs_var) +
        scale_fill_manual(values = cols_var, labels = labs_var) +
        coord_cartesian(xlim = c(1984, 2025.5), ylim = c(-3, 2.3)) +
        labs(x = 'Water year', y = 'Anomaly (SD units; each site\nscaled on 2001-2021)',
             color = NULL, fill = NULL, title = title, subtitle = subtitle) +
        theme_bw() +
        theme(legend.position = c(0.99, 0.02), legend.justification = c(1, 0),
              legend.background = element_rect(fill = alpha('white', 0.8)),
              panel.grid = element_blank())
}

sub <- paste0('Cross-site median, ', length(sites),
              ' non-experimental CONUS sites; band = interquartile range; bar = sensor-era mean')
p1a <- panel_series(c('gpp_landsat', 'gpp_modis'),
                    '(a) Before: Landsat GPP steps up at sensor transitions; MODIS GPP does not', sub)
p1b <- panel_series(c('ndvi_gs_raw', 'ndvi_gs_xcal', 'ndvi_gs_tmetm', 'ndvi_modis'),
                    '(b) After: calibration removes the Landsat sensor steps',
                    paste0('Same sites; NDVI = growing-season median. After 2013, Landsat NDVI runs below MODIS NDVI,\n',
                           'including Landsat 5 + 7 alone (green), so that gap is not caused by the calibration'),
                    show_drift = TRUE)
fig1 <- patchwork::wrap_plots(p1a, p1b, ncol = 1)
for(od in out_dirs) ggsave(file.path(od, 'fig1_series_before_after.png'), fig1, width = 10, height = 9, dpi = 200)

# era step sizes, for the text
print(era_means %>% select(var, era, mean) %>% pivot_wider(names_from = era, values_from = mean))

# fig2: trend flags ####
trend_flags <- function(yrs, vars, label){
    d %>%
        filter(var %in% vars, water_year %in% yrs) %>%
        select(site_code, water_year, var, val) %>%
        detect_trends() %>%
        add_flags() %>%
        mutate(window = label)
}
w1 <- trend_flags(2001:2021, c('gpp_landsat', 'gpp_modis', 'ndvi_gs_raw', 'ndvi_gs_xcal', 'nirv_gs_xcal', 'ndvi_modis'),
                  '2001-2021 (MODIS overlap)')
w2 <- trend_flags(first_year:last_year, c('gpp_landsat', 'ndvi_gs_raw', 'ndvi_gs_xcal', 'nirv_gs_xcal'),
                  'Full Landsat record')

flag_cols <- c(increasing = '#1B9E1B', 'non-significant' = 'grey75',
               decreasing = '#8B2323', 'insufficient data' = 'black')
counts <- bind_rows(w1, w2) %>%
    right_join(expand_grid(site_code = sites, var = setdiff(names(labs_var), 'ndvi_gs_tmetm'),
                           window = unique(c(w1$window, w2$window))),
               by = c('site_code', 'var', 'window')) %>%
    filter(!(window == 'Full Landsat record' & var %in% c('gpp_modis', 'ndvi_modis'))) %>%
    mutate(flag = ifelse(is.na(flag) | !flag %in% names(flag_cols), 'insufficient data', flag),
           flag = factor(flag, rev(names(flag_cols))),
           var = factor(var, setdiff(names(labs_var), 'ndvi_gs_tmetm'),
                        labels = sub(' \\(', '\n(', sub(', ', '\n', labs_var[setdiff(names(labs_var), 'ndvi_gs_tmetm')])))) %>%
    count(window, var, flag)
print(counts %>% pivot_wider(names_from = flag, values_from = n, values_fill = 0), width = Inf)
write.csv(counts, file.path(gr_dir, 'deliverable', 'fig2_trend_counts.csv'), row.names = FALSE)

fig2 <- ggplot(counts, aes(var, n, fill = flag)) +
    geom_col(width = 0.7) +
    geom_text(aes(label = ifelse(n >= 6, n, '')), position = position_stack(vjust = 0.5),
              color = 'white', fontface = 'bold', size = 3.5) +
    facet_grid(~window, scales = 'free_x', space = 'free_x') +
    scale_fill_manual(values = flag_cols, breaks = names(flag_cols)) +
    labs(x = NULL, y = 'Sites', fill = "Trend\n(Sen's slope, 95% CI)",
         title = 'Sites flagged as greening, same sites and years',
         subtitle = paste0(length(sites), ' non-experimental CONUS sites')) +
    theme_bw() +
    theme(axis.text.x = element_text(size = 8), panel.grid = element_blank())
for(od in out_dirs) ggsave(file.path(od, 'fig2_trend_counts.png'), fig2, width = 12, height = 5.5, dpi = 200)

# fig3: summer NDVI by sensor, before and after calibration ####
# Jun-Aug median per pixel, year, and sensor; then site median; then departure from
# the site's Landsat 7 (1999-2017) mean; then cross-site median.
obs_main <- readRDS(file.path(gr_dir, 'obs_main.rds'))
obs_tmetm <- readRDS(file.path(gr_dir, 'obs_tmetm.rds'))
l7_drift <- obs_tmetm[satellite == 'LANDSAT_7' & year > l7_last_year]
l7_drift[, ndvi.xcal := ndvi]
l7_drift[, sensor := 'LE7_drift']
obs <- rbind(obs_main[, .(site_code, sample.id, sensor, year, doy, ndvi, ndvi.xcal)],
             l7_drift[, .(site_code, sample.id, sensor, year, doy, ndvi, ndvi.xcal)])
obs <- obs[doy %in% 152:243]

by_sensor <- obs[, .(raw = median(ndvi), xcal = median(ndvi.xcal)), by = .(site_code, sample.id, sensor, year)][
    , .(raw = median(raw), xcal = median(xcal), n_px = .N), by = .(site_code, sensor, year)]
by_sensor <- by_sensor[n_px >= 5]
l7_ref <- by_sensor[sensor == 'LE7', .(ref = mean(raw)), by = site_code]
by_sensor <- by_sensor[l7_ref, on = 'site_code']
by_sensor <- melt(by_sensor, id.vars = c('site_code', 'sensor', 'year'), measure.vars = c('raw', 'xcal'),
                  variable.name = 'version')[l7_ref, on = 'site_code']
by_sensor[, dep := value - ref]
sensor_series <- by_sensor[, .(med = median(dep), n_sites = .N), by = .(sensor, version, year)][
    n_sites >= 0.5 * length(sites)]
sensor_series[, version := factor(version, c('raw', 'xcal'),
                                  c('(a) Uncalibrated', '(b) Calibrated to Landsat 7'))]

sensor_cols <- c(LT5 = '#1B9E77', LE7 = '#D95F02', LE7_drift = '#F4B183', LC8 = '#7570B3', LC9 = '#E7298A')
sensor_labs <- c(LT5 = 'Landsat 5 (TM)', LE7 = 'Landsat 7 (ETM+)',
                 LE7_drift = 'Landsat 7, 2018-2021 (orbit drift; not used)',
                 LC8 = 'Landsat 8 (OLI)', LC9 = 'Landsat 9 (OLI-2)')

overlap <- sensor_series[year %in% 2013:2017 & sensor %in% c('LE7', 'LC8'),
                         .(med = mean(med)), by = .(version, sensor)]
overlap <- dcast(overlap, version ~ sensor, value.var = 'med')[, gap := LC8 - LE7]
print(overlap)
ann <- overlap[, .(version, lab = sprintf('Landsat 8 minus Landsat 7, 2013-2017: %+.3f NDVI', gap))]

fig3 <- ggplot(sensor_series, aes(year, med, color = sensor)) +
    geom_hline(yintercept = 0, color = 'grey70') +
    geom_line() + geom_point(size = 1.4) +
    geom_text(data = ann, aes(x = 1984, y = 0.045, label = lab), inherit.aes = FALSE,
              hjust = 0, size = 3.3) +
    facet_wrap(~version, ncol = 1) +
    scale_color_manual(values = sensor_cols, labels = sensor_labs, breaks = names(sensor_cols)) +
    labs(x = 'Year', y = "Summer NDVI minus site's Landsat 7 mean",
         color = NULL,
         title = 'Summer (Jun-Aug) NDVI by sensor, cross-site median',
         subtitle = 'Each sensor summarized separately; where sensors overlap, calibrated values should coincide') +
    theme_bw() +
    theme(legend.position = 'bottom', panel.grid = element_blank(),
          strip.background = element_blank(), strip.text = element_text(hjust = 0, face = 'bold'))
for(od in out_dirs) ggsave(file.path(od, 'fig3_ndvi_by_sensor.png'), fig3, width = 10, height = 7, dpi = 200)
