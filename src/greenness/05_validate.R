# Validation of the calibrated greenness series.
#
# 1. CONUS-wide annual anomaly series: Robinson GPP vs raw vs calibrated vs TM/ETM+-only.
# 2. Per-site step at the ETM+ -> OLI transition, relative to the extrapolated
#    2003-2012 trend, with a placebo break (2005) for comparison.
# 3. Direct test: during 2013-2017, both the calibrated series (L7 + L8) and the
#    TM/ETM+-only series (L7 alone) exist. Calibrated - TM/ETM+ should be ~0 before and
#    after 2013; raw - TM/ETM+ should jump at 2013.
# 4. LandsatTS cross-calibration diagnostics (collected from 03_*).
# 5. Sensitivity: Sen's slopes from calibrated vs TM/ETM+-only series, 1984-2017
#    (pre-drift, like for like) and 1984-2021 (as specified; L7 drift years included).

source(here::here('src', 'greenness', '00_config.R'))
suppressMessages(source(here::here('src', 'setup.R'))) # detect_trends(), add_flags()

ga <- readRDS(file.path(gr_dir, 'greenness_annual.rds'))
sites <- unique(ga$site_code)

gpp <- feather::read_feather(here('data_raw', 'ms', 'v2', 'spatial_timeseries_vegetation.feather')) %>%
    filter(var == 'gpp_CONUS_30m_median', site_code %in% sites) %>%
    mutate(water_year = water_year(date)) %>%
    group_by(site_code, water_year) %>%
    summarize(val = mean(val, na.rm = TRUE), .groups = 'drop') %>%
    mutate(var = 'gpp_robinson')

d <- bind_rows(select(ga, -n_pts), gpp)

rel_anom <- function(df, base = 2001:2012){
    df %>%
        group_by(site_code, var) %>%
        mutate(rel = val / mean(val[water_year %in% base], na.rm = TRUE) - 1) %>%
        ungroup()
}

# 1. CONUS anomaly series ####
series_cols <- c(gpp_robinson = 'grey40', ndvi_gs_raw = '#D55E00',
                 ndvi_gs_xcal = '#0072B2', ndvi_gs_tmetm = '#009E73')
# legend labels. TM = Landsat 5, ETM+ = Landsat 7, OLI = Landsat 8 (and OLI-2 = Landsat 9)
series_labs <- c(gpp_robinson = 'Robinson GPP (current)',
                 ndvi_gs_raw = 'NDVI, raw (L5 + L7 + L8/9, uncalibrated)',
                 ndvi_gs_xcal = 'NDVI, calibrated (L5 + L7 + L8/9, on L7 scale)',
                 ndvi_gs_tmetm = 'NDVI, L5 + L7 only (no OLI)',
                 nirv_gs_raw = 'NIRv, raw (L5 + L7 + L8/9, uncalibrated)',
                 nirv_gs_xcal = 'NIRv, calibrated (L5 + L7 + L8/9, on L7 scale)',
                 nirv_gs_tmetm = 'NIRv, L5 + L7 only (no OLI)')

conus <- d %>%
    filter(var %in% names(series_cols)) %>%
    rel_anom() %>%
    group_by(var, water_year) %>%
    summarize(med = median(rel, na.rm = TRUE),
              q25 = quantile(rel, 0.25, na.rm = TRUE),
              q75 = quantile(rel, 0.75, na.rm = TRUE),
              n = n(), .groups = 'drop')

p1 <- ggplot(conus, aes(water_year, med, color = var, fill = var)) +
    annotate('rect', xmin = 2012.5, xmax = 2013.5, ymin = -Inf, ymax = Inf, alpha = 0.15) +
    geom_hline(yintercept = 0, color = 'grey70') +
    geom_ribbon(aes(ymin = q25, ymax = q75), alpha = 0.12, color = NA) +
    geom_line(linewidth = 0.8) +
    scale_color_manual(values = series_cols, labels = series_labs) +
    scale_fill_manual(values = series_cols, labels = series_labs) +
    labs(x = 'Water year', y = 'Relative anomaly (vs. 2001-2012 mean)',
         color = NULL, fill = NULL,
         title = 'Median across watersheds (IQR shaded); OLI era begins 2013') +
    theme_bw()
ggsave(file.path(gr_fig_dir, 'val_conus_anomaly_series.png'), p1, width = 9, height = 5)

# same, NIRv
conus_nirv <- d %>%
    filter(var %in% c('gpp_robinson', 'nirv_gs_raw', 'nirv_gs_xcal', 'nirv_gs_tmetm')) %>%
    rel_anom() %>%
    group_by(var, water_year) %>%
    summarize(med = median(rel, na.rm = TRUE), .groups = 'drop')
p1b <- ggplot(conus_nirv, aes(water_year, med, color = var)) +
    geom_vline(xintercept = 2013, linetype = 'dashed') +
    geom_line(linewidth = 0.8) +
    scale_color_discrete(labels = series_labs) +
    labs(x = 'Water year', y = 'Relative anomaly (vs. 2001-2012 mean)', color = NULL) +
    theme_bw()
ggsave(file.path(gr_fig_dir, 'val_conus_anomaly_series_nirv.png'), p1b, width = 9, height = 5)

# 2. per-site step ####
step_test <- function(df, break_yr, pre_n = 10, post_n = 4){
    df %>%
        group_by(var, site_code) %>%
        group_modify(~{
            pre <- filter(.x, water_year %in% (break_yr - pre_n):(break_yr - 1))
            post <- filter(.x, water_year %in% (break_yr + 1):(break_yr + post_n))
            if(nrow(pre) < pre_n - 2 || nrow(post) < post_n - 1) return(tibble())
            m <- lm(val ~ water_year, pre)
            tibble(rel_step = mean(post$val - predict(m, post)) / mean(pre$val))
        }) %>%
        ungroup() %>%
        mutate(break_yr = break_yr)
}

steps <- bind_rows(step_test(d, 2013), step_test(d, 2005))
step_summary <- steps %>%
    group_by(var, break_yr) %>%
    summarize(n_sites = n(),
              median_rel_step = median(rel_step),
              frac_positive = mean(rel_step > 0),
              wilcox_p = wilcox.test(rel_step)$p.value,
              .groups = 'drop') %>%
    arrange(break_yr, var)
print(step_summary, n = Inf)
write.csv(step_summary, file.path(gr_dir, 'val_step_summary.csv'), row.names = FALSE)

p2 <- steps %>%
    filter(grepl('_gs_|gpp', var)) %>%
    ggplot(aes(var, rel_step, fill = factor(break_yr))) +
    geom_hline(yintercept = 0) +
    geom_boxplot(outlier.size = 0.8) +
    scale_fill_manual(values = c('2005' = 'grey80', '2013' = '#56B4E9'),
                      labels = c('2005 (placebo)', '2013 (L7 ETM+ -> L8 OLI)')) +
    labs(x = NULL, y = 'Step vs. extrapolated pre-break trend (fraction of mean)', fill = 'Break') +
    theme_bw() + theme(axis.text.x = element_text(angle = 30, hjust = 1))
ggsave(file.path(gr_fig_dir, 'val_step_boxplots.png'), p2, width = 9, height = 5)

# 3. calibrated / raw minus TM/ETM+-only, by year ####
direct <- ga %>%
    filter(grepl('_gs_', var)) %>%
    separate(var, c('si', 'metric', 'series'), sep = '_') %>%
    select(-n_pts) %>%
    pivot_wider(names_from = series, values_from = val) %>%
    filter(water_year <= 2017) %>% # L7 pre-drift
    mutate(xcal_minus_tmetm = xcal - tmetm, raw_minus_tmetm = raw - tmetm) %>%
    pivot_longer(c(xcal_minus_tmetm, raw_minus_tmetm), names_to = 'comparison', values_to = 'diff') %>%
    group_by(si, comparison, water_year) %>%
    summarize(med = median(diff, na.rm = TRUE),
              q25 = quantile(diff, 0.25, na.rm = TRUE),
              q75 = quantile(diff, 0.75, na.rm = TRUE), .groups = 'drop')

p3 <- ggplot(direct, aes(water_year, med, color = comparison, fill = comparison)) +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 2012.5, linetype = 'dashed') +
    geom_ribbon(aes(ymin = q25, ymax = q75), alpha = 0.15, color = NA) +
    geom_line() +
    scale_color_discrete(labels = c(raw_minus_tmetm = 'raw - (L5 + L7 only)',
                                    xcal_minus_tmetm = 'calibrated - (L5 + L7 only)'),
                         aesthetics = c('color', 'fill')) +
    facet_wrap(~si, scales = 'free_y') +
    labs(x = 'Water year', y = 'Difference from TM/ETM+-only series (index units)',
         title = 'Calibrated should stay ~0 across 2013; raw should jump') +
    theme_bw()
ggsave(file.path(gr_fig_dir, 'val_direct_vs_tmetm.png'), p3, width = 10, height = 4.5)

direct_summary <- direct %>%
    mutate(period = ifelse(water_year >= 2013, '2013-2017', '1999-2012')) %>%
    filter(water_year >= 1999) %>%
    group_by(si, comparison, period) %>%
    summarize(median_diff = median(med), .groups = 'drop')
print(direct_summary)
write.csv(direct_summary, file.path(gr_dir, 'val_direct_summary.csv'), row.names = FALSE)

# 4. LandsatTS diagnostics ####
xcal_eval <- list.files(gr_fig_dir, pattern = '_xcal_rf_eval\\.csv$', recursive = TRUE, full.names = TRUE)
xcal_eval <- bind_rows(lapply(xcal_eval, function(f) read.csv(f) %>% mutate(run = basename(dirname(f)))))
print(xcal_eval)
write.csv(xcal_eval, file.path(gr_dir, 'val_xcal_rf_eval.csv'), row.names = FALSE)
# scatterplots: figures/greenness/xcal_*/*_xval_pred_vs_obs.jpg

# 5. trend sensitivity ####
trend_compare <- function(yrs){
    d %>%
        filter(var %in% c('ndvi_gs_xcal', 'ndvi_gs_tmetm', 'ndvi_gs_raw', 'gpp_robinson'),
               water_year %in% yrs) %>%
        select(site_code, water_year, var, val) %>%
        detect_trends() %>%
        add_flags() %>%
        mutate(period = paste(range(yrs), collapse = '-'))
}

trends_sens <- bind_rows(trend_compare(first_year:2017), trend_compare(first_year:2021))
write.csv(trends_sens, file.path(gr_dir, 'val_trend_sensitivity.csv'), row.names = FALSE)

flag_table <- trends_sens %>% count(period, var, flag) %>%
    pivot_wider(names_from = flag, values_from = n, values_fill = 0)
print(flag_table)

sens_wide <- trends_sens %>%
    select(period, site_code, var, trend, flag) %>%
    pivot_wider(names_from = var, values_from = c(trend, flag))

agree <- sens_wide %>%
    group_by(period) %>%
    summarize(n = n(),
              spearman_xcal_tmetm = cor(trend_ndvi_gs_xcal, trend_ndvi_gs_tmetm, method = 'spearman', use = 'complete.obs'),
              sign_agree = mean(sign(trend_ndvi_gs_xcal) == sign(trend_ndvi_gs_tmetm), na.rm = TRUE),
              flag_agree = mean(flag_ndvi_gs_xcal == flag_ndvi_gs_tmetm, na.rm = TRUE))
print(agree)
write.csv(agree, file.path(gr_dir, 'val_trend_agreement.csv'), row.names = FALSE)

p5 <- ggplot(sens_wide, aes(trend_ndvi_gs_tmetm, trend_ndvi_gs_xcal)) +
    geom_abline(linetype = 'dashed') +
    geom_hline(yintercept = 0, color = 'grey70') + geom_vline(xintercept = 0, color = 'grey70') +
    geom_point(alpha = 0.6) +
    facet_wrap(~period) +
    labs(x = "Sen's slope, L5 TM + L7 ETM+ only (NDVI/yr)", y = "Sen's slope, calibrated L5 + L7 + L8/9 (NDVI/yr)") +
    theme_bw()
ggsave(file.path(gr_fig_dir, 'val_trend_xcal_vs_tmetm.png'), p5, width = 9, height = 4.5)
