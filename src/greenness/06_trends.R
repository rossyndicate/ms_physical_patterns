# Driver trends with calibrated greenness alongside Robinson GPP.
#
# Mirrors src/mega_zipper_data.R (clim_trends): annual values -> longest
# continuous temp_mean run per site -> detect_trends() (zyp Sen's slope) ->
# add_flags() (CI excludes 0). Greenness variables are added to the same long
# table, so they receive the identical window and filters as temp, precip, and GPP.
#
# Output data_working/trends/full_prisim_climate_greenness.csv has the same
# columns as full_prisim_climate.csv (read at src/nitrogen_figures.R:86), plus rows
# for the greenness variables. Swapping it into the figures is a separate step.

source(here::here('src', 'greenness', '00_config.R'))
suppressMessages(source(here::here('src', 'setup.R')))

greenness_vars <- c('ndvi_gs_xcal', 'nirv_gs_xcal', 'ndvi_max_xcal', 'ndvi_gs_raw')

metrics <- readRDS(here('data_working', 'discharge_metrics_siteyear_nTest.rds')) %>%
    distinct()

greenness <- readRDS(file.path(gr_dir, 'greenness_annual.rds')) %>%
    filter(var %in% greenness_vars) %>%
    mutate(agg_code = 'annual') %>%
    select(site_code, water_year, agg_code, var, val)

clim_long <- metrics %>%
    select(-contains('date'), -source) %>%
    pivot_longer(cols = -c('site_code', 'water_year', 'agg_code'),
                 names_to = 'var', values_to = 'val') %>%
    filter(var %in% c('temp_mean', 'precip_mean', 'gpp_CONUS_30m_median'),
           agg_code == 'annual') %>%
    bind_rows(greenness) %>%
    distinct()

clim_trends_gr <- clim_long %>%
    reduce_to_longest_site_runs(., metric = 'temp_mean') %>%
    detect_trends(.)

write_csv(clim_trends_gr, here('data_working', 'trends', 'full_prisim_climate_greenness.csv'))

# greening / browning counts ####
nonexp_conus <- ms_site_data %>%
    filter(ws_status == 'non-experimental',
           latitude > 24, latitude < 50, longitude > -125, longitude < -66) %>%
    pull(site_code)

no3_trends <- readRDS(here('data_working', 'no3_trends_annual.rds'))
no3_declining <- no3_trends %>% filter(flag == 'decreasing') %>% pull(site_code)

veg <- clim_trends_gr %>%
    filter(var %in% c('gpp_CONUS_30m_median', greenness_vars),
           site_code %in% nonexp_conus,
           code == 'good') %>%
    add_flags()

# only sites that have both GPP and calibrated NDVI trends, so counts compare like for like
common <- veg %>%
    filter(var %in% c('gpp_CONUS_30m_median', 'ndvi_gs_xcal')) %>%
    count(site_code) %>%
    filter(n == 2) %>%
    pull(site_code)

count_table <- function(df, label){
    df %>%
        filter(site_code %in% common) %>%
        group_by(var) %>%
        summarize(n_sites = n(),
                  positive_slope = sum(trend > 0),
                  negative_slope = sum(trend < 0),
                  greening_sig = sum(flag == 'increasing'),
                  browning_sig = sum(flag == 'decreasing'),
                  nonsig = sum(flag == 'non-significant'),
                  .groups = 'drop') %>%
        mutate(subset = label, .before = 1)
}

counts <- bind_rows(count_table(veg, 'all non-exp CONUS'),
                    count_table(filter(veg, site_code %in% no3_declining), 'NO3-N declining'))
print(counts, width = Inf)
write.csv(counts, file.path(gr_dir, 'greening_counts.csv'), row.names = FALSE)

# site-level transitions, GPP flag -> calibrated NDVI flag
transitions <- veg %>%
    filter(site_code %in% common, var %in% c('gpp_CONUS_30m_median', 'ndvi_gs_xcal')) %>%
    select(site_code, var, flag) %>%
    pivot_wider(names_from = var, values_from = flag) %>%
    mutate(no3_declining = site_code %in% no3_declining) %>%
    count(no3_declining, gpp = gpp_CONUS_30m_median, ndvi_xcal = ndvi_gs_xcal)
print(transitions, n = Inf)
write.csv(transitions, file.path(gr_dir, 'greening_transitions.csv'), row.names = FALSE)
