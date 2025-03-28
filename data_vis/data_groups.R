# clear environment
#rm(list = ls())
# Load packages.
library(here)
source(here('src', 'setup.R'))
#source(here('src', 'mega_zipper_data.R'))
flag_colors <- c('increasing' = "red", 'decreasing' = 'blue', 'flat' = 'green', 'non-significant' = "grey", 'insufficient data' = 'black')

target_q_trend <- "q_mean"

q_trends <- read_csv(here('data_working', 'trends', 'best_run_prisim.csv')) %>%
    add_flags()%>%
    filter(
        var == target_q_trend
        ) %>%
    select(site_code, q_trend = trend, q_flag = flag)


# read in data####
full_prism_trends <- read_csv(here('data_working', 'trends', 'full_prisim_climate.csv')) %>%
    add_flags() %>%
    select(site_code, var, trend, flag) %>%
    pivot_wider(id_cols = site_code, values_from = c(trend, flag), names_from = var) %>%
    mutate(wetting = case_when(flag_precip_mean == 'increasing' ~ 'W',
                               flag_precip_mean == 'decreasing' ~ 'D',
                               flag_precip_mean == 'non-significant' ~ 'N'),
           warming = case_when(flag_temp_mean == 'increasing' ~ 'H',
                               flag_temp_mean == 'decreasing' ~ 'C',
                               flag_temp_mean == 'non-significant' ~ 'N'),
           greening = case_when(flag_gpp_CONUS_30m_median == 'increasing' ~ 'G',
                                flag_gpp_CONUS_30m_median == 'decreasing' ~ 'B',
                                flag_gpp_CONUS_30m_median == 'non-significant' ~ 'N',
                                flag_gpp_CONUS_30m_median == NA ~ 'N'),
           grouping = as.factor(paste0(warming, wetting, greening))
           ) %>%
    left_join(., ms_site_data, by = 'site_code') %>%
    mutate(grouping_exp = case_when(ws_status == 'experimental' ~ 'EXP',
                                .default = 'NON')) %>%
    left_join(., q_trends, by = 'site_code')

full_prism_trends$grouping <- factor(full_prism_trends$grouping, levels = c('HDG', #strong down
                                                                            'HDN', 'HNG', 'NDG', # mid down
                                                                            'HDB', 'HNN', 'NNG', 'NDN', # light down
                                                                            'NNN', # no change
                                                                            'HWG', 'HWN', 'HNB', 'NDB', 'NWG', # variable
                                                                            'NNB', 'NWN', #light up
                                                                            'NWB')) # strong up

full_prism_trends %>%
    #select(site_code, grouping, streamflow) %>%
    write_csv(here('data_working', 'site_groupings_by_prsim_trend.csv'))
# plots ####
# figure 2 for the paper
# make plot of warming on x, wetting on y, and gpp as point size or color

gpp_plot <- ggplot(full_prism_trends, aes(x = trend_temp_mean, y = trend_precip_mean, text = paste("Site:", site_code, "<br>Domain:", domain))) +
    # Points with 'non-significant' flag
    geom_point(data = subset(full_prism_trends, flag_gpp_CONUS_30m_median == "non-significant"),
               color = "grey", size = 10) +
    # Points with other flags
    geom_point(data = subset(full_prism_trends, flag_gpp_CONUS_30m_median != "non-significant"),
               aes(color = trend_gpp_CONUS_30m_median), size = 10) +
    scale_color_distiller(palette = 'BrBG', direction = 1) +
    theme_few(base_size = 20) +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 0) +
    labs(x = 'Temperature trend (mean annual, degrees C)',
         y = 'Precipitation trend (mean annual, mm)',
         color = 'GPP trend (mean annual)')

library(plotly)
ggplotly(gpp_plot, tooltip = 'text')


limit <- max(abs(full_prism_trends$q_trend), na.rm = T) * c(-1, 1)

q_plot <- ggplot(full_prism_trends, aes(x = trend_temp_mean, y = trend_precip_mean, text = paste("Site:", site_code, "<br>Domain:", domain))) +
    # Points with 'non-significant' flag
    geom_point(data = subset(full_prism_trends, q_flag == "non-significant"),
               color = "grey", size = 10) +
    # Points with other flags
    geom_point(data = subset(full_prism_trends, q_flag != "non-significant"),
               aes(color = q_trend), size = 10) +
    scale_color_distiller(palette = 'RdBu', direction = 1, limit = limit) +
    theme_few(base_size = 20) +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 0) +
    labs(x = 'Temperature trend (mean annual, degrees C)',
         y = 'Precipitation trend (mean annual, mm)',
         color = 'Q trend (mean annual)')

ggplotly(q_plot, tooltip = 'text')



## bar charts  by group ####
ggplot(full_prism_trends, aes(x = grouping, fill = streamflow))+
    geom_bar()+
    theme_few(base_size = 20)+
    scale_fill_manual(values = flag_colors)+#,
    labs(fill = target_q_trend)

ggplot(full_prism_trends, aes(x = grouping, fill = streamflow))+
    geom_bar()+
    theme_few(base_size = 20)+
    theme(axis.text.x = element_text(angle = 45, vjust = .5, hjust = .5))+
    scale_fill_manual(values = flag_colors)+#,
    facet_wrap(~grouping_exp) +
    labs(fill = target_q_trend)


full_prism_trends %>%
    filter(grouping_exp == 'EXP',
           streamflow == 'decreasing') %>%
    select(domain, site_code)

full_prism_trends %>%
    filter(grouping_exp == 'NON',
           streamflow == 'increasing') %>%
    select(domain)

full_prism_trends %>%
    filter(grouping_exp == 'NON',
           streamflow == 'decreasing') %>%
    select(domain)

View(full_prism_trends %>%
    filter(grouping == 'NNN',
           streamflow == 'decreasing') %>%
    select(domain, site_code, ws_status))


metrics <- readRDS(here('data_working', 'discharge_metrics_siteyear.RDS')) %>%
    distinct()

metrics %>%
    filter(site_code == 'GSWS06', water_year > 1980) %>%
    ggplot(aes(x = water_year, y = gpp_CONUS_30m_median))+
    geom_point()

## hydrographs ####
q_data <- ms_load_product(prodname = 'discharge', macrosheds_root = my_ms_dir)

group_counts <- full_prism_trends %>%
    filter(streamflow != 'data limited') %>%
    group_by(grouping) %>%
    summarize(n = n())

options(scipen = 999) # turn off scientific notation
q_data %>%
    right_join(., full_prism_trends, by = 'site_code') %>%
    right_join(., group_counts) %>%
    filter(val != 0) %>%
    mutate(facet_lab = paste0(grouping, ', n=', n)) %>%
    ggplot(aes(x = date, y = val, color = site_code))+
        geom_line()+
        scale_y_log10()+
    #geom_label_repel(aes(label = domain))+
    theme_few(base_size = 20)+
    scale_color_viridis(discrete = T)+
    theme(#axis.text = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          legend.position = 'none',
          )+
    facet_wrap(~facet_lab, scales = 'fixed')





