# Nitrogen data viz script

#### README ####
# The following script will create sequential
# figures for use in Heili's 2024 AGU talk to
# discuss MacroSheds as a project as well as
# annual & monthly nitrogen trends

#### Load packages ####
library(here)
library(ggtext)
library(scales)
source(here('src', 'setup.R'))

#### Load data ####

# Annual
# All sites for which we could calculate annual VWM
n_annual_vwm <- readRDS("data_working/nitrogen_annual_VWM.rds")

# Sites that were actually used in annual-scale trend analyses
n_annual_vwm_filtered <- readRDS("data_working/nitrogen_annual_VWM_good.rds")

# And annual trends that were calculated.
n_annual_trends <- readRDS("data_working/nitrogen_annual_trends.rds")

# Including monthly data.
n_monthly_vwm <- readRDS("data_working/nitrogen_monthly_VWM.rds")

n_monthly_vwm_filtered <- readRDS("data_working/nitrogen_monthly_VWM_good.rds")

# As well as monthly trends.
n_monthly_trends <- readRDS("data_working/nitrogen_monthly_trends.rds")

#### Figures ####

##### HB only #####

# This is to illustrate the W6 record at Hubbard Brook, which
# many will be familiar with.
(fig1 <- ggplot(n_annual_vwm %>%
                    # create new column to delineate W6 @ HB
                    mutate(select = factor(case_when(site_code == "w6" ~ "YES",
                                                     TRUE ~ "NO"),
                                           levels = c("YES", "NO"))) %>%
                    filter(analyte == "nitrate_N") %>%
                    # removing outliers for plotting ease
                    filter(annual_vwm_mgL > 0.000001) %>%
                    filter(annual_vwm_mgL < 40),
                aes(x = water_year,
                    y = annual_vwm_mgL,
                    color = select)) +
     geom_line(linewidth = 1) +
     scale_color_manual(values = c("black", "transparent")) +
     labs(x = "Water Year", y = "Mean Annual VWM NO<sub>3</sub><sup>-</sup> (mg/L)") +
     scale_y_log10(labels = label_comma(accuracy = 0.0001)) +
     theme_bw() +
     theme(legend.position = "none",
           axis.title.y = element_markdown(),
           text = element_text(size = 20)))

# ggsave(fig1,
#        filename = "figures/agu_fig1.jpeg",
#        height = 20,
#        width = 35,
#        units = "cm")

##### All sites #####

site_count <- n_annual_vwm %>%
    filter(analyte == "nitrate_N") %>%
    select(site_code) %>%
    distinct()

# generating additional base year dataset
# so as to remove the connecting lines on the plot
years <- seq(from = 1964, to = 2024, by = 1)
yrs_rep <- rep(years, times = 183)
site_rep <- rep(site_count$site_code, each = 61)
full_site_years <- as.data.frame(cbind(yrs_rep, site_rep))

full_site_years <- full_site_years %>%
    rename(water_year = yrs_rep,
           site_code = site_rep) %>%
    mutate(water_year = as.numeric(water_year)) %>%
    mutate(analyte = "nitrate_N")

# This is the base plot that all others should be built
# around since it includes all possible sites (n = 183).
(fig2 <- ggplot(n_annual_vwm %>%
                    filter(analyte == "nitrate_N") %>%
                    # removing 4 outliers for plotting ease
                    filter(annual_vwm_mgL > 0.000001) %>%
                    filter(annual_vwm_mgL < 40) %>%
                    # add full site-years in to help with
                    # plotting lines properly
                    dplyr::right_join(full_site_years),
                aes(x = water_year,
                    y = annual_vwm_mgL)) +
     geom_line(aes(group = site_code),
               linewidth = 1, color = "black") +
     labs(x = "Water Year", y = "Mean Annual VWM NO<sub>3</sub><sup>-</sup> (mg/L)") +
     scale_y_log10(labels = label_comma(accuracy = 0.0001)) +
     theme_bw() +
     theme(axis.title.y = element_markdown(),
           text = element_text(size = 20)))

# ggsave(fig2,
#        filename = "figures/agu_fig2.jpeg",
#        height = 20,
#        width = 35,
#        units = "cm")

##### Remove exp. sites #####

n_annual_vwm <- n_annual_vwm %>%
    left_join(., ms_site_data)

site_counts_experimental <- n_annual_vwm %>%
    filter(analyte == "nitrate_N") %>%
    select(site_code, ws_status) %>%
    distinct() %>%
    count(ws_status) %>%
    ungroup()

# Color by experimental (n = 36) and
# non-experimental (n = 145) sites.
(fig3 <- ggplot(n_annual_vwm %>%
                    # removes Bonanza Creek sites only
                    drop_na(ws_status) %>%
                    filter(analyte == "nitrate_N") %>%
                    # removing outliers for plotting ease
                    filter(annual_vwm_mgL > 0.000001) %>%
                    filter(annual_vwm_mgL < 40) %>%
                    # add full site-years in to help with
                    # plotting lines properly
                    dplyr::right_join(full_site_years),
                aes(x = water_year,
                    y = annual_vwm_mgL,
                    group = site_code,
                    color = ws_status)) +
        geom_line(linewidth = 1) +
        scale_color_manual(values = c("transparent", "black")) +
        labs(x = "Water Year", y = "Mean Annual VWM NO<sub>3</sub><sup>-</sup> (mg/L)") +
        scale_y_log10(labels = label_comma(accuracy = 0.0001)) +
        theme_bw() +
        theme(axis.title.y = element_markdown(),
              text = element_text(size = 20),
              legend.position = "none"))

# ggsave(fig3,
#        filename = "figures/agu_fig3.jpeg",
#        height = 20,
#        width = 35,
#        units = "cm")

##### Trends #####

# This will include only sites with sufficient
# data for trends (i.e., min. 10 mos per year
# and 10 years within a 20 year timeframe, n = 26).

# First, to keep the same base plot we need to create a
# set of site-wys that passed our filters that we'll use
# to create a new column.
passed <- paste(n_annual_vwm_filtered$site_code,
                n_annual_vwm_filtered$water_year,
                sep = "_")

# Joining this with the site metadata df too to combine
# information about experimental/non-experimental sites
# and those that passed the filters in the same column.
n_annual_vwm <- n_annual_vwm %>%
    mutate(site_code_wy = paste(site_code, water_year, sep = "_")) %>%
    mutate(keep = factor(case_when(site_code_wy %in% passed ~ "YES",
                                    TRUE ~ "NO"),
                         levels = c("YES", "NO"))) %>%
    # also need to combine with experimental watershed filter
    mutate(keepkeep = factor(case_when(keep == "YES" &
                                       ws_status == "non-experimental" ~ "YES",
                                       TRUE ~ "NO"),
                             levels = c("YES", "NO")))

site_counts_sufficientdata <- n_annual_vwm %>%
    select(site_code, keepkeep) %>%
    distinct() %>%
    count(keepkeep) %>%
    ungroup()

(fig4 <- ggplot(n_annual_vwm %>%
                    filter(analyte == "nitrate_N")%>%
                    # removing outliers for plotting ease
                    filter(annual_vwm_mgL > 0.000001) %>%
                    filter(annual_vwm_mgL < 40) %>%
                    # add full site-years in to help with
                    # plotting lines properly
                    dplyr::right_join(full_site_years),
                aes(x = water_year,
                    y = annual_vwm_mgL,
                    color = keepkeep,
                    group = site_code)) +
     geom_line(linewidth = 1) +
     scale_color_manual(values = c("black", "transparent")) +
     labs(x = "Water Year",
          y = "Mean Annual VWM NO<sub>3</sub><sup>-</sup> (mg/L)") +
     scale_y_log10(labels = label_comma(accuracy = 0.0001)) +
     theme_bw() +
     theme(axis.title.y = element_markdown(),
           text = element_text(size = 20),
           legend.position = "none"))

# ggsave(fig4,
#        filename = "figures/agu_fig4.jpeg",
#        height = 20,
#        width = 35,
#        units = "cm")

# And now to actually color said trends.
# Pull out trends for NO3 at sites.
sites_trends <- n_annual_trends %>%
    filter(var == "nitrate_N") %>%
    select(site_code, group)

n_annual_vwm <- n_annual_vwm %>%
    left_join(., sites_trends) %>%
    # adding yet another column that combines the filters
    # of interest - passed/exp/sig
    mutate(keepkeepkeep = factor(case_when(keepkeep == "YES" &
                                               group == "sig. increasing" ~ "increasing",
                                           keepkeep == "YES" &
                                               group == "sig. decreasing" ~ "decreasing",
                                           keepkeep == "YES" &
                                               group == "no trend" ~ "no trend",
                                           TRUE ~ "remove"),
                                 levels = c("increasing",
                                            "decreasing",
                                            "no trend",
                                            "remove")))

site_counts_trends <- n_annual_vwm %>%
    select(site_code, keepkeepkeep) %>%
    distinct() %>%
    count(keepkeepkeep) %>%
    ungroup()

(fig5a <- ggplot(n_annual_vwm %>%
                    filter(analyte == "nitrate_N")%>%
                    # removing outliers for plotting ease
                    filter(annual_vwm_mgL > 0.000001) %>%
                    filter(annual_vwm_mgL < 40) %>%
                     # add full site-years in to help with
                     # plotting lines properly
                     dplyr::right_join(full_site_years),
                aes(x = water_year,
                    y = annual_vwm_mgL,
                    group = factor(site_code),
                    color = keepkeepkeep)) +
        geom_line(linewidth = 1) +
        scale_color_manual(values = c("#F48849FF",
                                      "#0D0887FF",
                                      "grey80",
                                     "transparent")) +
        labs(x = "Water Year",
             y = "Mean Annual VWM NO<sub>3</sub><sup>-</sup> (mg/L)") +
        scale_y_log10(labels = label_comma(accuracy = 0.0001)) +
        theme_bw() +
        theme(axis.title.y = element_markdown(),
              text = element_text(size = 20),
              legend.position = "none"))

# ggsave(fig5a,
#        filename = "figures/agu_fig5a.jpeg",
#        height = 20,
#        width = 35,
#        units = "cm")

sen <- function(..., weights = NULL) {
    mblm::mblm(...)
}

# Highlighting positive trends at BES sites.
(fig5a1 <- ggplot(n_annual_vwm %>%
                      filter(analyte == "nitrate_N")%>%
                      # removing outliers for plotting ease
                      filter(annual_vwm_mgL > 0.000001) %>%
                      filter(annual_vwm_mgL < 40) %>%
                      # add full site-years in to help with
                      # plotting lines properly
                      dplyr::right_join(full_site_years) %>%
                      mutate(width = case_when(site_code %in% c("BARN",
                                                                "MCDN") ~ "BES",
                                               TRUE ~ "Other")),
                  aes(x = water_year,
                      y = annual_vwm_mgL,
                      group = factor(site_code),
                      color = keepkeepkeep,
                      linewidth = width)) +
        geom_line() +
        scale_linewidth_manual(values = c(3, 1)) +
        scale_color_manual(values = c("#F48849FF",
                                      "#0D0887FF",
                                      "grey80",
                                      "transparent")) +
        # Add increasing trends at BES
        # geom_smooth(data = n_annual_vwm %>%
        # # create additional column to designate which lms show
        # mutate(model = factor(case_when(site_code %in% c("BARN", "MCDN") ~ "YES",
        #                                                 TRUE ~ "NO"),
        #                                       levels = c("YES", "NO"))) %>%
        #                 filter(model == "YES"),
        #             aes(x = water_year,
        #                 y = annual_vwm_mgL,
        #                 group = site_code),
        #             method = sen,
        #             se = F,
        #             color = "#F48849FF",
        #             linewidth = 2) +
        labs(x = "Water Year",
             y = "Mean Annual VWM NO<sub>3</sub><sup>-</sup> (mg/L)") +
        scale_y_log10(labels = label_comma(accuracy = 0.0001)) +
        theme_bw() +
        theme(axis.title.y = element_markdown(),
              text = element_text(size = 20),
              legend.position = "none"))

# ggsave(fig5a1,
#        filename = "figures/agu_fig5a1.jpeg",
#        height = 20,
#        width = 35,
#        units = "cm")

# Now, creating zoomed in version of only decreasing sites.
list_keep <- sites_trends %>%
    filter(group == "sig. decreasing")

list_keep <- list_remove$site_code

(fig5a_zoom <- ggplot(n_annual_vwm %>%
                      filter(analyte == "nitrate_N")%>%
                      # removing outliers for plotting ease
                      filter(annual_vwm_mgL > 0.000001) %>%
                      filter(annual_vwm_mgL < 40) %>%
                      filter(keepkeepkeep == "decreasing") %>%
                      # add full site-years in to help with
                      # plotting lines properly
                      dplyr::right_join(full_site_years),
                  aes(x = water_year,
                      y = annual_vwm_mgL,
                      group = factor(site_code))) +
        xlim(c(1964, 2024)) +
        geom_line(linewidth = 1, color = "#0D0887FF") +
        labs(x = "Water Year",
             y = "Mean Annual VWM NO<sub>3</sub><sup>-</sup> (mg/L)") +
        theme_bw() +
        theme(axis.title.y = element_markdown(),
              text = element_text(size = 20),
              legend.position = "none"))

# ggsave(fig5a_zoom,
#        filename = "figures/agu_fig5a_zoom.jpeg",
#        height = 20,
#        width = 35,
#        units = "cm")

# Highlighting negative trends at LUQ sites.
# And removing positive/no trend sites to help with interpretation.
(fig5a2 <- ggplot(n_annual_vwm %>%
                      filter(analyte == "nitrate_N")%>%
                      # removing outliers for plotting ease
                      filter(annual_vwm_mgL > 0.000001) %>%
                      filter(annual_vwm_mgL < 40) %>%
                      filter(keepkeepkeep == "decreasing") %>%
                      mutate(coloring = case_when(site_code %in% c("Q1","Q2","Q3") ~ "LUQ",
                                                  TRUE ~ "Other")) %>%
                      # add full site-years in to help with
                      # plotting lines properly
                      dplyr::right_join(full_site_years),
                  aes(x = water_year,
                      y = annual_vwm_mgL,
                      group = factor(site_code),
                      alpha = coloring,
                      linewidth = coloring)) +
        xlim(c(1964, 2024)) +
        geom_line(color = "#0D0887FF") +
        scale_alpha_manual(values = c(1, 0.2)) +
        scale_linewidth_manual(values = c(3, 1)) +
        labs(x = "Water Year",
             y = "Mean Annual VWM NO<sub>3</sub><sup>-</sup> (mg/L)") +
        theme_bw() +
        theme(axis.title.y = element_markdown(),
              text = element_text(size = 20),
              legend.position = "none"))

# ggsave(fig5a2,
#        filename = "figures/agu_fig5a2.jpeg",
#        height = 20,
#        width = 35,
#        units = "cm")

# Highlighting negative trends at HB sites.
(fig5a3 <- ggplot(n_annual_vwm %>%
                      filter(analyte == "nitrate_N")%>%
                      # removing outliers for plotting ease
                      filter(annual_vwm_mgL > 0.000001) %>%
                      filter(annual_vwm_mgL < 40) %>%
                      filter(keepkeepkeep == "decreasing") %>%
                  mutate(coloring = case_when(site_code %in% c("w3","w6","w7", "w8", "w9") ~ "HB",
                                              TRUE ~ "Other")) %>%
                      # add full site-years in to help with
                      # plotting lines properly
                      dplyr::right_join(full_site_years),
                  aes(x = water_year,
                      y = annual_vwm_mgL,
                      group = factor(site_code),
                      alpha = coloring,
                      linewidth = coloring)) +
        xlim(c(1964, 2024)) +
        geom_line(color = "#0D0887FF") +
        scale_alpha_manual(values = c(1, 0.2)) +
        scale_linewidth_manual(values = c(3, 1)) +
        labs(x = "Water Year",
             y = "Mean Annual VWM NO<sub>3</sub><sup>-</sup> (mg/L)") +
        theme_bw() +
        theme(axis.title.y = element_markdown(),
              text = element_text(size = 20),
              legend.position = "none"))

# ggsave(fig5a3,
#        filename = "figures/agu_fig5a3.jpeg",
#        height = 20,
#        width = 35,
#        units = "cm")

#### Monthly #####

site_count_monthly <- n_monthly_vwm %>%
    filter(analyte == "nitrate_N") %>%
    select(site_code) %>%
    distinct()

# generating additional base year dataset
# so as to remove the connecting lines on the plot
years <- seq(from = 1964, to = 2024, by = 1)
yrs_rep <- rep(years, times = 183)
site_rep <- rep(site_count_monthly$site_code, each = 61)
full_site_years <- as.data.frame(cbind(yrs_rep, site_rep))

full_site_years <- full_site_years %>%
    rename(year = yrs_rep,
           site_code = site_rep) %>%
    mutate(year = as.numeric(year)) %>%
    mutate(analyte = "nitrate_N")

# Also want to filter out unused data right off the bat.
n_monthly_vwm <- n_monthly_vwm %>%
    left_join(., ms_site_data)

# Create set of site-wys that passed our filters that we'll use
# to create a new column.
passed_m <- paste(n_monthly_vwm_filtered$site_code,
                n_monthly_vwm_filtered$year,
                sep = "_")

# Joining this with the site metadata df too to combine
# information about experimental/non-experimental sites
# and those that passed the filters in the same column.
n_monthly_vwm <- n_monthly_vwm %>%
    mutate(site_code_y = paste(site_code, year, sep = "_")) %>%
    mutate(keep = factor(case_when(site_code_y %in% passed_m ~ "YES",
                                   TRUE ~ "NO"),
                         levels = c("YES", "NO"))) %>%
    # also need to combine with experimental watershed filter
    mutate(keepkeep = factor(case_when(keep == "YES" &
                                           ws_status == "non-experimental" ~ "YES",
                                       TRUE ~ "NO"),
                             levels = c("YES", "NO")))

# Pull out trends for NO3 at sites.
sites_trends_m <- n_monthly_trends %>%
    filter(var == "nitrate_N") %>%
    select(site_code, group)

n_monthly_vwm <- n_monthly_vwm %>%
    left_join(., sites_trends_m) %>%
    # adding yet another column that combines the filters
    # of interest - passed/exp/sig
    mutate(keepkeepkeep = factor(case_when(keepkeep == "YES" &
                                               group == "sig. increasing" ~ "increasing",
                                           keepkeep == "YES" &
                                               group == "sig. decreasing" ~ "decreasing",
                                           keepkeep == "YES" &
                                               group %in% c("ns. increasing",
                                                            "ns. decreasing",
                                                            "no trend")~ "no trend",
                                           TRUE ~ "remove"),
                                 levels = c("increasing",
                                            "decreasing",
                                            "no trend",
                                            "remove")))

# This is the base plot that all others should be built
# around since it includes all possible sites (n = 185).
(fig_monthly <- ggplot(n_monthly_vwm %>%
                     filter(analyte == "nitrate_N")%>%
                     filter(month == 6) %>%
                     # removing outliers for plotting ease
                     filter(monthly_vwm_mgL > 0.000001),
                 aes(x = year,
                     y = monthly_vwm_mgL,
                     group = factor(site_code),
                     color = keepkeepkeep)) +
        geom_line(linewidth = 1) +
        scale_color_manual(values = c("#F48849FF",
                                      "#0D0887FF",
                                      "grey80",
                                      "transparent")) +
        labs(x = "Water Year",
             y = "Mean June VWM NO<sub>3</sub><sup>-</sup> (mg/L)") +
        scale_y_log10(labels = label_comma(accuracy = 0.001)) +
        theme_bw() +
        theme(axis.title.y = element_markdown(),
              text = element_text(size = 20),
              legend.position = "none"))

# ggsave(fig_monthly,
#        filename = "figures/agu_figmonthly_A.jpeg",
#        height = 20,
#        width = 35,
#        units = "cm")

# And alternatively adding all linear models to the figure
# for those with significant trend.
(fig5b <- ggplot(n_annual_vwm %>%
                     filter(analyte == "nitrate_N")%>%
                     # removing outliers for plotting ease
                     filter(annual_vwm_mgL > 0.000001) %>%
                     filter(annual_vwm_mgL < 40),
                 aes(x = water_year,
                     y = annual_vwm_mgL,
                     group = factor(site_code),
                     color = keepkeepkeep)) +
        geom_point(size = 2, shape = 21) +
        scale_fill_manual(values = c("#55C667FF",
                                     "#404788FF",
                                     "transparent",
                                     "transparent")) +
        geom_smooth(data = n_annual_vwm %>%
                        filter(keepkeepkeep %in% c("increasing",
                                                   "decreasing")),
                    aes(x = water_year,
                        y = annual_vwm_mgL,
                        group = site_code,
                        fill = keepkeepkeep,
                        color = keepkeepkeep),
                    method = "lm",
                    se = F) +
        scale_color_manual(values = c("#55C667FF",
                                      "#404788FF",
                                      "grey70",
                                      "transparent")) +
        labs(x = "Water Year", y = "Mean Annual VWM NO<sub>3</sub><sup>-</sup> (mg/L)") +
        scale_y_log10(labels = label_comma(accuracy = 0.0001)) +
        theme_bw() +
        theme(axis.title.y = element_markdown(),
              text = element_text(size = 20),
              legend.position = "none"))

# ggsave(fig5b,
#        filename = "figures/agu_fig5b.jpeg",
#        height = 20,
#        width = 35,
#        units = "cm")

##### HB Specific #####

# Creating a historical monthly dataset for HB data (1965-1975)
(fig_hb_past <- ggplot(n_monthly_vwm %>%
                           filter(analyte == "nitrate_N") %>%
                           filter(site_code == "w6") %>%
                           filter(year >= 1965) %>%
                           filter(year < 1975),
                       aes(x = factor(month),
                           y = monthly_vwm_mgL)) +
        geom_boxplot(fill = "#0D0887FF",
                     color = "#0D0887FF",
                     alpha = 0.2) +
        ylim(0, 1.25) +
        labs(x = "Month",
             y = "Monthly VWM NO<sub>3</sub><sup>-</sup> (mg/L)") +
     theme_bw() +
     theme(axis.title.y = element_markdown(),
           text = element_text(size = 20)))

(fig_hb_present <- ggplot(n_monthly_vwm %>%
                           filter(analyte == "nitrate_N") %>%
                           filter(site_code == "w6") %>%
                           filter(year >= 2010) %>%
                           filter(year < 2020),
                       aes(x = factor(month),
                           y = monthly_vwm_mgL)) +
        geom_boxplot(fill = "#0D0887FF",
                     color = "#0D0887FF",
                     alpha = 0.2) +
        ylim(0, 1.25) +
        labs(x = "Month",
             y = "Monthly VWM NO<sub>3</sub><sup>-</sup> (mg/L)") +
        theme_bw() +
        theme(axis.title.y = element_markdown(),
              text = element_text(size = 20)))

(fig_hb <- fig_hb_past + fig_hb_present)

# ggsave(fig_hb,
#        filename = "figures/agu_figHB.jpeg",
#        height = 16,
#        width = 40,
#        units = "cm")

#### Drivers ####

##### Temperature #####

# Load in climate data.
clim_raw <- read_feather(here('data_raw',
                          'spatial_timeseries_climate.feather'))
clim <- clim_raw %>%
    mutate(year = year(date),
           month = month(date),
           water_year = case_when(month %in% c(10, 11, 12) ~ year+1,
                                  TRUE ~ year)) %>%
    select(site_code, date, water_year, var, val) %>%
    pivot_wider(id_cols = c(site_code, date, water_year),
                names_from = var, values_from = val, values_fn = mean) %>%
    mutate(month = month(date))

# Create new columns to add to 41 sites for plotting.
sites_to_plot <- n_annual_vwm %>%
    filter(analyte == "nitrate_N") %>%
    filter(keepkeepkeep %in% c("decreasing",
                              "increasing",
                              "no trend")) %>%
    select(site_code, keepkeepkeep) %>%
    unique()

my41sites <- sites_to_plot$site_code

# Filter climate dataset for sites of interest
clim_trim_annual <- clim %>%
    filter(site_code %in% my41sites) %>%
    group_by(site_code, water_year) %>%
    summarize(mean_ann_temp = mean(temp_median, na.rm = TRUE),
              sum_N_dep = sum(N_flux_mean, na.rm = TRUE)) %>%
    ungroup() %>%
    full_join(sites_to_plot)

(fig6 <- ggplot(clim_trim_annual %>%
                    # filter out strange final year
                    filter(water_year < 2023) %>%
                    mutate(keepkeepkeep = factor(keepkeepkeep)),
                 aes(x = water_year,
                     y = mean_ann_temp,
                     group = factor(site_code),
                     color = keepkeepkeep)) +
        geom_line(linewidth = 1) +
        scale_color_manual(values = c("#F48849FF",
                                      "#0D0887FF",
                                      "grey80",
                                      "transparent")) +
        labs(x = "Water Year",
             y = "Mean Annual Temperature (C)") +
        theme_bw() +
        facet_grid(keepkeepkeep~.) +
        theme(axis.title.y = element_markdown(),
              text = element_text(size = 20),
              legend.position = "none",
              strip.background = element_blank(),
              strip.text.y = element_blank()))

# ggsave(fig6,
#        filename = "figures/agu_fig6.jpeg",
#        height = 25,
#        width = 15,
#        units = "cm")

##### Deposition #####

# Need to make new dataset for deposition since it's
# already annualized.
clim_dep <- clim_raw %>%
    filter(var == "N_flux_mean") %>%
    select(site_code, year, var, val) %>%
    pivot_wider(id_cols = c(site_code, year),
                names_from = var, values_from = val, values_fn = mean) %>%
    filter(site_code %in% my41sites) %>%
    full_join(sites_to_plot)

(fig7 <- ggplot(clim_dep %>%
                    mutate(keepkeepkeep = factor(keepkeepkeep)),
                aes(x = year,
                    y = N_flux_mean,
                    group = factor(site_code),
                    color = keepkeepkeep)) +
     geom_line(linewidth = 1) +
     scale_color_manual(values = c("#F48849FF",
                                   "#0D0887FF",
                                   "grey80",
                                   "transparent")) +
     labs(x = "Year",
          y = "Cumulative Annual N Deposition (kg/ha)") +
     theme_bw() +
     facet_grid(keepkeepkeep~.) +
     theme(axis.title.y = element_markdown(),
           text = element_text(size = 20),
           legend.position = "none",
           strip.background = element_blank(),
           strip.text.y = element_blank()))

# ggsave(fig7,
#        filename = "figures/agu_fig7.jpeg",
#        height = 25,
#        width = 15,
#        units = "cm")

##### Productivity #####

prod_raw <- read_feather(here('data_raw',
                              'spatial_timeseries_vegetation.feather'))

# Filter productivity dataset for sites of interest
prod <- prod_raw %>%
    mutate(year = year(date),
           month = month(date),
           water_year = case_when(month %in% c(10, 11, 12) ~ year+1,
                                  TRUE ~ year)) %>%
    select(site_code, date, water_year, var, val) %>%
    pivot_wider(id_cols = c(site_code, date, water_year),
                names_from = var, values_from = val, values_fn = mean) %>%
    mutate(month = month(date))

prod_trim_annual <- prod %>%
    filter(site_code %in% my41sites) %>%
    group_by(site_code, water_year) %>%
    summarize(sum_ann_prod = sum(gpp_CONUS_30m_median, na.rm = TRUE)) %>%
    ungroup() %>%
    full_join(sites_to_plot)

(fig8 <- ggplot(prod_trim_annual %>%
                    # remove weird final years
                    filter(water_year < 2021) %>%
                    # and need to remove LUQ sites
                    # where we don't have GPP
                    filter(sum_ann_prod > 0) %>%
                    mutate(keepkeepkeep = factor(keepkeepkeep)),
                aes(x = water_year,
                    y = sum_ann_prod,
                    group = factor(site_code),
                    color = keepkeepkeep)) +
        geom_line(linewidth = 1) +
        scale_color_manual(values = c("#F48849FF",
                                      "#0D0887FF",
                                      "grey80",
                                      "transparent")) +
        labs(x = "Water Year",
             y = "Cumulative Annual GPP (kg C/m<sup>2</sup>)") +
        theme_bw() +
        facet_grid(keepkeepkeep~.) +
        theme(axis.title.y = element_markdown(),
              text = element_text(size = 20),
              legend.position = "none",
              strip.background = element_blank(),
              strip.text.y = element_blank()))

# ggsave(fig8,
#        filename = "figures/agu_fig8.jpeg",
#        height = 25,
#        width = 15,
#        units = "cm")

# End of script.
