# Nitrogen data trends script

#### README ####
# The following script will estimate trends
# for nitrogen data. It will also create
# accompanying data visualizations.

#### Load packages ####
library(here)
source(here('src', 'setup.R'))

#### Load data ####

# Annual volume weighted means
n_annual <- readRDS("data_working/nitrogen_annual_VWM_good.rds")

# Monthly volume weighted means
n_monthly <- readRDS("data_working/nitrogen_monthly_VWM_good.rds")

# Discharge
q_data <- ms_load_product(
    macrosheds_root = here(my_ms_dir),
    prodname = "discharge",
    warn = F
)

#### Trends ####

##### Annual #####

# Renaming a column to work with the function below.
n_annual <- n_annual %>%
    rename(var = analyte) %>%
    rename(val = annual_vwm_mgL)

# Now, within each dataframe, calculate and combine sens slope results.
# Repurposing the "detect_trends" function here from the setup script.
detect_trends <- function(df_in){

    com_long <- df_in

    #  make trend dataset for entire record
    out_frame <- tibble(site_code = as.character(),
                        var = as.character(),
                        start = as.integer(),
                        end = as.integer(),
                        median = as.integer(),
                        trend = as.integer(),
                        p = as.integer(),
                        code = as.character())

    for(i in unique(com_long$site_code)){

        target_site <- filter(com_long, site_code == i)

            for(j in unique(target_site$var)){

                target_solute <- filter(target_site, var == j)  %>%
                    arrange(water_year) %>%
                    distinct() %>%
                    na.omit()

                    start <- min(target_solute$water_year)
                    end <- max(target_solute$water_year)
                    median <- median(target_solute$val)
                    n <- nrow(target_solute)

                    slope_data <- target_solute %>%
                        select(val) %>%
                        as.ts()

                    # assigns water years to rownames to properly index
                    rownames(slope_data) <- target_solute$water_year
                    test <- sens.slope(slope_data)
                    trend <- test[[1]]
                    p <- test[[3]]

                    inner <- tibble(site_code = i,
                                    var = j,
                                    start = start,
                                    end = end,
                                    median = median,
                                    n = n,
                                    trend = trend,
                                    p = p
                    )

                    # bind out
                    out_frame <- rbind(out_frame, inner)

            }# end solute loop

    } #end site loop

    return(out_frame)

} #end function

n_annual_trends <- detect_trends(n_annual)

# Add columns for identification.
n_annual_trends <- n_annual_trends %>%
    mutate(sig = case_when(p < 0.05 ~ 1,
                           TRUE ~ 0)) %>%
    mutate(group = factor(case_when(sig == 1 & trend > 0 ~ "sig. increasing",
                                    sig == 0 & trend > 0 ~ "ns. increasing",
                                    sig == 1 & trend < 0 ~ "sig. decreasing",
                                    sig == 0 & trend < 0 ~ "ns. decreasing",
                                    TRUE ~ "no trend"),
                          levels = c("sig. increasing", "ns. increasing",
                                     "no trend",
                                     "ns. decreasing", "sig. decreasing")))

# Export for use in making figures.
# saveRDS(n_annual_trends, "data_working/nitrogen_annual_trends.rds")

n_annual_trends_summary <- n_annual_trends %>%
    count(var, group) %>%
    ungroup()

(fig_annual <- ggplot(n_annual_trends_summary,
                       aes(x = var, y = n, fill = group)) +
        scale_fill_manual(values = c("red", "gray", "blue")) +
        geom_bar(position = "stack", stat = "identity") +
        labs(x = "Analyte", y = "Site Count", fill = "Trend") +
        theme_bw())

# ggsave(fig_annual,
#        filename = "figures/n_vwm_annual_trends.jpg",
#        width = 12,
#        height = 8,
#        units = "cm")

##### Monthly #####

# Formatting columns to work with the function below.
n_monthly <- n_monthly %>%
    rename(var = analyte) %>%
    rename(val = monthly_vwm_mgL) %>%
    mutate(water_year = case_when(month %in% c(10, 11, 12) ~ year+1,
                                  TRUE ~ year))

# Re-repurposing the "detect_trends" function here from the setup script.
detect_monthly_trends <- function(df_in){

    com_long <- df_in

    #  make trend dataset for entire record
    out_frame <- tibble(site_code = as.character(),
                        var = as.character(),
                        month = as.integer(),
                        start = as.integer(),
                        end = as.integer(),
                        trend = as.integer(),
                        p = as.integer(),
                        code = as.character())

    for(i in unique(com_long$site_code)){ # for each site

        target_site <- filter(com_long, site_code == i)

        for(j in unique(target_site$var)){ # and each analyte

            target_solute <- filter(target_site, var == j)

                for(k in unique(target_solute$month)){ # and each month

                    target_month <- filter(target_solute, month == k) %>%
                        arrange(water_year) %>%
                        distinct() %>%
                        na.omit()

                    start <- min(target_month$water_year)
                    end <- max(target_month$water_year)
                    n <- nrow(target_month)

                    slope_data <- target_month %>%
                        select(val) %>%
                        as.ts()

                    # assigns water years to rownames to properly index
                    rownames(slope_data) <- target_month$water_year
                    test <- sens.slope(slope_data)
                    trend <- test[[1]]
                    p <- test[[3]]

                    inner <- tibble(site_code = i,
                                    var = j,
                                    month = k,
                                    start = start,
                                    end = end,
                                    n = n,
                                    trend = trend,
                                    p = p
                    )

                    # bind out
                    out_frame <- rbind(out_frame, inner)

                } # end month loop

            } # end solute loop

        } # end site loop

    return(out_frame)

} # end function

# Calculate monthly trends using sen slope.
n_monthly_trends <- detect_monthly_trends(n_monthly)

# Add columns for identification.
n_monthly_trends <- n_monthly_trends %>%
    mutate(sig = case_when(p < 0.05 ~ 1,
                           TRUE ~ 0)) %>%
    mutate(group = factor(case_when(sig == 1 & trend > 0 ~ "sig. increasing",
                                    sig == 0 & trend > 0 ~ "ns. increasing",
                                    sig == 1 & trend < 0 ~ "sig. decreasing",
                                    sig == 0 & trend < 0 ~ "ns. decreasing",
                                    TRUE ~ "no trend"),
                          levels = c("sig. increasing", "ns. increasing",
                                     "no trend",
                                     "ns. decreasing", "sig. decreasing")))

# Export for use in making figures.
# saveRDS(n_monthly_trends, "data_working/nitrogen_monthly_trends.rds")

n_monthly_trends_summary <- n_monthly_trends %>%
    count(var, month, group) %>%
    ungroup()

(fig_monthly <- ggplot(n_monthly_trends_summary %>%
           filter(var %in% c("nitrate_N", "ammonia_N",
                                 "TDN", "TN")),
       aes(x = month, y = n, fill = group)) +
    scale_x_continuous(breaks = c(2,4,6,8,10,12)) +
    scale_fill_manual(values = c("red", "darksalmon", "gray59", "cadetblue3", "blue")) +
    geom_bar(position = "stack", stat = "identity") +
    labs(x = "Month", y = "Site Count", fill = "Trend") +
    facet_wrap(var~., scales = "free") +
    theme_bw())

# ggsave(fig_monthly,
#        filename = "figures/n_vwm_monthly_trends.jpg",
#        width = 20,
#        height = 10,
#        units = "cm")

#### Seasonality ####

# Also curious to examine seasonal/monthly means over the full record
# for all sites.
n_monthly_means <- n_monthly %>%
    # calculate monthly means
    group_by(site_code, var, month) %>%
    summarize(mean_vwm = mean(val, na.rm = TRUE),
              sd_vwm = sd(val, na.rm = TRUE)) %>%
    ungroup()

n_peak_means <- n_monthly_means %>%
    # and determine what season peak concentrations occur in
    group_by(site_code, var) %>%
    slice_max(mean_vwm) %>%
    select(site_code, var, month) %>%
    rename(peak_month = month) %>%
    ungroup()

# Trim down metadata for using domains.
domains <- ms_site_data %>%
    select(domain, site_code)

n_monthly_means <- full_join(n_monthly_means, n_peak_means) %>%
    left_join(., domains) %>%
    mutate(peak_season = factor(case_when(peak_month %in% c(9,10,11) ~ "Fall",
                                   peak_month %in% c(12,1,2) ~ "Winter",
                                   peak_month %in% c(3,4,5) ~ "Spring",
                                   peak_month %in% c(6,7,8) ~ "Summer"),
                                levels = c("Fall", "Winter",
                                           "Spring", "Summer")))

(fig_seasonal1 <- ggplot(n_monthly_means %>%
                             filter(var %in% c("nitrate_N",
                                                   "ammonia_N",
                                                   "TDN",
                                                   "TN")) %>%
                             # need to filter out multiples
                             select(site_code, var, peak_month, domain) %>%
                             distinct(),
                  aes(x = peak_month, fill = domain)) +
        scale_x_continuous(breaks = c(2,4,6,8,10,12)) +
        scale_fill_viridis(discrete = TRUE) +
        geom_bar(position = "stack") +
        theme_bw() +
        facet_wrap(var~.))

# ggsave(fig_seasonal1,
#        filename = "figures/n_vwm_monthly_peaks.jpg",
#        width = 30,
#        height = 15,
#        units = "cm")

(fig_seasonal2 <- ggplot(n_monthly_means %>%
                             filter(var == "nitrate_N"),
                         aes(x = month,
                             y = mean_vwm,
                             group = site_code,
                             color = domain)) +
        scale_x_continuous(breaks = c(2,4,6,8,10,12)) +
        scale_color_viridis(discrete = TRUE) +
        geom_line(linewidth = 1) +
        theme_bw() +
        facet_wrap(peak_season~., scales = "free"))

# ggsave(fig_seasonal2,
#        filename = "figures/n_vwm_seasonal.jpg",
#        width = 30,
#        height = 15,
#        units = "cm")

nitrate_monthly_means_summary <- n_monthly_means %>%
    filter(var == "nitrate_N") %>%
    group_by(site_code) %>%
    slice_head() %>%
    ungroup() %>%
    count(peak_season) %>%
    ungroup()

# Also going back to raw discharge data to calculate
# mean monthly discharge (including all possible data
# since there are far fewer restrictions on this).

# For normalization by watershed area.
# Normalize by watershed area.
area <- ms_site_data %>%
    select(site_code, ws_area_ha)

q_monthly <- dplyr::distinct(q_data, site_code, date, .keep_all = TRUE) %>%
    mutate(month = month(date)) %>%
    # Removes any interpolated values.
    filter(ms_interp == 0) %>%
    # Adds column in L/s*ha
    left_join(area, by = c("site_code")) %>%
    mutate(val_Lsecha = val/ws_area_ha) %>%
    # And summarize values by month
    group_by(site_code, month) %>%
    summarize(mean_q_Lsha = mean(val_Lsecha)) %>%
    ungroup()

# Identify peak seasons.
q_monthly_peaks <- q_monthly %>%
    group_by(site_code) %>%
    slice_max(mean_q_Lsha) %>%
    ungroup() %>%
    rename(peak_month_q = month) %>%
    mutate(peak_season_q = factor(case_when(peak_month_q %in% c(9,10,11) ~ "Fall",
                                            peak_month_q %in% c(12,1,2) ~ "Winter",
                                            peak_month_q %in% c(3,4,5) ~ "Spring",
                                            peak_month_q %in% c(6,7,8) ~ "Summer"),
                                  levels = c("Fall", "Winter",
                                             "Spring", "Summer")))

# Plot these for comparison with N vwm peaks.
# NOTE THIS IS AT ALL SITES - NOT FILTERED TO MATCH N YET!
(fig_seasonal3 <- ggplot(q_monthly_peaks %>%
                             left_join(.,domains),
                         aes(x = peak_month_q, fill = domain)) +
        scale_x_continuous(breaks = c(2,4,6,8,10,12)) +
        scale_fill_viridis(discrete = TRUE) +
        geom_bar(position = "stack") +
        labs(x = "Peak Month", y = "Site Count",
             fill = "Domain") +
        theme_bw())

# ggsave(fig_seasonal3,
#        filename = "figures/q_monthly_peaks.jpg",
#        width = 20,
#        height = 10,
#        units = "cm")

# And join with N data to examine coincidence of peaks.
monthly_peak_match <- left_join(n_monthly_means, q_monthly_peaks, by = c("site_code")) %>%
    select(site_code, domain, var,
           peak_month, peak_season,
           peak_month_q, peak_season_q) %>%
    distinct() %>%
    mutate(peak_match = case_when(peak_season == peak_season_q ~ 1,
                                  TRUE ~ 0))

# End of script.
