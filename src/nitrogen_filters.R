# Nitrogen data filtering script

#### README ####
# The following script will perform filtering
# for nitrogen data. It will filter data
# according to rough data coverage filters akin
# to what the main "metrics" script does for the
# discharge and climate data.

#### Load packages ####
library(here)
source(here('src', 'setup.R'))

#### Load data ####

# Annual volume weighted means
n_annual_vwm <- readRDS("data_working/nitrogen_annual_VWM.rds")

# Monthly volume weighted means
n_monthly_vwm <- readRDS("data_working/nitrogen_monthly_VWM.rds")

#### Good Data Filter ####

# Need to determine "good" site-years and site-months, such that
# there is a minimum of 1 observation per month and 10 months
# of the year for inclusion in larger annual analyses, as well
# as a minimum of 10 years of data.

## Annual #####

# First working on filters for annual-level data.
# Using the monthly dataset, let's count observations per month
# and months per year.
n_month_years <- n_monthly_vwm %>%
    mutate(water_year = case_when(month %in% c(10, 11, 12) ~ year+1,
                                  TRUE ~ year)) %>%
    select(site_code, analyte, water_year, month, n_obs) %>%
    pivot_wider(values_from = n_obs,
                names_from = month) %>%
    # reordering just for ease of viewing
    select(site_code, analyte, water_year, `10`, `11`, `12`,
           `1`, `2`, `3`, `4`, `5`, `6`, `7`, `8`, `9`) %>%
    # now to add appropriate flags
    mutate(flag_10 = case_when(`10` >= 1 ~ 1,
                               TRUE ~ 0),
           flag_11 = case_when(`11` >= 1 ~ 1,
                               TRUE ~ 0),
           flag_12 = case_when(`12` >= 1 ~ 1,
                               TRUE ~ 0),
           flag_1 = case_when(`1` >= 1 ~ 1,
                               TRUE ~ 0),
           flag_2 = case_when(`2` >= 1 ~ 1,
                               TRUE ~ 0),
           flag_3 = case_when(`3` >= 1 ~ 1,
                               TRUE ~ 0),
           flag_4 = case_when(`4` >= 1 ~ 1,
                               TRUE ~ 0),
           flag_5 = case_when(`5` >= 1 ~ 1,
                               TRUE ~ 0),
           flag_6 = case_when(`6` >= 1 ~ 1,
                               TRUE ~ 0),
           flag_7 = case_when(`7` >= 1 ~ 1,
                               TRUE ~ 0),
           flag_8 = case_when(`8` >= 1 ~ 1,
                               TRUE ~ 0),
           flag_9 = case_when(`9` >= 1 ~ 1,
                               TRUE ~ 0)) %>%
    mutate(flag_months = flag_10 + flag_11 + flag_12 +
                                 flag_1 + flag_2 + flag_3 +
                                 flag_4 + flag_5 + flag_6 +
                                 flag_7 + flag_8 + flag_9)

# Trim down only to years that meet the monthly observation
# and frequency within a year criteria.
# This drops us from 8495 site-years to 5397 site-years or
# a 40% loss of data.
n_mos_yrs_trim <- n_month_years %>%
    filter(flag_months >= 10)

# Now to filter for data density (i.e., minimum 10 years of
# data for trend analysis).
n_years <- n_mos_yrs_trim %>%
    group_by(site_code, analyte) %>%
    mutate(years = length(unique(water_year)),
              start = min(water_year),
              end = max(water_year))

# And trim down only to sites with sufficient data.
n_yrs_trim <- n_years %>%
    # First filter = 10 years minimum
    filter(years >= 10) %>% # down to 4392 site-years
    # And perform calculations for data coverage
    mutate(duration = end - start) %>%
    mutate(coverage = years/duration) %>%
    # Second filter = 60% of data present
    filter(coverage > 0.6) %>%
    select(site_code, analyte, water_year) # down to 4166 site-years

# Finally merge with annual vwm data.
n_annual_vwm_filtered <- left_join(n_yrs_trim, n_annual_vwm,
                                   by = c("site_code",
                                          "analyte",
                                          "water_year"))
# No loss of data.

# Export filtered data.
# saveRDS(n_annual_vwm_filtered, "data_working/nitrogen_annual_VWM_good.rds")

## Monthly #####

# Next working on filters for monthly-level data.
# Using the monthly dataset, let's count observations per month
# and trim down to >1 obs. per month.
n_mos_trim <- n_monthly_vwm %>%
    select(site_code, analyte, year, month, n_obs) %>%
    filter(n_obs >= 1)

# Now to filter for data density (i.e., minimum 10 years of
# data for trend analysis).
n_moyears <- n_mos_trim %>%
    group_by(site_code, analyte, month) %>%
    mutate(years = length(unique(year)),
           start = min(year),
           end = max(year)) %>%
    ungroup()

# And trim down only to sites with sufficient data.
n_moyrs_trim <- n_moyears %>%
    # First filter = 10 years minimum
    filter(years >= 10) %>% # 64311 site-months of data remaining
    # And perform calculations for data coverage
    mutate(duration = end - start) %>%
    mutate(coverage = years/duration) %>%
    # Second filter = 60% of data present
    filter(coverage > 0.6) %>%
    select(site_code, analyte, year, month) # 61184 site-months remaining

# Finally merge with annual vwm data.
n_monthly_vwm_filtered <- left_join(n_moyrs_trim, n_monthly_vwm,
                                   by = c("site_code", "analyte",
                                          "year", "month"))
# This drops us from 79244 site-months to 61184 site-months or
# a 28% loss of data.

# Export filtered data.
#saveRDS(n_monthly_vwm_filtered, "data_working/nitrogen_monthly_VWM_good.rds")

# End of script.
