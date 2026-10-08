# Swap calibrated Landsat greenness (NDVI or NIRv) in for Robinson et al. (2018) Landsat
# GPP (gpp_CONUS_30m_median). The series is built by the pipeline in src/greenness/ on
# branch greenness-investigation of github.com/vlahm/ms_physical_patterns.
#
# The greenness values keep the GPP variable names (gpp_CONUS_30m_median in the
# vegetation time series, gpp_CONUS_30m_mean in the watershed summaries), so downstream
# code runs unchanged. Plot labels use greenness_label and greenness_short below.
#
# Sourced by src/setup.R. Used where spatial_timeseries_vegetation.feather is read:
#   veg <- read_feather(...) %>% swap_in_greenness()

# settings ####
greenness_var <- 'ndvi' # 'ndvi' or 'nirv'
greenness_file <- here::here('data_raw', 'greenness', 'ms_landsat_greenness_annual.csv')
greenness_label <- c(ndvi = 'Growing-season NDVI', nirv = 'Growing-season NIRv')[greenness_var]
greenness_short <- c(ndvi = 'NDVI', nirv = 'NIRv')[greenness_var]

# annual sum that is NA, not 0, when a site-year has no values (sites without a
# greenness series would otherwise plot as 0)
sum_or_na <- function(x) if(all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE)

read_greenness <- function(){
    if(! file.exists(greenness_file)) stop('greenness series not found at ', greenness_file)
    g <- readr::read_csv(greenness_file, show_col_types = FALSE)
    g$val <- g[[greenness_var]]
    dplyr::filter(g, ! is.na(val))
}

swap_in_greenness <- function(veg){

    # veg: spatial_timeseries_vegetation.feather (long: site_code, date, var, val, ...).
    # Replaces gpp_CONUS_30m_median rows with one greenness value per site and water
    # year, dated July 1. Annual means and sums of that variable then return the
    # greenness value; seasonal and monthly aggregates of it are not meaningful.

    g <- read_greenness() %>%
        dplyr::transmute(network, domain, site_code, var = 'gpp_CONUS_30m_median',
                         year = water_year, date = as.Date(paste0(water_year, '-07-01')), val)

    veg %>%
        dplyr::filter(var != 'gpp_CONUS_30m_median') %>%
        dplyr::bind_rows(g[, intersect(names(g), names(veg))])
}

swap_in_greenness_attr <- function(ws_attr){

    # ws_attr: watershed_summaries.feather. Replaces gpp_CONUS_30m_mean with each
    # site's mean greenness over its record (NA for sites without a greenness series).

    g <- read_greenness() %>%
        dplyr::group_by(site_code) %>%
        dplyr::summarize(gpp_CONUS_30m_mean = mean(val), .groups = 'drop')

    ws_attr %>%
        dplyr::select(-gpp_CONUS_30m_mean) %>%
        dplyr::left_join(g, by = 'site_code')
}
