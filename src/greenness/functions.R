# Shared functions (used by 04_growing_season.R, 09_leohs.R, 09b_l7_only.R, 10_leohs_figures.R).
#
# point_gs(): per-pixel phenology curves and annual growing-season summaries.
# ws_aggregate(): pixel-year values -> watershed-year values.
# leohs_apply(): LEOHS linear Landsat 8 -> 7 band harmonization of observations.

point_gs <- function(obs, si, col){

    # obs: observation-level data.table; col: column holding the index values

    dt <- obs[, .(sample.id, latitude, longitude, year, doy, val = get(col))]
    setnames(dt, 'val', si)

    pheno <- lsat_fit_phenological_curves(dt,
                                          si = si,
                                          window.yrs = pheno_window_yrs,
                                          window.min.obs = pheno_window_min_obs,
                                          si.min = pheno_si_min[[si]],
                                          spl.fit.outfile = FALSE,
                                          progress = FALSE)

    gs <- lsat_summarize_growing_seasons(pheno, si = si, min.frac.of.max = gs_min_frac_of_max)
    setnames(gs, gsub(paste0('^', si, '\\.'), '', names(gs)))
    gs[n.obs >= min_obs_per_pt_year,
       .(sample.id, year, n.obs, gs.med, gs.avg, max)]
}

ws_aggregate <- function(pt, value_col){

    pt <- copy(pt)[, val := get(value_col)]
    pt[, site_code := sub('__[0-9]+$', '', sample.id)]
    pt[, n_pts_site := uniqueN(sample.id), by = site_code]
    pt[, pt_mean := mean(val, na.rm = TRUE), by = sample.id]
    pt[, anom := val - pt_mean]

    level <- pt[, .(pt_mean = first(pt_mean)), by = .(site_code, sample.id)][
        , .(ws_level = mean(pt_mean)), by = site_code]

    ws <- pt[, .(anom = median(anom, na.rm = TRUE),
                 n_pts = uniqueN(sample.id),
                 n_pts_site = first(n_pts_site)),
             by = .(site_code, year)]
    ws <- ws[n_pts >= min_frac_pts_per_year * n_pts_site]
    ws[level, on = 'site_code', val := ws_level + anom]
    ws[, .(site_code, water_year = year, val, n_pts)]
}

leohs_apply <- function(obs, cf){

    # Apply LEOHS Landsat 8 -> Landsat 7 linear band equations (cf: rows of
    # leohs_coefficients.csv for one set) to red and NIR, recovered from the stored
    # indices: N = NIRv / NDVI, R = N (1 - NDVI) / (1 + NDVI). Landsat 5 keeps its
    # LandsatTS calibration and Landsat 7 is unchanged. Adds ndvi.leohs, nirv.leohs.

    cf_red <- cf[cf$band == 'red', ]
    cf_nir <- cf[cf$band == 'nir', ]
    obs[, `:=`(ndvi.leohs = ndvi.xcal, nirv.leohs = nirv.xcal)]
    oli <- obs$satellite == 'LANDSAT_8' & obs$ndvi > 0.01 # includes Landsat 9, relabeled in 03_*
    nir8 <- obs$nirv[oli] / obs$ndvi[oli]
    red8 <- nir8 * (1 - obs$ndvi[oli]) / (1 + obs$ndvi[oli])
    nir7 <- cf_nir$slope * nir8 + cf_nir$intercept
    red7 <- cf_red$slope * red8 + cf_red$intercept
    ndvi7 <- (nir7 - red7) / (nir7 + red7)
    obs[oli, `:=`(ndvi.leohs = ndvi7, nirv.leohs = ndvi7 * nir7)]
    obs[satellite == 'LANDSAT_8' & !oli, `:=`(ndvi.leohs = NA_real_, nirv.leohs = NA_real_)]
    invisible(obs)
}
