# Calibrated Landsat greenness (replacement for Robinson Landsat GPP)

Robinson et al. (2018) Landsat GPP (`gpp_CONUS_30m_median`, from MacroSheds
`spatial_timeseries_vegetation.feather`) is not cross-calibrated among Landsat sensors.
Landsat 8/9 OLI reads higher NDVI than TM/ETM+, so the series steps up when OLI
enters (water years 2014-2015 in the MacroSheds data), inflating greening trends.

This directory rebuilds a productivity proxy, growing-season NDVI (and NIRv), from
Collection 2 surface reflectance, cross-calibrated to the Landsat 7 scale with
LandsatTS (Berner et al. 2023, Ecography, doi:10.1111/ecog.06768).

## Run order

| script | does | needs |
|---|---|---|
| `00_config.R` | shared parameters (sourced by all) | |
| `01_sample_points.R` | 30 random points (distinct 30 m pixels) per non-experimental CONUS watershed | MacroSheds `ws_boundary` |
| `02_export_gee.R` | per-point L5/7/8/9 time series -> Google Drive | rgee, Earth Engine project (`GEE_USER`, `GEE_PROJECT` env vars) |
| `03_clean_calibrate.R` | format, clean, NDVI/NIRv, `lsat_calibrate_rf()` | exported CSVs in `data_working/greenness/export/` |
| `04_growing_season.R` | phenology, annual growing-season metrics, watershed aggregation | |
| `05_validate.R` | step tests, diagnostics, sensitivity | |
| `06_trends.R` | Sen's slopes with the same filters as temp/precip/GPP; greening counts | `data_working/discharge_metrics_siteyear_nTest.rds`, `no3_trends_annual.rds` |
| `07_export_series.R` | collaborator deliverable: annual ndvi/nirv per site-water year (`data_working/greenness/deliverable/`) | `greenness_annual.rds`, `sample_points_summary.csv` |

Outputs go to `data_working/greenness/` and `figures/greenness/` (both gitignored).
The primary variable is `ndvi_gs_xcal` in `data_working/greenness/greenness_annual.rds`.

## Decisions

- **Index/metric.** Growing-season median NDVI (`ndvi_gs_xcal`). Also produced:
  NIRv (closer to GPP, less saturation in closed forest) and phenology-modeled annual max.
- **Growing season.** Per point, observations where the multi-year phenology spline is
  >= 75% of its seasonal peak (LandsatTS definition). Adapts to winter/spring green-up at
  Mediterranean sites. Calendar-year growing season N -> water year N.
- **Points.** 30 per watershed, from distinct 30 m cells; all cells where fewer exist
  (Bigelow, 1.4 ha; ER_BTH1, 2.6 ha). 3,764 points in 126 watersheds.
- **Point -> watershed.** Watershed level (mean of point means) + median point anomaly
  that year; a year needs >= 50% of points. Keeps cloud/SLC-off sampling changes out.
- **Landsat 7 drift.** L7 observations after 2017 are dropped from calibration training
  and from the primary series (its overpass time drifted earlier from ~2017; orbit
  lowered 2022). LandsatTS's docs don't address this.
- **Landsat 9.** LandsatTS doesn't export or recognize L9. `02_*` adds LC09 collections;
  `03_*` relabels L9 as L8 (OLI-2 ~ OLI) and checks L8 vs L9 agreement
  (`diag_l8_vs_l9.csv`).
- **Calibration window.** `doy.rng` 121:273 (May-Sep) rather than the Arctic default 152:243.
- **NIRv phenology.** `si.min` = 0.02 for NIRv; the LandsatTS default (0.15) suits NDVI only.

## Alternative calibration

LEOHS (Richardson et al. 2025, Geocarto Int., doi:10.1080/10106049.2025.2538108;
`pip install leohs`) fits regional ETM+ <-> OLI per-band regressions from
near-simultaneous image pairs in WRS-2 overlap zones, for user-chosen years and
months. It covers L7/L8 only (no L5, no L9). It's a fallback if the LandsatTS RF
performs poorly for OLI, e.g. if the pre-drift L7/L8 overlap (2013-2017) is too thin.

## Known issues

- `macrosheds::ms_download_core_data()` / `ms_download_ws_attr()` (v2.0.3): figshare's
  `ndownloader` URLs now return an AWS WAF challenge (HTTP 202, empty body). The
  package saves the empty response, `unzip()` only warns, and the function reports
  success. Workaround: download from `https://api.figshare.com/v2/file/download/<id>`
  using the ids in `macrosheds::file_ids_for_r_package`.
- Installing LandsatTS can fail on `protolite` when a conda env's `protoc` shadows the
  system one; build with the conda env removed from `PATH`.
