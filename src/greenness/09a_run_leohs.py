# Regional LEOHS harmonization for the MacroSheds watersheds (see 09_leohs.R).
# Run with the leohs_env Python (Python 3.13; `pip install leohs`):
#   ~/anaconda3/envs/leohs_env/bin/python src/greenness/09a_run_leohs.py
# AOI: non-experimental CONUS watersheds buffered by 20 km (made from sample_points.gpkg).
# Years 2013-2017 (Landsat 7 before orbit drift), May-September (matches calibration window).
import ee
import leohs

ee.Initialize(project='macrosheds-293818')
leohs.run_leohs(
    Aoi_shp_path='data_working/greenness/leohs/aoi_watersheds_20km.shp',
    Save_folder_path='data_working/greenness/leohs/run_2013_2017',
    SR_or_TOA='SR',
    months=[5, 6, 7, 8, 9],
    years=[2013, 2014, 2015, 2016, 2017],
    sample_points_n=100000,
    Regression_types=['OLS', 'RMA', 'TS'],
    project_ID='macrosheds-293818')
