# Export per-point Landsat Collection 2 surface reflectance time series from
# Google Earth Engine to Google Drive.
#
# lsat_export_ts_l9() is LandsatTS::lsat_export_ts() (v1.2.3, MIT license,
# Berner/Assmann et al.) with Landsat 9 (LC09 T1/T2) added to the merged
# collection and the deprecated JRC/GSW1_0 water mask updated to JRC/GSW1_4 (same
# max_extent band). Otherwise unchanged, so output matches what lsat_format_data()
# expects, except SPACECRAFT_ID can now be LANDSAT_9 (handled in 03_*).
#
# Requires rgee configured against a Python env with earthengine-api, and an
# Earth Engine Cloud project. Submits ~16 tasks of <=250 points each.
#
# Usage: set GEE_USER and GEE_PROJECT env vars (and RETICULATE_PYTHON if needed),
# then see the run section at the bottom.

source(here::here('src', 'greenness', '00_config.R'))
library(rgee)
library(purrr)

gee_user <- Sys.getenv('GEE_USER')       # e.g. 'someone@gmail.com'
gee_project <- Sys.getenv('GEE_PROJECT') # Earth Engine Cloud project id

lsat_export_ts_l9 <- function(pixel_coords_sf,
                              sample_id_from = 'sample_id',
                              chunks_from = NULL,
                              this_chunk_only = NULL,
                              max_chunk_size = 250,
                              drive_export_dir = 'lsatTS_export',
                              file_prefix = 'lsatTS_export',
                              start_doy = 152,
                              end_doy = 243,
                              start_date = '1984-01-01',
                              end_date = 'today',
                              scale = 30,
                              mask_value = 0){

    if(end_date == 'today') end_date <- as.character(Sys.Date())
    sf::sf_use_s2(FALSE)

    bands <- list('SR_B1', 'SR_B2', 'SR_B3', 'SR_B4', 'SR_B5', 'SR_B6', 'SR_B7',
                  'QA_PIXEL', 'QA_RADSAT')
    BAND_LIST <- ee$List(bands)

    ADDON <- ee$Image('JRC/GSW1_4/GlobalSurfaceWater')$float()$unmask(mask_value)
    ADDON_BANDLIST <- ee$List(list('max_extent'))

    ZERO_IMAGE <- ee$Image(0)$select(list('constant'), list('SR_B6'))$selfMask()

    PROPERTIES <- list('CLOUD_COVER', 'COLLECTION_NUMBER', 'DATE_ACQUIRED',
                       'GEOMETRIC_RMSE_MODEL', 'LANDSAT_PRODUCT_ID',
                       'LANDSAT_SCENE_ID', 'PROCESSING_LEVEL', 'SPACECRAFT_ID',
                       'SUN_ELEVATION')

    colls <- c('LANDSAT/LT05/C02/T1_L2', 'LANDSAT/LE07/C02/T1_L2',
               'LANDSAT/LC08/C02/T1_L2', 'LANDSAT/LC09/C02/T1_L2',
               'LANDSAT/LT05/C02/T2_L2', 'LANDSAT/LE07/C02/T2_L2',
               'LANDSAT/LC08/C02/T2_L2', 'LANDSAT/LC09/C02/T2_L2')
    ls8_1 <- ee$ImageCollection('LANDSAT/LC08/C02/T1_L2')

    ALL_BANDS <- BAND_LIST$cat(ADDON_BANDLIST)

    LS_COLL <- reduce(lapply(colls[-1], ee$ImageCollection),
                      function(a, b) a$merge(b),
                      .init = ee$ImageCollection(colls[1]))$
        filterDate(start_date, end_date)$
        filter(ee$Filter$calendarRange(start_doy, end_doy, 'day_of_year'))$
        map(function(image){
            ee$Algorithms$If(image$bandNames()$size()$eq(ee$Number(10)),
                             image,
                             image$addBands(ZERO_IMAGE))
        })$
        map(function(image) image$addBands(ADDON, ADDON_BANDLIST))$
        select(ALL_BANDS)$
        map(function(image) image$float())

    if(is.null(chunks_from)){
        n_chunks <- floor(nrow(pixel_coords_sf) / max_chunk_size) + 1
        pixel_coords_sf$chunk_id <-
            paste0('chunk_', sort(rep(1:n_chunks, max_chunk_size)))[1:nrow(pixel_coords_sf)]
        chunks_from <- 'chunk_id'
    }

    if(!is.null(this_chunk_only)){
        pixel_coords_sf <- pixel_coords_sf[
            sf::st_drop_geometry(pixel_coords_sf)[[chunks_from]] == this_chunk_only, ]
    }

    cat('Exporting time-series for', nrow(pixel_coords_sf), 'pixels in',
        length(unique(sf::st_drop_geometry(pixel_coords_sf)[[chunks_from]])), 'chunks.\n')

    pixel_coords_sf %>%
        split(sf::st_drop_geometry(.)[[chunks_from]]) %>%
        map(function(chunk){

            chunk_name <- sf::st_drop_geometry(chunk)[[chunks_from]][1]
            cat('Submitting task to EE for chunk_id:', chunk_name, '\n')

            ee_chunk <- sf_as_ee(chunk[, c('geometry', sample_id_from, chunks_from)])

            ee_chunk_export <- ee_chunk$map(function(feature){
                ee$ImageCollection$fromImages(
                    list(ee$Image(list(0, 0, 0, 0, 0, 0, 0, 0, 0, 0))$
                             select(list(0, 1, 2, 3, 4, 5, 6, 7, 8, 9), ALL_BANDS)$
                             copyProperties(ls8_1$first())))$
                    merge(LS_COLL$filterBounds(feature$geometry()))$
                    map(function(image){
                        ee$Feature(feature$geometry(),
                                   image$reduceRegion(ee$Reducer$first(),
                                                      feature$geometry(),
                                                      scale))$
                            copyProperties(image, PROPERTIES)$
                            set(sample_id_from, feature$get(sample_id_from))$
                            set(chunks_from, feature$get(chunks_from))
                    })
            })$flatten()

            chunk_task <- ee_table_to_drive(
                collection = ee_chunk_export,
                description = paste0(file_prefix, '_', chunk_name),
                folder = drive_export_dir,
                fileNamePrefix = paste0(file_prefix, '_', chunk_name),
                timePrefix = FALSE,
                fileFormat = 'csv')
            chunk_task$start()
            chunk_task
        })
}

# download finished exports from Drive ####
download_exports <- function(drive_dir = gee_drive_dir, prefix = 'ms_lsat',
                             dest = file.path(gr_dir, 'export')){
    dir.create(dest, showWarnings = FALSE, recursive = TRUE)
    # concurrent EE tasks can each create a same-named Drive folder, so search all of them
    folders <- googledrive::drive_find(q = sprintf("name = '%s'", drive_dir),
                                       type = 'folder')
    files <- bind_rows(lapply(seq_len(nrow(folders)), function(i){
        googledrive::drive_ls(folders[i, ], pattern = paste0('^', prefix, '_.*\\.csv$'))
    }))
    files <- files[!duplicated(files$name), ]
    for(i in seq_len(nrow(files))){
        out <- file.path(dest, files$name[i])
        if(file.exists(out)) next
        googledrive::drive_download(files[i, ], path = out, overwrite = FALSE)
    }
    message(nrow(files), ' files in Drive; ', length(list.files(dest, '\\.csv$')), ' local')
}

# run ####
# Rscript src/greenness/02_export_gee.R submit    # start export tasks
# Rscript src/greenness/02_export_gee.R download  # fetch finished CSVs
action <- commandArgs(trailingOnly = TRUE)[1]
if(!is.na(action)){

    ee_Initialize(user = gee_user, drive = TRUE, project = gee_project, quiet = TRUE)

    if(action == 'submit'){

        pts <- st_read(file.path(gr_dir, 'sample_points.gpkg'), quiet = TRUE) %>%
            rename(geometry = geom)

        task_list <- lsat_export_ts_l9(pts,
                                       sample_id_from = 'sample_id',
                                       max_chunk_size = export_chunk_size,
                                       drive_export_dir = gee_drive_dir,
                                       file_prefix = 'ms_lsat',
                                       start_doy = export_doy[1],
                                       end_doy = export_doy[2],
                                       start_date = export_start_date,
                                       end_date = export_end_date)

        saveRDS(sapply(task_list, function(t) t$id), file.path(gr_dir, 'export_task_ids.rds'))

    } else if(action == 'download'){
        download_exports()
    }
}
