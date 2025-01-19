# Extracts a zipped shapefile, mapinfo file, gpkg, etc, then reads uzing st_read
# 'subpath' is the string between the top zipped file and the ultimate file"
# 'file' not needed for files that don't have layers (eg shapefiles) if there is only one in the directory
# use 'file' (rather than 'layer') for shapefiles and mapinfo files; use both for gpkg [and sqlite?]
# library(sf)
read_zipped_GIS <- function(zipfile, subpath = "", file = NULL, layer = NULL) {
  temp <- tempfile()
  unzip(zipfile, exdir = temp)
  if (is.null(layer)) {
    st_read(paste0(temp, subpath, file))
  } else {
    st_read(paste0(temp, subpath, file), layer)
  }
}


# Extracts a zipped raster file, using library 'raster'
# (Note - alternatively, can use the library 'terra',
# in which case change the command 'raster' to 'rast')
read_zipped_raster <- function(zipfile, subpath = "", file) {
  temp <- tempfile()
  unzip(zipfile, exdir = temp)
  # rast(paste0(temp, subpath, file)) ## for terra
  raster(paste0(temp, subpath, file)) ## for raster
}


# Writes an sf object as a zipped shapefile, by first saving it to temp dir
# example parameters, which will create 'landcover.shp' inside 'landcover.zip'
# write_zipped_shp(sf.obj = landcover.poly, 
#                  shp.name = "landcover",
#                  shp.location = "../data/landcover.zip")

write_zipped_shp <- function(sf.obj, shp.name, shp.location) {
  # write sf object to temporary directory
  temp_dir <- tempdir()
  temp_shapefile <- file.path(temp_dir, paste0(shp.name, ".shp"))
  st_write(sf.obj, temp_shapefile, delete_dsn = TRUE)
  
  # zip the folder, and clean up the zipped files
  zip_file <- file.path(temp_dir, paste0(shp.name, ".zip"))
  files_to_zip <- list.files(temp_dir, pattern = paste0(shp.name, "\\."))
  
  # change the working directory (saving original first)
  orig.wd <- getwd()
  setwd(temp_dir)
  
  # zip the files
  zip(zip_file, files = files_to_zip)
  
  # restore wd, and save the zipped folder
  setwd(orig.wd)
  file.copy(zip_file, shp.location, overwrite = TRUE)
  
  # clean up temp files
  file.remove(file.path(temp_dir, files_to_zip))
  unlink(zip_file)
  
}

