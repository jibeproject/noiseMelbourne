# Read landcover data and add required fields

library(dplyr)
library(sf)
library(fs)
library(raster)
library(stars)

dir_walk(path="./functions/",source, recurse=T, type = "file")

PROJECT.CRS <- 28355

# 1 Read in landcover, crop to study area, save as .shp ----
# -----------------------------------------------------------------------------#
# load landcover raster and study area
landcover <- read_zipped_raster(zipfile = "../data/original/VIC_LANDCOVER_TS.zip",
                                subpath = "/VIC_LANDCOVER_TS/",
                                file = "VIC_LANDCOVER_TS_2015_19.tif")

study.area <- st_read("../data/processed/region_buffer.sqlite") %>%
  st_transform(st_crs(landcover))

# crop landcover raster to study area
landcover.study.area <- raster::crop(x = landcover, y = study.area)

# convert to a stars object, and then to polygons
landcover.study.area.stars <- st_as_stars(landcover.study.area)
landcover.poly <- st_as_sf(landcover.study.area.stars,
                                  as_points = FALSE, merge = TRUE)

# tidy field name, and update crs to project crs
landcover.poly <- landcover.poly %>%
  rename(CLASS = START_YEAR) %>%
  st_transform(PROJECT.CRS)

# write the output
write_zipped_shp(sf.obj = landcover.poly, 
                 shp.name = "landcover",
                 shp.location = "../data/processed/landcover.zip")


# get and write the attribute table from the raster file
landcover.attributes <- levels(landcover)[[1]] #%>%
  # omit characters that can't be converted [maybe unreasonable?]
  # mutate_all(~gsub('[^ -~]', '', .))

write.csv(landcover.attributes, "../data/processed/landcover_attributes.csv", 
          row.names = FALSE)


# 2 Read in landcover polygons, join to attributes .... ----
# -----------------------------------------------------------------------------#
# Might be better not to do this, or at least not until after added G-codes
# (don't get too worked up about the size of the table with attributes added - 
# its 192MB with vs 184MB without)
landcover.table <- read_zipped_GIS(zipfile = "../data/processed/landcover.zip") %>%
  left_join(read.csv("../data/processed/landcover_attributes.csv"), by = "CLASS")





