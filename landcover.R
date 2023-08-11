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


# 2 Allocate G-values ----
# -----------------------------------------------------------------------------#
# join attributes and allocate g values
landcover.table <- read_zipped_GIS(zipfile = "../data/processed/landcover.zip") %>%
  left_join(read.csv("../data/processed/landcover_attributes.csv"), by = "CLASS") %>%
  mutate(g = case_when(
    CLASS %in% c("Built up", "Water (fresh / saline)") ~ 0,
    TRUE                                               ~ 1
  ))

# write the output (overwriting any version previously saved without G values)
write_zipped_shp(sf.obj = landcover.table, 
                 shp.name = "landcover",
                 shp.location = "../data/processed/landcover.zip")


# 3 Check areas for each class ----
# -----------------------------------------------------------------------------#
landcover.areas <- read_zipped_GIS(zipfile = "../data/processed/landcover.zip") %>%
  mutate(area_ha = st_area(geometry) / 10000)

area.summary <- landcover.areas %>%
  st_drop_geometry() %>%
  group_by(CLASS) %>%
  summarise(total_area = as.numeric(sum(area_ha))) %>%
  ungroup() %>%
  mutate(area_pct = total_area / sum(total_area) * 100)
