# investigation of Melbourne buildings file

library(dplyr)
library(sf)

source("./functions/readWriteZippedGIS.R")

# 1 overture buildings - categories and height ----
# -----------------------------------------------------------------------------#

# load
overture <- read_zipped_GIS("../data/original/melbourne_buildings.zip")

# check categories of buildings
n_buildings <- nrow(overture)

categories1 <- overture %>%
  st_drop_geometry() %>%
  group_by(subtype) %>%
  summarise(n = n(),
            pct = round((n / n_buildings) * 100, 2)) %>%
  ungroup()

write.csv(categories1, "./GIS/overture_buildings_subtype.csv", row.names = F)

categories2 <- overture %>%
  st_drop_geometry() %>%
  group_by(subtype, class) %>%
  summarise(n = n(),
            pct = round((n / n_buildings) * 100, 2)) %>%
  ungroup()

write.csv(categories1, "./GIS/overture_buildings_subtype_class.csv", row.names = F)

# check number of buildings with specified height
height_rows <- nrow(overture %>%
                      filter(!is.na(height)))

height_pct <- round((height_rows / n_buildings) * 100, 2 )
## 65.31


# 2 microsoft buildings - intersection ----
# -----------------------------------------------------------------------------#
microsoft <- st_read("../data/original/australiabuildings.geojson")

region.buffered <- st_read("../data/processed/region_buffer.sqlite")

# not tested, reading in took too long
melbournebuildings <- microsoft %>%
  st_transform(st_crs(region.buffered)) %>%
  st_filter(., .predicate = st_intersects)