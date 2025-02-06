# add purpose and height to buildings

# set correct PROJ_LIB environmental variable, if necessary (this is 
# some weird problem of a conflict with the GDAL or PROJ library in my
# postgreSQL installation - relevant for terra in section 4)
Sys.setenv(GDAL_LIB = "C:/Users/steve/AppData/Local/R/win-library/4.4/sf/gdal")
Sys.setenv(PROJ_LIB = "C:/Users/steve/AppData/Local/R/win-library/4.4/sf/proj")
Sys.getenv("GDAL_LIB")
Sys.getenv("PROJ_LIB")


library(dplyr)
library(sf)
library(terra)
library(ggplot2)

source("./functions/readWriteZippedGIS.R")


# 1 Load required inputs ----
# -----------------------------------------------------------------------------#

# overture buildings (these have purpose and height for some buildings only)
overture <- read_zipped_GIS(zipfile = "../data/original/melbourne_buildings.zip") %>%
  st_set_crs(4326)

# points of interest (for purpose)
poi <- st_read("../data/processed/poi.gpkg")

# # review pois
# poi.classified <- poi %>%
#   st_drop_geometry() %>%
#   group_by(Attribute, Indicator) %>%
#   summarise(n = n()) %>%
#   ungroup() %>%
#   dplyr::select(Indicator, Attribute, n) %>%
#   arrange(Indicator, Attribute)

# meshblocks (for purpose)
mb <- read_zipped_GIS(zipfile = "../data/original/1270055001_mb_2016_vic_shape.zip") %>%
  st_transform(st_crs(poi)) %>%
  left_join(read.csv("../data/original/2016 census mesh block counts.csv"),
            by = c("MB_CODE16" = "MB_CODE_2016"))

# Buildings for Melbourne, Yarra and Manningham LGAs (for height)
# LGAs
LGAs <- read_zipped_GIS(zipfile = "../data/original/LGAs.zip", 
                        subpath = "/mga94_55/esrishape/whole_of_dataset/victoria/VMADMIN/")

# Melbourne
melb.lga.buildings <- read_zipped_GIS(zipfile = "../data/original/2018-building-footprints.zip")

# Yarra
yarra <- LGAs %>%
  filter(NAME == "YARRA")
yarra.bed.elevation <- rast("../data/processed/yarra_buildings/yarra_building_bed_elevation.tif")
yarra.dem <- rast("../data/original/yarra_buildings/mga55_yarra_1m_dtm.tif")

# Manningham
mann.lga.buildings <- read_zipped_GIS(zipfile = "../data/original/manningham_building_footprints.zip")


# Global building height (150m resolution raster)
# See Ma et al (2024), A global 150-m dataset of urban building heights around 2020 
# (GBH2020) https://figshare.com/articles/dataset/2020_150__/25729248/6 
global.height <- rast("../data/original/GBH2020_150m_GEDI.tif")


# 2 Purpose ----
# -----------------------------------------------------------------------------#

# Include purpose (Education, Residential or Other), based on (1) purpose in
# Overture data, if present, or (2) (if none) POI purpose, or (3) (if none)
# meshblock purpose

buildings.purpose <- overture %>%
  
  # join poi indicators, recoded as 'Education' or 'Other' (leaving NAs unchanged)
  st_transform(st_crs(poi)) %>%
  st_join(poi %>% 
            mutate(poi_purpose = case_when(
              Indicator == "Education" ~ "Education",
              !is.na(Indicator) ~ "Other"
            )) %>%
            dplyr::select(poi_purpose), 
          join = st_intersects) %>%
  
  # there may be duplicates - if so, keep the 'highest' poi purpose, 
  # with priority order Education, Other, NA
  mutate(priority = case_when(
    poi_purpose == "Education" ~ 2,
    poi_purpose == "Other"     ~ 1,
    TRUE                       ~ 0
  )) %>%
  arrange(id, desc(priority)) %>%  # sort by id and priority descending
  filter(!duplicated(id)) %>%  # keep the first occurrence of each id
  select(-priority) %>% # remove the priority column

  # join meshblocks, recoded as 'Education', 'Residential' or 'Other' (note that
  # residential is both meshblocks specified as residential, plus any other
  # non-education with > 5 dwellings and > 10 people)
  st_join(mb %>% 
            mutate(mb_purpose = case_when(
              MB_CAT16 == "Education"                              ~ "Education",
              MB_CAT16 == "Residential"                            ~ "Residential",
              MB_CAT16 != "Education" & Dwelling > 5 & Person > 10 ~ "Residential",
              TRUE                                                 ~ "Other"
            )) %>%
            dplyr::select(mb_purpose), 
          join = st_intersects) %>%

  # there may be duplicates - if so, keep the one with the largest intersection
  # area (ie the meshblock that contains most of the building)
  mutate(area = st_area(.)) %>%
  arrange(id, desc(area)) %>%  # sort by id and area descending
  filter(!duplicated(id))%>%  # keep the first occurrence of each id
  select(-area) %>%  # remove the area column
  
  # determine purpose from (1) Overture, (2) poi or (3) meshblock
  mutate(purpose = case_when(
    # first - based on Overture purposes
    subtype == "education"   ~ "Education",
    subtype == "residential" ~ "Residential",
    !is.na(subtype)          ~ "Other",
    
    # second = based on poi purposes
    poi_purpose == "Education" ~ "Education",
    poi_purpose == "Other"     ~ "Other",
    
    # third = based on meshblock
    is.na(mb_purpose) ~ "Other",  # pier or foreshore buildings outside meshblocks
    TRUE              ~ mb_purpose
  ))

# # check how many have purpose from Overture or poi
# no.overture.purpose <- nrow(buildings.purpose %>% filter(!is.na(subtype))) /
#   nrow(buildings.purpose)
# no.overture.purpose  # 13.59%
# no.poi.purpose <- nrow(buildings.purpose %>% filter(!is.na(subtype) | !is.na(poi_purpose))) /
#   nrow(buildings.purpose)
# no.poi.purpose  # 14.14%
  

# 3 Height ----
# -----------------------------------------------------------------------------#

## 3.1 Add LGA or global 150m height ----
## ------------------------------------#
# Where no Overture height specified - use height from LGA datasets, if available
# (Melbourne, Yarra, Manningham), or from global 150m height dataset

# Prepare heights for City of Yarra buildings - this uses a raster of building
# 'bed elevation' calculated from City of Yarra 3D buildings, created using
# the following process:
# 1.	In MeshLab, import the 12.3ds files (using ‘Load in a single layer’), 
#     using File > Import Mesh.  Merge them into a single layer using Filters > 
#     Mesh Layer > Flatten Visible Layers.  Export the result as a .ply file using 
#     File > Export Mesh As (data/processed/yarra_buildings/yarra_buildings.ply)
# 2.	In QGIS, open the .ply file, using Layer > Add Layer > Add Mesh Layer.
# 3.	In QGIS, use the ‘Rasterise mesh dataset’ tool to convert the yarra_buildings 
#     layer to a raster, selecting ‘Bed Elevation’ as the dataset group.  
#     Save the result (data/processed/yarra_buildings/yarra_building_bed_elevation.tif),
# Then, building height is calculated in the script below as 'bed elevation' minus
# DEM elevation.

# overture building centroids for Yarra
overture.yarra <- overture %>%
  st_transform(st_crs(yarra)) %>%
  st_filter(yarra, .predicate = st_intersects) %>%
  st_centroid()

# add height to yarra overture centroids (bed elevation minus DEM elevation)
height.values <- terra::extract(yarra.bed.elevation, overture.yarra, method = "bilinear", ID = FALSE) - 
  terra::extract(yarra.dem, overture.yarra, method = "bilinear", ID = FALSE) %>%
  as.data.frame() 
height.values <- height.values %>%
  rename_with(~"yarra_lga_height") %>%
  mutate(yarra_lga_height = ifelse(is.nan(yarra_lga_height), NA, yarra_lga_height)) %>%
  .$yarra_lga_height

overture.yarra$yarra_lga_height <- height.values


# global (150m) heights for buildings
building.centroids <- overture %>%
  st_make_valid() %>%
  st_centroid() %>%
  st_transform(st_crs(global.height))
global.height.values <- terra::extract(global.height, building.centroids, ID = FALSE)
names(global.height.values) <- "height_150m"
building.centroids.height.150 <- cbind(building.centroids, global.height.values) %>%
  mutate(height_150m = ifelse(is.nan(height_150m), NA, height_150m))
  

# add height from LGA and global (150m) datasets
buildings.specified.height <- buildings.purpose %>%
  
  # rename height as overture.height
  rename(overture_height = height) %>%
  
  # join melb and manningham height
  st_join(melb.lga.buildings %>%
            st_transform(st_crs(buildings.purpose)) %>%
            dplyr::select(melb_lga_height = structure_m),
          join = st_intersects) %>%
  st_join(mann.lga.buildings %>%
            st_transform(st_crs(buildings.purpose)) %>%
            dplyr::select(mann_lga_height = height),
          join = st_intersects) %>%

  # there may be duplicates - if so, keep the one with the largest intersection
  # area (ie the melb or manningham building that contains most of the overture building)
  mutate(area = st_area(.)) %>%
  arrange(id, desc(area)) %>%  # sort by id and area descending
  filter(!duplicated(id))%>%  # keep the first occurrence of each id
  select(-area) %>% # remove the area column

  # join yarra building height
  left_join(overture.yarra %>%
              st_drop_geometry() %>%
              dplyr::select(id, yarra_lga_height) %>%
              # omit any NAs, and any below zero
              filter(!is.na(yarra_lga_height) & yarra_lga_height > 0),
            by = "id") %>%
  
  # join global (150m) height
  left_join(building.centroids.height.150 %>%
              st_drop_geometry() %>%
              dplyr::select(id, height_150m),
            by = "id") %>%
 
  # height is overture height if specified, or else LGA height if specified, or
  # else global (150m) height if specified
  mutate(height = ifelse(!is.na(overture_height), overture_height, 
                         ifelse(!is.na(melb_lga_height), melb_lga_height, 
                                ifelse(!is.na(yarra_lga_height), yarra_lga_height,
                                       ifelse(!is.na(mann_lga_height), mann_lga_height,
                                              height_150m)))))


## 3.2 Add meshblock height ----
## ------------------------------------#
# Where no height found in 3.1, use average height of buildings in the meshblock
# that do have height; and, if none, then from nearest other meshblock that has
# buildings with height

# where height is not specified, calculate as the average height of buildings in the meshblocks
buildings.with.mb.height <- buildings.specified.height %>%
  
  # join intersecting meshblock
  st_join(mb %>% dplyr::select(MB_CODE16),
          join = st_intersects) %>%
  
  # there may be duplicates - if so, keep the one with the largest intersection
  # area (ie the meshblock that contains most of the building)
  mutate(area = st_area(.)) %>%
  arrange(id, desc(area)) %>%  # sort by id and area descending
  filter(!duplicated(id))%>%  # keep the first occurrence of each id
  select(-area) %>% # remove the area column
  
  # calculate the mean height for the meshblock
  group_by(MB_CODE16) %>%
  mutate(height_mb = mean(height, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(height_mb = ifelse(is.nan(height_mb), NA, height_mb)) %>%
  
  # calculate height - as specified, or else average meshblock height
  mutate(height = ifelse(!is.na(height), height, height_mb))

# where height still not specified, take the average height from the nearest meshblock
# table of heights for meshblocks that have them
mb.heights <- buildings.with.mb.height %>%
  st_drop_geometry() %>%
  distinct(MB_CODE16, height_mb) %>%
  filter(!is.na(height_mb)) %>%
  # join the geometry
  left_join(mb %>% dplyr::select(MB_CODE16), by = "MB_CODE16") %>%
  st_as_sf()

# buildings lacking height, with height from nearest meshblock that has height
buildings.nearest.mb.height <- buildings.with.mb.height %>%
  filter(is.na(height)) %>%
  mutate(height_near_mb = mb.heights$height_mb[st_nearest_feature(., mb.heights)])

# add the nearest mb height to the buildings.height table
buildings.height <- buildings.with.mb.height %>%
  left_join(buildings.nearest.mb.height %>% 
              st_drop_geometry() %>%
              dplyr::select(id, height_near_mb),
            by = "id") %>%
  mutate(height = ifelse(!is.na(height), height, height_near_mb))



# 4 Write output ----
# -----------------------------------------------------------------------------#
st_write(buildings.height, "../data/processed/melbourne_buildings_processed.sqlite",
         delete_layer = TRUE)


# 5 Height descriptives ----
# -----------------------------------------------------------------------------#

## 5.1 Buildings in the region ----
## ------------------------------------#
# read in height details and filter to region
buildings.height <- st_read("../data/processed/melbourne_buildings_processed.sqlite")

# study region (greater melbourne gccsa)
region <- st_read("../data/processed/region.sqlite")

# buildings in the region (note - slow)
region.buildings <- buildings.height[st_intersects(buildings.height, region, sparse = FALSE), ]


## 5.2 Table of sources of height ----
## ------------------------------------#
# buildings with height from each source
# no of buildings in the region
region.building.no <- nrow(region.buildings)

# overture height
overture.pct <- nrow(region.buildings %>% 
                       filter(!is.na(overture_height))) / region.building.no * 100

# LGA height (don't use overture, and use melbourne, yarra or manningham instead)
lga.pct <- nrow(region.buildings %>%
                  filter(is.na(overture_height) & (
                    !is.na(melb_lga_height) | !is.na(yarra_lga_height) | !is.na(mann_lga_height)
                  ))) / region.building.no * 100

# global 150 height (don't use overture or LGA, and use global 150 height instead)
global150.pct <- nrow(region.buildings %>%
                        filter(is.na(overture_height) & is.na(melb_lga_height) &
                                 is.na(yarra_lga_height) & is.na(mann_lga_height) &
                                 !is.na(height_150m))) / region.building.no * 100

# average meshblock height (don't use overture, LGA or global 150m height, and
# use average meshblock height instead)
mb.pct <- nrow(region.buildings %>%
                 filter(is.na(overture_height) & is.na(melb_lga_height) &
                          is.na(yarra_lga_height) & is.na(mann_lga_height) &
                          is.na(height_150m) & !is.na(height_mb)
                        )) / region.building.no * 100

# nearest meshblock height (only calculated for the buildings that didn't use another source)
near.mb.pct <- nrow(region.buildings %>% filter(!is.na(height_near_mb))) / region.building.no * 100

# assemble into a table
source.table <- data.frame(
  "Source of height" = c("Overture height", "LGA height",
                         "Global 150m dataset height", "Meshblock average height", 
                         "Other nearest meshblock average height"),
  "Percentage" = c(overture.pct, lga.pct, global150.pct, mb.pct, near.mb.pct)
)

write.csv(source.table, "./GIS/greater melbourne buildings source of height.csv", row.names = F)

## 5.3 Density plots ----
## ------------------------------------#
# assemble height data for plot - source and weight are the height for whichever
# source is used as the final height - and only for buildings up to 20m
height.data <- region.buildings %>%
  st_drop_geometry() %>%
  mutate(lga_height = ifelse(!is.na(melb_lga_height), melb_lga_height,
                             ifelse(!is.na(yarra_lga_height), yarra_lga_height,
                                    mann_lga_height))) %>%
  dplyr::select(id, overture_height, lga_height, height_150m, height_mb, height_near_mb) %>%
  mutate(source = case_when(
    !is.na(overture_height) ~ "overture",
    !is.na(lga_height)      ~ "lga",
    !is.na(height_150m)     ~ "global 150m dataset",
    !is.na(height_mb)       ~ "meshblock average",
    !is.na(height_near_mb)  ~ "nearest meshblock average"
  )) %>%
  mutate(weight = case_when(
    !is.na(overture_height) ~ overture_height,
    !is.na(lga_height)      ~ lga_height,
    !is.na(height_150m)     ~ height_150m,
    !is.na(height_mb)       ~ height_mb,
    !is.na(height_near_mb)  ~ height_near_mb
  )) %>%
  filter(weight <= 20)

# plot
source.used <- ggplot(height.data, aes(x = weight, colour = source)) +
  geom_density() +
  theme_bw()

ggsave(paste0("./GIS/greater melbourne distrib of source used.png"),
       source.used, 
       width = 20, height = 16, units = "cm")

# comparing all the values for overture and global 150 (note - this is all their
# values, not just the one that was used in the final height output)

overture_height150 <- ggplot() +
  geom_density(data = height.data %>%
                 filter(overture_height <= 20), aes(x = overture_height), 
               colour = "blue", fill = "blue", alpha = 0.4) +
  geom_density(data = height.data %>%
                 filter(height_150m <= 20), aes(x = height_150m), 
               colour = "red", fill = "red", alpha = 0.4) +
  labs(x = "overture_height (blue) / height_150m (red)") +
  theme_bw()

ggsave(paste0("./GIS/greater melbourne overture vs global 150.png"),
       overture_height150, 
       width = 20, height = 16, units = "cm")

# and comparing all for overture and global 150, but only where a building 
# has both heights and both are <= 20)
overture_height150_matched <- ggplot(height.data %>%
                                       filter(overture_height <= 20 & height_150m <= 20 &
                                                !is.na(overture_height) & !is.na(height_150m))) +
  geom_density(aes(x = overture_height), 
               colour = "blue", fill = "blue", alpha = 0.4) +
  geom_density(aes(x = height_150m), 
               colour = "red", fill = "red", alpha = 0.4) +
  labs(x = "overture_height (blue) / height_150m (red)") +
  theme_bw()

ggsave(paste0("./GIS/greater melbourne overture vs global 150 matched.png"),
       overture_height150_matched, 
       width = 20, height = 16, units = "cm")


# comparing means by LGA
height.lga <- region.buildings %>%
  st_centroid() %>%
  st_join(LGAs %>% dplyr::select(LGA = NAME), join = st_intersects)

height.lga.means <- height.lga %>%
  st_drop_geometry() %>%
  group_by(LGA) %>%
  summarise(buildings = n(),
            overture_mean = mean(overture_height, na.rm = T),
            height_150m_mean = mean(height_150m, na.rm = T)) %>%
  ungroup() %>%
  mutate(difference = height_150m_mean - overture_mean) %>%
  arrange(LGA) %>%
  # omit small numbers (eg baw baw 3)
  filter(buildings > 10)

write.csv(height.lga.means, "./GIS/building height compare LGA means.csv", row.names = )


