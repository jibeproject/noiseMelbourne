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
library(RANN)  # fast nearest neighbour (points)
library(terra)

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
# Melbourne
melb.lga.buildings <- read_zipped_GIS(zipfile = "../data/original/2018-building-footprints.zip")

# Yarra
yarra <- read_zipped_GIS(zipfile = "../data/original/LGAs.zip", 
                         subpath = "/mga94_55/esrishape/whole_of_dataset/victoria/VMADMIN/") %>%
  filter(NAME == "YARRA")
yarra.bed.elevation <- rast("../data/processed/yarra_buildings/yarra_building_bed_elevation.tif")
yarra.dem <- rast("../data/original/yarra_buildings/mga55_yarra_1m_dtm.tif")

# Manningham
mann.lga.buildings <- read_zipped_GIS(zipfile = "../data/original/manningham_building_footprints.zip")

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

# add height, if not already specified in Overture data, using Melbourne, Yarra
# (prepared as above) or Manningham building height
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
              # omit any NAs, and any below zero (instead, they are determined by closest buildings)
              filter(!is.na(yarra_lga_height) & yarra_lga_height > 0),
            by = "id") %>%
  
  # height is overture height if specified, or else melb, yarra or manningham height if specified
  mutate(height = ifelse(!is.na(overture_height), overture_height, 
                         ifelse(!is.na(melb_lga_height), melb_lga_height, 
                                ifelse(!is.na(yarra_lga_height), yarra_lga_height, 
                                       mann_lga_height))))

# # check how many still lack height
# no.overture.height <- nrow(buildings.specified.height %>% filter(!is.na(overture_height))) /
#   nrow(buildings.specified.height)
# no.overture.height  # 65.31% have height
# no.overture.lga.height <- nrow(buildings.specified.height %>% filter(!is.na(height))) /
#   nrow(buildings.specified.height)
# no.overture.lga.height  # 66.61% have height (compare previous results:
# # 66.05% with just adding melb; 66.43% with just adding melb and yarra)

# where height not specified, calculate as average of 3 nearest buildings

# building centroids with and without height (nb RANN needs points not polygons)
buildings.with.height <- buildings.specified.height %>%
  filter(!is.na(height)) 
centroids.with.height <- buildings.with.height %>%
  st_centroid() %>%
  st_coordinates()
buildings.without.height <- buildings.specified.height %>%
  filter(is.na(height)) 
centroids.without.height <- buildings.without.height %>%
  st_centroid() %>%
  st_coordinates()

# find the 3 nearest neighbors for each centroid without height (returns list
# containing nn.idx (matrix of near neighbour indices) and nn.dists (matrix
# of near neighbour euclidean distances))
nn <- RANN::nn2(centroids.with.height, centroids.without.height, k = 3)

# compute the average height (ie average of heights for the 3 indices
# for each building, contained in nn.idx)
heights <- apply(nn$nn.idx, 1, function(indices) {
  mean(buildings.with.height$height[indices], na.rm = TRUE)
})

# add the computed heights as a new column
buildings.without.height$height <- heights

# re-combine the two dataframes
buildings.height <- bind_rows(buildings.with.height,
                              buildings.without.height)


# 4 Write output ----
# -----------------------------------------------------------------------------#
st_write(buildings.height, "../data/processed/melbourne_buildings_processed.sqlite",
         delete_layer = TRUE)
