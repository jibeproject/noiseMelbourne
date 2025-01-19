# make zipfile of SA3s for Greater Melbourne

library(dplyr)
library(sf)
library(fs)

dir_walk(path="./functions/",source, recurse=T, type = "file")

PROJECT.CRS <- 28355


# read in meshblocks
meshblocks <- 
  read_zipped_GIS(zipfile = "../data/original/1270055001_mb_2016_vic_shape.zip") %>%
  st_transform(PROJECT.CRS)

# get Greater Melbourne SA3s
melb.SA3s <- meshblocks %>%
  filter(GCC_NAME16 == "Greater Melbourne") %>%
  group_by(SA3_CODE16, SA3_NAME16, SA4_CODE16, SA4_NAME16, 
             GCC_CODE16, GCC_NAME16, STE_CODE16, STE_NAME16) %>%
  summarise(.) %>%
  ungroup() %>%
  arrange(SA3_NAME16)

# melb.SA3s$SA3_NAME16  

# write output as zipped shapefile
write_zipped_shp(sf.obj = melb.SA3s, 
                 shp.name = "melb_sa3",
                 shp.location = "../data/processed/melb_sa3.zip")
