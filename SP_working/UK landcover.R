# Review landcover types in UK ordnance survey data

# Notes
# - reviewed in order to compare to the field types being used for UK G coding
# - file is located in 'WP 5 > Melbourne datasets > UK landcover example' at 
#   https://rmiteduau.sharepoint.com/sites/JIBEUKAUS/Shared%20Documents/Forms/AllItems.aspx ;
#   this script assumes it has been downloaded into "../data" folder

library(sf)  # note also need library rgdal installed for section 1
library(dplyr)

# 1 Read in Ordnance Survey layer ----
# -----------------------------------------------------------------------------#

# note that 'TARGETFILE' is inside 'GSFILE', which is in turn inside 'ZIPFILE';
# the layer of the TARGETFILE to be read is 'LAYER'; reads using driver 'GML'

ZIPFILE <- "../data/topography-england-gml.zip"
GZFILE <- "5436275-SX9090.gz"
TARGETFILE <- "5436275-SX9090"
LAYER <- "TopographicArea"

# Create a temporary folder
temp_folder <- tempdir()

# Unzip the 'topography-england-gml.zip' file to the temporary folder
zip_file <- ZIPFILE
unzip(zip_file, exdir = temp_folder)

# Unzip the .gz file contained within the zipfile
gz_file <- file.path(temp_folder, GZFILE)
unzipped_file <- file.path(temp_folder, TARGETFILE)  # Name of the unzipped file

# Open the .gz file and read its contents using readLines
gz_con <- gzfile(gz_file, "rb")
gz_content <- readLines(gz_con)
close(gz_con)

# Write the contents to a new file without the '.gz' extension
writeLines(gz_content, unzipped_file)

# Path to the 'TopographicArea' layer file
path_to_topographic_area <- file.path(temp_folder, TARGETFILE)

# Read the 'TopographicArea' layer using sf::st_read()
topographic_area <- st_read(dsn = path_to_topographic_area, layer = LAYER)

# Clean up - Delete the main files created during unzipping and processing
files_to_remove <- c(gz_file, unzipped_file)
file.remove(files_to_remove)


# 2 Investigate 'themes' and 'descriptive groups' ----
# -----------------------------------------------------------------------------#
themes <- topographic_area$theme %>% unlist() %>% unique() %>% sort()
themes
# [1] "Buildings"                "Heritage And Antiquities" "Land"                    
# [4] "Rail"                     "Roads Tracks And Paths"   "Structures"              
# [7] "Water"     

desc <- topographic_area$descriptiveGroup %>% unlist() %>% unique() %>% sort()
desc
# [1] "Building"            "General Surface"     "Glasshouse"          "Historic Interest"  
# [5] "Inland Water"        "Landform"            "Natural Environment" "Path"               
# [9] "Rail"                "Road Or Track"       "Roadside"            "Structure"          
# [13] "Tidal Water"         "Unclassified" 

combinations <- topographic_area %>%
  st_drop_geometry() %>%
  group_by(theme, descriptiveGroup) %>%
  summarise(n = n())
  
combinations_rev <- topographic_area %>%
  st_drop_geometry() %>%
  group_by(descriptiveGroup, theme) %>%
  summarise(n = n())

