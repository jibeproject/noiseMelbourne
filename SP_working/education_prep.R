# this extracts data from 
# au-govt-abs-abs-data-by-region-education-and-employment-asgs-sa2-2011-2019-sa2-2016.json


# install.packages("geojsonsf")
library(geojsonsf)
library(sf)

educ.file <- 
  "../data/original/au-govt-abs-abs-data-by-region-education-and-employment-asgs-sa2-2011-2019-sa2-2016.json"

educ.sf <- geojson_sf(educ.file)

st_write(educ.sf, "../data/processed/education_sa2.sqlite")
write.csv(educ.sf %>%
            st_drop_geometry(), 
          "../data/processed/education_sa2.csv", row.names = FALSE)
