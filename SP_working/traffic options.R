# review of options fr traffic data

# Notes
# - none of these options ultimately used; replaced instead by DOT freight data
# - files are located in 'WP 5 > Melbourne datasets > other traffic data' at 
#   https://rmiteduau.sharepoint.com/sites/JIBEUKAUS/Shared%20Documents/Forms/AllItems.aspx ;
#   this script assumes they have been downloaded into "../data" folder


library(dplyr)
library(sf)
library(fs)
dir_walk(path="./functions/",source, recurse=T, type = "file")


# 1 Investigate traffic datasets ----
# -----------------------------------------------------------------------------#
## Typical Traffic Daily Volume Profile ----
TTDVP <- read.csv("../data/2018.csv", header = FALSE)
head(TTDVP)

# From https://discover.data.vic.gov.au/dataset/typical-daily-traffic-volume-profile
# 15 minute traffic volumes, available for 2018 or 2019, no truck volumes
# start and end lat/lon only; could join to spatial data in HTV


## Typical Hourly Traffic Volumes ----
THTV <- read.csv("../data/TYPICAL_HOURLY_VOLUME_DATA.csv")
head(THTV)

# Not available online any more, but metadata at 
# https://data.vicroads.vic.gov.au/Metadata/Typical%20Hourly%20Traffic%20Volumes.html
# hourly traffic volumes, for 2015, no truck volumes


## Homologous Traffic Volume ----
HTV <- read_zipped_GIS(zipfile = "../data/homogenous_traffic_flow.zip")
head(HTV)

# From https://discover.data.vic.gov.au/dataset/homogeneous-traffic-volume-network
# Spatial data for links for which 15 min or hourly data has been provided
# note that field 'HMGNS_FLOW' is a road number, not a traffic volume figure

dup.htv <- HTV %>%
  st_drop_geometry() %>%
  group_by(HMGNS_FLOW) %>%
  summarise(n = n()) %>%
  filter(n > 1)  # 38 obs


## Traffic Volume for Freeways and Arterial Roads ----
TVFAR <- read_zipped_GIS(zipfile = "../data/Traffic_Volume.zip")
head(TVFAR)

# From https://discover.data.vic.gov.au/dataset/traffic-volume-for-freeways-and-arterial-roads
# Daily traffic volumes, including trucks (and a variety of other vehicle types)
# Year unclear - possibly 2020 (covid issues); might need to enquire from provider
# Note issues with percentages explored below; would probably calculate
# own percentages based on volumes provided

## check whether PER_TRUCKS_AADT is sum of HV and LV - it's not always
chk <- TVFAR %>%
  mutate(chk = PER_TRUCKS - (PER_HV + PER_LV)) %>%
  dplyr::select(1:5, PER_TRUCKS, PER_HV, PER_LV, chk)

# Does TTDVP or THTV include a lot of roads that are missing from TVFAR?
TVFAR.id <- TVFAR$HMGNS_LNK_ %>% unique  # 7404
TTDVP.id <- TTDVP$V3 %>% unique  # 6385
THTV.id <- THTV$HMGNS_LNK_ID %>% unique # 5442

# some random checks suggest they're the same
TVFAR[TVFAR$HMGNS_LNK_ == 1000, ]
TTDVP[TTDVP$V3 == 1000, ]
THTV[THTV$HMGNS_LNK_ID == 1000, ]

TTDVP.id[!(TTDVP.id %in% TVFAR.id)]  # 35
THTV.id[!(THTV.id %in% TVFAR.id)] # 22

TTDVP %>% 
  filter(!(V3 %in% TVFAR.id)) # Mix of fwy's and other roads

THTV %>%
  filter(!(HMGNS_LNK_ID %in% TVFAR.id))  # all Tullamarine Fwy

# do percentages add up? No, not always
chk2 <- TVFAR %>%
  st_drop_geometry() %>%
  dplyr::select(HMGNS_LN_1, ALLVEHS_AA, TRUCKS_AAD, PER_TRUCKS, PER_HV, PER_LV) %>%
  mutate(calc = round(TRUCKS_AAD / ALLVEHS_AA, 2), 
         diff = round(PER_TRUCKS - calc, 2)) %>%
  group_by(diff) %>%
  summarise(n = n())



# 2 Starting code for joining TVFAR to links ----
# -----------------------------------------------------------------------------#
# Note - not proceeded with, once provided with DOT freight model

# This is based on agent based modeeling/calibration-validation (so see that),
# but not completed.  The approach in that file was:-
# - filter Network to freeways and main roads
# - select links in a 300m buffer around each road of interest, and dupicate any one way
# - filter out any that are in wrong direction
# - filter out any where azimuth (bearing) is >17.5 degrees away from road
# - select the remaining link closest to the road midpoint
# - add the link row number to the car data
# - also add the link's from and to node id's


# Read in traffic data, and join to network

library(dplyr)
library(sf)
library(fs)
library(lwgeom)
library(nngeo)  # for st_azimuth
dir_walk(path="./functions/",source, recurse=T, type = "file")

library(ggplot2)

PROJECT.CRS <- 28355
AZ.TOL  <- 17.5 # azimuth tolerance (to filter out links > 17.5 degrees from road azimuth)



## 1 Read in traffic and network ----
## -----------------------------------------------------------------------------#


# load traffic data and study area
traffic <- read_zipped_GIS(zipfile = "../data/Traffic_Volume.zip") %>% # see above for location of this file
  st_transform(PROJECT.CRS)

study.area <- st_read("../data/region_buffer.sqlite")  # note - already in PROJECT.CRS

# intersect traffic with study area
traffic.study.area <- st_intersection(traffic, study.area)

# ggplot() + geom_sf(data = traffic.study.area)


# load network
# read in links and nodes ### DO I NEED NODES?
links <- st_read("../data/network.sqlite", layer = "links") %>%
  # filter to main roads only ( don't include trunk_link, primary_link, secondary_link)
  filter(highway %in% c("motorway", "motorway_link", "trunk", "primary", "secondary"))

nodes <- st_read("../data/network.sqlite", layer = "nodes") %>%
  # filter to nodes used in links, to remove any disconnected (not really necessary)
  filter(id %in% links$from_id | id %in% links$to_id)


## 2 Find links corresonding to traffic roads ----
## -----------------------------------------------------------------------------#
traffic.table <- traffic.study.area %>%
  # temporarily remove some columns for checking
  .[, c(1:20)] %>%
  
  # add columns for link_id and node from_ and to_ id's 
  mutate(link_id = "NA", from_id = "NA", to_id = "NA")


# for (i in 1:nrow(traffic.table)) {
for (i in 1:10) {
  
  direction <- traffic.table[i, "FLOW"] %>%
    st_drop_geometry() %>%
    stringr::word(., 1)  # first word in the "flow" column for the road: EAST, WEST, NORTH or SOUTH
  
  # find azimuth of road
  point1 <- lwgeom::st_startpoint(traffic.table[i, ])
  point2 <- lwgeom::st_endpoint(traffic.table[i, ])
  azimuth <- st_azimuth(point1, point2)
  
  # buffer the road to 300m
  local.area <- st_buffer(traffic.table[i, ], 300)
  
  # find links in the local area, and duplicate in opposite direction if two way 
  # [comment out duplication if links have already been duplicated for one-way network]
  filtered.links <- links %>%
    filter(st_intersects(GEOMETRY, local.area, sparse = FALSE)) %>% 
    mutate(correct_dir = 0, correct_az = 0)
  
  potential.links.oneway <- filtered.links %>%
    filter(is_oneway == 1)
  
  potential.links.twoway <- filtered.links %>%
    filter(is_oneway == 0)
  
  potential.links.twoway.reversed <- potential.links.twoway %>%
    mutate(new.from = to_id, new.to = from_id) %>%
    mutate(from_id = new.from, to_id = new.to)
  
  # more extended version in case we need x and y (but we don't)
  # potential.links.twoway.reversed <- potential.links.twoway %>%
  #   mutate(new.from = to_id, new.to = from_id,
  #          new.fromx = fromx, new.tox = tox,
  #          new.fromy = tox, new.toy = toy) %>%
  #   mutate(from_id = new.from, to_id = new.to,
  #          fromx = new.fromx, tox = new.tox,
  #          fromy = new.fromy, toy = new.toy)
  
  
  potential.links <- dplyr::bind_rows(potential.links.oneway,
                                      potential.links.twoway,
                                      potential.links.twoway.reversed) 
  
  
  # filter to links in correct direction and with correct azimuth
  # j=1
  for (j in 1:nrow(potential.links)) {
    link <- potential.links[j, ]
    
    # set correct_dir to 1 if correct direction
    startpoint <- nodes[nodes$id == link$from_id, ]
    endpoint <- nodes[nodes$id == link$to_id, ]
    if (# startpoint has lower easting coordinate
      direction == "EAST" & startpoint$x < endpoint$x | 
      # startpoint has higher easting coordinate
      direction == "WEST" & startpoint$x > endpoint$x | 
      # startpoint has lower northing coordinate
      direction == "NORTH" & startpoint$y < endpoint$y | 
      # startpoint has higher northing coordinate
      direction == "SOUTH" & startpoint$y > endpoint$y) {
      potential.links[j, "correct_dir"] <- 1
    }
    
    # find azimuth for link
    link.az <- st_azimuth(startpoint, endpoint)
    potential.links[j, "azimuth"] <- link.az  # not used - just for debugging
    
    # if  link.az is within tolerance, set "correct_az" to 1
    if (azimuth-AZ.TOL >= 0 & azimuth+AZ.TOL <= 360) {
      if (link.az > azimuth-AZ.TOL & link.az < azimuth+AZ.TOL) {
        potential.links[j, "correct_az"] <- 1
      }
    } else if (azimuth-AZ.TOL < 0) {
      if (link.az > azimuth-AZ.TOL+360 | link.az < azimuth+AZ.TOL) {
        potential.links[j, "correct_az"] <- 1
      }
    } else if (azimuth+AZ.TOL > 360) {
      if (link.az > azimuth-AZ.TOL | link.az < azimuth+AZ.TOL-360) {
        potential.links[j, "correct_az"] <- 1
      }
    }
  }
  
  ## Below here is from 'agent based modelling/calibration-validation'
  ## Would need to be updated; 'carData' is 'traffic.table'
  potential.links <- potential.links %>%
    filter(correct_dir == 1 & correct_az == 1)
  
  # get midpoint of road (contained in the data)
  # consider whether you actually want the midpoint - or are you finding all links
  # that align with the relevant traffic road?
  midpoint <- carData[i, ] %>% 
    st_drop_geometry() %>%
    st_as_sf(coords = c("midpnt_lon", "midpnt_lat"), remove = F, crs = 4326) %>%
    st_transform(28355)
  
  # find filtered link closest to the midpoint (st_nearest_feature returns the index)
  closest.link <- potential.links[st_nearest_feature(midpoint, potential.links), ]
  # carData <- carData %>% st_as_sf()
  # complete link_id with row number of closest.link
  carData$link_id[i] <- as.character(closest.link$link_id)
  
  # complete from_id and to_id with node numbers for closest link
  carData$from_id[i] <- closest.link$from_id
  carData$to_id[i] <- closest.link$to_id
  
return(carData)
# 'return' probably isn't right - need to add the required details to traffic.table
  
}  

# Would need to determine how best to join results to network