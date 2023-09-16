# Using DTP freight volumes, allocate freight to links on an hourly basis

# There are two versions, controlled by the 'matsimOutput' variable:
# • matsim output - output is origin and destination addresses for trips, used 
#   as an input for matsim to run the routing
# • full output - this script runs the routing and calculates freight
#   volumes for each link

# Approach:
# Introduction (both versions)-
# • Process freight demand matrices to find the hourly volumes for each pair
#   of traffic zones
#   - Fractional demands are rounded; but if the rounded demands for the four
#     periods do not equal the rounded daily demand, then the rounded period 
#     demands are adjusted to match the daily demand 
#   - Each whole number of demand volume is treated as a separate trip
#   - Randomly allocate each trip to an hour within its demand period
# • Filter road network to links which are motorway, trunk, primary, 
#   secondary or tertiary
# • For each traffic zone, find the addresses within it that are contained in 
#   meshblocks likely to generate freight traffic (categorised as 'commercial', 
#   'industrial' or 'primary production')
#   - For zones with no such addresses, the centroid is used as the 'address' 
# For matsim output -
# • For each trip, select random addresses from its origin and destination zones
#   - If the same address is selected for both origin and destination, but another
#     selection would be possible (because one or both has more than once possible
#     address), then resample until the selected nodes are not the same
#   - Trips that may have the same origin and destination address (for example, 
#     because it is the centroid of the same zone) are not discarded (but will
#     be unroutable)
# • Find the X and Y coordinates of the relevant origin and destination addresses,
#   and write as output.
# For full output - 
# • Find the nearest network node for each address
#   - For zones where the centroid is used as the address, only nodes on 
#     motorway, trunk or primary roads are used (to avoid exaggerated demand
#     on secondary and tertiary roads)
# • For each trip, select random addresses from its origin and destination zones
#   - If the same node is selected for both origin and destination, but another
#     selection would be possible (because there is more than one address node in 
#     either or both of the origin and destination zones), then re-sample until
#     the selected nodes are not the same
#   - Trips where there is no choice but to select the same node are 
#     discarded (for example, if both origin and destination are in the same
#     zone, and it is a zone  where the centroid is used as the address)
# • Find the fastest route (using length/freespeed) on the filtered road network 
#   for each trip, expressed as the path of the links making up the trip
#   - Where multiple trips share a single pair of O/D nodes, the route is
#     calculated only once, and the results joined to each trip
# • Tally the number of trips using each link, for each hour; and sum the hourly
#   totals for each link to produce daily totals; and write as output


library(dplyr)
library(sf)
library(fs)
library(igraph)
library(ggplot2)
library(doSNOW) # like doParallel, but allows progress reporting
library(parallel)
library(foreach)
library(tidyr)  # pivoting

dir_walk(path="./functions/",source, recurse=T, type = "file")

PROJECT.CRS <- 28355

matsimOutput <- T  # change to F if full output required


# 1 Load freight, zones, meshblocks, addresses and network ----
# -----------------------------------------------------------------------------#
# From DTP metadata: Vehicle demand is aggregate of VITM time periods defined as:
# - AM peak: 7am to 9am (2 hours)
# - Inter-peak: 9am to 3pm (6 hours)
# - PM peak: 3pm to 6pm (3 hours)
# - Off-peak: 6pm to 7am the following day (13 hours)
freight.AM <- 
  read.csv("../data/original/DTP freight/Y2018/HeavyVehicle_AM_Demand_Y2018_RC23v1_05.csv.gz")
freight.IP <- 
  read.csv("../data/original/DTP freight/Y2018/HeavyVehicle_IP_Demand_Y2018_RC23v1_05.csv.gz")
freight.PM <- 
  read.csv("../data/original/DTP freight/Y2018/HeavyVehicle_PM_Demand_Y2018_RC23v1_05.csv.gz")
freight.OP <- 
  read.csv("../data/original/DTP freight/Y2018/HeavyVehicle_OP_Demand_Y2018_RC23v1_05.csv.gz")

zones <- read_zipped_GIS(zipfile = "../data/original/DTP freight/_Spatial.zip",
                        file = "/VITM2974_Metro_Reference_v3_AGD66_AMG55.shp") %>%
  st_transform(PROJECT.CRS)

meshblocks <- 
  read_zipped_GIS(zipfile = "../data/original/1270055001_mb_2016_vic_shape.zip") %>%
  st_transform(PROJECT.CRS)

addresses<- read.table("../data/original/VIC_ADDRESS_DEFAULT_GEOCODE_psv.psv",
                       sep = "|", header = TRUE) %>%
  # crs is GDA 94 (see section 5.1 of 'G-NAF Product Description.pdf' within
  # 'nov18_gnaf_pipeseparatedvalue_20181119200719.zip')
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = st_crs(4283)) %>%
  st_transform(PROJECT.CRS)

links <- st_read("../data/processed/network.sqlite", layer = "links") %>%
  # filter to main roads only, tertiary and above
  filter(highway %in% c("motorway", "motorway_link", "trunk", "trunk_link",
                        "primary", "primary_link", "secondary", "secondary_link",
                        "tertiary", "tertiary_link"))
nodes <- st_read("../data/processed/network.sqlite", layer = "nodes")

# keep just the largest connected network
network <- largestConnectedComponent(nodes, links)
network.nodes <- network[[1]]
network.links <- network[[2]]


# 2 Process demand matrices and allocate trips to hours ----
# -----------------------------------------------------------------------------#
# combine freight matrices
freight.combined <- freight.AM %>%
  rename(Demand.am = Demand) %>%
  full_join(freight.IP %>%rename(Demand.ip = Demand), by = c("Origin", "Destination"),) %>%
  full_join(freight.PM %>%rename(Demand.pm = Demand), by = c("Origin", "Destination")) %>%
  full_join(freight.OP %>%rename(Demand.op = Demand), by = c()) %>%
  mutate(Demand.total = Demand.am + Demand.ip + Demand.pm + Demand.op)

# round and filter to non-zero entries
freight <- adjustedRoundedFreight(freight.combined)

# write/reload (to avoid need to recalculate rounding)
# saveRDS(freight, "../data/processed/adjusted_rounded_freight.rds")
# freight <- readRDS("../data/processed/adjusted_rounded_freight.rds")

# pivot to longer form, with 1 row for each trip, and allocate to hours
freight.trips <- freight %>%
  dplyr::select(-Demand.total) %>%
  
  # pivot into long form, creating a 'period' column which is am/ip/pm/op demand
  pivot_longer(cols = starts_with("Demand"), names_to = "period") %>%
  mutate(period = gsub("Demand.", "", period)) %>%
  
  # remove the origin/destination/period groups with zero demand
  filter(value > 0) %>%
  
  # create a number of individual rows for each origin/destination based on 'value'
  group_by(Origin, Destination, period) %>%
  slice(rep(1:n(), each = value)) %>%
  ungroup() %>%
  dplyr::select(-value) %>%
  
  # allocate hour of day, randomly according to periods
  rowwise() %>%
  mutate(hour = case_when(
    period == "am" ~ sample(c(7, 8), size = 1),
    period == "ip" ~ sample(c(9, 10, 11, 12, 13, 14), size = 1),
    period == "pm" ~ sample(c(15, 16, 17), size = 1),
    period == "op" ~ sample(c(18, 19, 20, 21, 22, 23, 0, 1, 2, 3, 4, 5, 6), 
                            size = 1))) %>%
  ungroup()


# 3 Locate addresses ----
# -----------------------------------------------------------------------------#
# find addresses located in meshblocks for 'freight' purposes
freight.meshblock.addresses <- addresses %>%
  # intersect with addresses and filter to commercial, industrial and primary production
  st_intersection(meshblocks) %>%
  filter(MB_CAT16 %in% c("Commercial", "Industrial", "Primary Production")) %>%
  # intersect with zones
  st_intersection(zones) %>%
  # remove Z dimension which is present in addresses(so can bind with centroids)
  st_zm()

# find zones which lack freight meshblock addresses
TZNs <- zones$TZN %>% unique()
freight.meshblock.address.TZNs <- freight.meshblock.addresses$TZN %>% unique()
zones.without.addresses <- TZNs[!(TZNs %in% freight.meshblock.address.TZNs)]

# find centroids for zones without freight meshblock addresses
centroid.addresses <- zones %>%
  filter(TZN %in% zones.without.addresses) %>%
  st_centroid()

# 4 Produce required outputs
# -----------------------------------------------------------------------------#
if (outputMatsim) {
  
  ## 4.1 Matsim output ----
  ## ------------------------------------#
  freight.trip.ods <- 
    allocateAddresses(freight.trips, freight.meshblock.addresses, centroid.addresses,
                      freight.meshblock.address.TZNs)
  
  freight.trip.ods <- freight.trip.ods %>%
    rename(orig_zn = Origin, dest_zn = Destination)
  
  write.csv(freight.trip.ods, "../data/processed/freight_trips_with_ODs.csv",
            row.names = FALSE)
  
} else {

  ## 4.2 Full output ----
  ## ------------------------------------#
  ### 4.2.1  Find nearest nodes to freight addresses ----
  ### -----------------------------------#
  # find nearest nodes for addresses in freight meshblocks 
  # (note - st_nearest_feature finds index of nearest feature; then  index is 
  # used to find the id's of the relevant features) 
  n.node <- network.nodes$id[st_nearest_feature(freight.meshblock.addresses, network.nodes)]
  freight.meshblock.addresses <- cbind(freight.meshblock.addresses, n.node) 
  
  # find nearest nodes for zone centroids where no freight meshblock addresses - 
  # limited to major highways, to avoid exaggerating traffic on secondary or 
  # tertiary roads that may be near centroid
  major.network.links <- network.links %>%
    filter(highway %in% c("motorway", "motorway_link", "trunk", "trunk_link",
                          "primary", "primary_link"))
  major.network.nodes <- network.nodes %>%
    filter(id %in% major.network.links$from_id | id %in% major.network.links$to_id)
  n.node <- major.network.nodes$id[st_nearest_feature(centroid.addresses, major.network.nodes)]
  centroid.addresses <- cbind(centroid.addresses, n.node) 
  
  # combine
  freight.addresses <- bind_rows(freight.meshblock.addresses,
                                 centroid.addresses)
  
  # write/reload (to avoid relocating nearest nodes) 
  # saveRDS(freight.addresses, "../data/processed/freight_addd.rds")
  # freight.addresses <- readRDS("../data/processed/freight_trips_nodes.rds")
  
  # write to sqlite for review 
  # st_write(freight.addresses, "../data/processed/freight_addresses.sqlite", 
  #          delete_dsn = TRUE)
  
  # randomly allocate origins and destinations to freight trips (about an hour)
  freight.trips.nodes <- allocateNodes(freight.trips, freight.addresses)
  # freight.trips.nodes <- allocateNodes(freight.trips[1:1000,], freight.addresses)
  
  # write/reload (to avoid need to reallocate random nodes) 
  # saveRDS(freight.trips.nodes, "../data/processed/freight_trips_nodes.rds")
  # freight.trips.nodes <- readRDS("../data/processed/freight_trips_nodes.rds")
  
  
  ### 4.2.2 Find routes for each freight trip ----
  ### ---------------------------------#
  # find unique OD pairs (to avoid calculating identical routes more than once)
  freight.trip.OD.pairs <- freight.trips.nodes %>%
    distinct(orig.node, dest.node) %>%
    # omit any where orig and dest nodes are the same
    filter(orig.node != dest.node)
  
  # create the graph used for routing
  # prepare links and nodes (links need to be two-way; from_id and to_id to 
  # be first; requires weight field)
  g.links <- network.links %>%
    st_drop_geometry() %>%
    # weight: time
    mutate(weight = length/freespeed) %>%
    dplyr::select(from_id, to_id, id, is_oneway, weight)  # shortest time
  
  g.links.twoway.reversed <- g.links %>%
    filter(is_oneway == 0) %>%
    mutate(orig_from_id = from_id,
           orig_to_id = to_id) %>%
    mutate(from_id = orig_to_id,
           to_id = orig_from_id) %>%
    dplyr::select(names(g.links))
  
  g.links <- rbind(g.links, g.links.twoway.reversed) %>%
    dplyr::select(-is_oneway)
  
  g.nodes <- network.nodes %>%
    st_drop_geometry()
  
  # create the graph
  g <- graph_from_data_frame(g.links, directed = T, vertices = g.nodes)
  
  
  # setup for parallel processing (using doSNOW instead of doParallel, because
  # allows progress reporting)
  cores <- detectCores()
  cluster <- parallel::makeCluster(cores)
  doSNOW::registerDoSNOW(cluster)
  
  # report
  print(paste(Sys.time(), "| Using parallel processing with", cores, 
              "cores to find routes for", nrow(freight.trip.OD.pairs), 
              "origin/destination pairs"))
  
  # set up progress reporting 
  pb <- txtProgressBar(max = nrow(freight.trip.OD.pairs), style = 3)
  progress <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress = progress)
  
  # find link id's for shortest route between each OD pair (about 10 hours)
  freight.trip.OD.pair.outputs <- 
    foreach(i = 1:nrow(freight.trip.OD.pairs),
            # foreach(i = 1:10,
            .packages = c("dplyr", "sf", "igraph"), 
            .combine = rbind,
            # .verbose = TRUE,
            .options.snow = opts) %dopar% {
              
              # begin with output row, which is the current row of selected.OD.pairs                
              output.row <- freight.trip.OD.pairs[i, ]
              
              # shortest route
              shortest <- shortest_paths(g, 
                                         from = as.character(output.row$orig.node),
                                         to = as.character(output.row$dest.node),
                                         mode = "out",
                                         output = "epath")
              
              # complete details if shortest route exists, otherwise NA
              if (length(shortest$epath[[1]]) > 0) {
                
                link_ids <- edge_attr(g, "id", index = shortest$epath[[1]]) %>%
                  toString(.)
                
              } else {
                link_ids <- NA
              }
              
              # add shortest route details to output row
              output.row <- c(output.row,
                              link_ids = link_ids)
              
              # convert the output row (which has become a list) to a dataframe
              output.row <- as.data.frame(output.row)
              
              # Return the output
              return(output.row)
              
            }                                                    
  
  # end parallel processing
  close(pb)
  stopCluster(cluster)
  
  # result is a table of O/D pairs, with link IDs in a comma-separated string
  
  freight.trips.routes <- freight.trips.nodes %>%
    left_join(., freight.trip.OD.pair.outputs, by = c("orig.node", "dest.node")) %>%
    # remove any without links - either because orig and dest nodes were the same,
    # or because no routes could be found
    filter(!is.na(link_ids))
  
  # longer table, with one row per link for each trip's link ids
  freight.trips.routes.long <- freight.trips.routes %>%
    mutate(link_ids = strsplit(link_ids, ",\\s*")) %>%
    unnest(link_ids) %>%
    pivot_longer(cols = link_ids, names_to = NULL, values_to = "link_id")
  
  
  ### 4.2.3 Tally freight volumes for each link ----
  ### ---------------------------------#
  # group  by link and hour, and tally, add column for daily total
  link.volumes <- freight.trips.routes.long %>%
    # tally number of trips per link and hour
    group_by(link_id, hour) %>%
    summarise(n = n()) %>%
    ungroup() %>%
    # set up hourly columns
    arrange(hour) %>%
    pivot_wider(names_from = hour, values_from = n, names_prefix = "hr_") %>%
    # convert NAs to 0
    mutate(across(starts_with("hr_"), ~ifelse(is.na(.), 0, .))) %>%
    # add daily total
    mutate(hr_total = rowSums(across(starts_with("hr_"), na.rm = TRUE)))
  
  write.csv(link.volumes, "../data/processed/freight_volumes.csv", row.names = FALSE)
 
}

