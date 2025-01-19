# various checks for freight data (note - some files mentioned below
# may no longer be held, eg 'new' zones and addresses)


library(dplyr)
library(sf)
library(fs)
library(igraph)
library(ggplot2)
library(doSNOW) # like doParallel, but allows progress reporting
library(parallel)
library(foreach)

dir_walk(path="./functions/",source, recurse=T, type = "file")


# 1 Compare zones ----
# -----------------------------------------------------------------------------#
# 'old' is as supplied by DTP, in old AGD 1966 projection; 'new' is converted by Alan
zones.old <- read_zipped_GIS(zipfile = "../data/original/DTP freight/_Spatial.zip",
                         file = "/VITM2974_Metro_Reference_v3_AGD66_AMG55.shp")
zones.new <- st_read("../data/original/DTP freight/vitm_regions.sqlite")  # EPSG:28355


freight.AM <- read.csv("../data/original/DTP freight/Y2018/HeavyVehicle_AM_Demand_Y2018_RC23v1_05.csv.gz")
freight.IP <- read.csv("../data/original/DTP freight/Y2018/HeavyVehicle_IP_Demand_Y2018_RC23v1_05.csv.gz")
freight.PM <- read.csv("../data/original/DTP freight/Y2018/HeavyVehicle_PM_Demand_Y2018_RC23v1_05.csv.gz")
freight.OP <- read.csv("../data/original/DTP freight/Y2018/HeavyVehicle_OP_Demand_Y2018_RC23v1_05.csv.gz")

freight.zones <- c(freight.AM$Origin %>% unique(),
                   freight.IP$Origin %>% unique(),
                   freight.PM$Origin %>% unique(),
                   freight.OP$Origin %>% unique(),
                   freight.AM$Destination %>% unique(),
                   freight.IP$Destination %>% unique(),
                   freight.PM$Destination %>% unique(),
                   freight.OP$Destination %>% unique()) %>%
                     unique() %>% sort()

# does either zone file contain zone numbers not used in the freight demand data?
zones.old$TZN[!(zones.old$TZN %in% freight.zones)]
# numeric(0)
zones.new$tzn[!(zones.new$tzn %in% freight.zones)]
# [1] 2975 2976 2977 2978 2979 2980 2981 2982 2983 2984 2985 2986 2987 2988 2989
# [16] 2990 2991 2992 2993 2994 2995 2996 2997 2998 2999 3000

# does the freight demand data contain zone numbers not used in either zone file?
freight.zones[!(freight.zones %in% zones.old$TZN)]
# [1]   32   39   45   62  153  412  413 1609 2113 2114 2132 2284 2285 2290 2291
freight.zones[!(freight.zones %in% zones.new$tzn)]
# [1]   39   45   62  123  146  411  412  731  900 1226 1578 1608 1825 2114 2115
# [16] 2119 2133 2151 2152 2153 2154 2155 2157 2158 2159 2160 2248 2285 2286 2287
# [31] 2288 2289


# 2 How much demand is in and out of Melbourne? ----
# -----------------------------------------------------------------------------#
# Assume we're using the 'old' zone file
study.area <- st_read("../data/processed/region_buffer.sqlite")

zones <- read_zipped_GIS(zipfile = "../data/original/DTP freight/_Spatial.zip",
                         file = "/VITM2974_Metro_Reference_v3_AGD66_AMG55.shp") %>%
  st_transform(st_crs(study.area))

# find the zone numbers that are inside and outside the study area (buffered region)
zones.melb <- st_filter(st_centroid(zones), study.area, .predicate = st_intersects) %>%
  pull(TZN)

zones.other <- zones$TZN[!zones$TZN %in% zones.melb]

# read in freight, and sum for total day
freight.AM <- read.csv("../data/original/DTP freight/Y2018/HeavyVehicle_AM_Demand_Y2018_RC23v1_05.csv.gz")
freight.IP <- read.csv("../data/original/DTP freight/Y2018/HeavyVehicle_IP_Demand_Y2018_RC23v1_05.csv.gz")
freight.PM <- read.csv("../data/original/DTP freight/Y2018/HeavyVehicle_PM_Demand_Y2018_RC23v1_05.csv.gz")
freight.OP <- read.csv("../data/original/DTP freight/Y2018/HeavyVehicle_OP_Demand_Y2018_RC23v1_05.csv.gz")

total.freight <- freight.AM %>%
  rename(Demand.am = Demand) %>%
  full_join(freight.IP %>%rename(Demand.ip = Demand), by = c("Origin", "Destination"),) %>%
  full_join(freight.PM %>%rename(Demand.pm = Demand), by = c("Origin", "Destination")) %>%
  full_join(freight.OP %>%rename(Demand.op = Demand), by = c()) %>%
  mutate(Demand.total = Demand.am + Demand.ip + Demand.pm + Demand.op)

# find in and out of Melbourne as percentages of total
# total demand
total.demand <- sum(total.freight$Demand.total)

# trips wholly within Melb
demand.within.melb <- total.freight %>%
  filter(Origin %in% zones.melb & Destination %in% zones.melb) %>%
  pull(Demand.total) %>%
  sum()

# trips starting or ending within Melb but ending or starting outside
demand.melb.other <- total.freight %>%
  filter((Origin %in% zones.melb & Destination %in% zones.other) |
           (Origin %in% zones.other & Destination %in% zones.melb)) %>%
  pull(Demand.total) %>%
  sum()

# trips both starting and ending outside Melb (but may still cross Melb)
demand.outside.melb <- total.freight %>%
  filter(Origin %in% zones.other & Destination %in% zones.other) %>%
  pull(Demand.total) %>%
  sum()

# above trip numbers as percentages of total
demand.within.melb.pct <- round(demand.within.melb / total.demand * 100, 1)
demand.melb.other.pct <- round(demand.melb.other / total.demand * 100, 1)
demand.outside.melb.pct <- round(demand.outside.melb / total.demand * 100, 1)

demand.within.melb.pct  # 87%
demand.melb.other.pct  # 6.1%
demand.outside.melb.pct  # 6.9%

# and note that interstate trips are presumably not counted at all


# 3 Numbers of OD trips to calculate ----
# -----------------------------------------------------------------------------#
# Assume we're using the 'old' zone file
study.area <- st_read("../data/processed/region_buffer.sqlite")

zones <- read_zipped_GIS(zipfile = "../data/original/DTP freight/_Spatial.zip",
                         file = "/VITM2974_Metro_Reference_v3_AGD66_AMG55.shp") %>%
  st_transform(st_crs(study.area))

# find the zone numbers that are inside and outside the study area (buffered region)
zones.melb <- st_filter(st_centroid(zones), study.area, .predicate = st_intersects) %>%
  pull(TZN)

zones.other <- zones$TZN[!zones$TZN %in% zones.melb]


# read in freight, and sum for total day
freight.AM <- read.csv("../data/original/DTP freight/Y2018/HeavyVehicle_AM_Demand_Y2018_RC23v1_05.csv.gz")
freight.IP <- read.csv("../data/original/DTP freight/Y2018/HeavyVehicle_IP_Demand_Y2018_RC23v1_05.csv.gz")
freight.PM <- read.csv("../data/original/DTP freight/Y2018/HeavyVehicle_PM_Demand_Y2018_RC23v1_05.csv.gz")
freight.OP <- read.csv("../data/original/DTP freight/Y2018/HeavyVehicle_OP_Demand_Y2018_RC23v1_05.csv.gz")

total.freight <- freight.AM %>%
  rename(Demand.am = Demand) %>%
  full_join(freight.IP %>%rename(Demand.ip = Demand), by = c("Origin", "Destination"),) %>%
  full_join(freight.PM %>%rename(Demand.pm = Demand), by = c("Origin", "Destination")) %>%
  full_join(freight.OP %>%rename(Demand.op = Demand), by = c()) %>%
  mutate(Demand.total = Demand.am + Demand.ip + Demand.pm + Demand.op)

# total number of zones
length(total.freight$Origin %>% unique())  #2 974
length(total.freight$Destination %>% unique()) # 2974  

# total number of OD combinations
nrow(total.freight)  # 8844676


# total number of combinations with non-zero readings
nrow(total.freight %>% 
       filter(Demand.total > 0))  # 1992784

# total number of combinations with non-zero reading within Melbourne
nrow(total.freight %>%
       filter(Origin %in% zones.melb & Destination %in% zones.melb) %>%
       filter(Demand.total > 0))  # 1626047

# total number of combinations with  >= 0.5 trips in any period
nrow(total.freight %>%
       filter(Demand.am >= 0.5 | Demand.ip >= 0.5 |
                Demand.pm >= 0.5 | Demand.op >= 0.5))  # 50201

# total number of combinations with >= 0.5 trips in any period within Melbourne
nrow(total.freight %>%
       filter(Origin %in% zones.melb & Destination %in% zones.melb) %>%
       filter(Demand.am >= 0.5 | Demand.ip >= 0.5 |
                Demand.pm >= 0.5 | Demand.op >= 0.5))  # 46278

# two different rounding methods below. First is by period, second is by day.
# Assume 0.3 trips in each of AM, IP, PM and OP periods, total 1.2 for the day.
# First method rounds each period to zero, so zero in total.  Second method 
# rounds daily total of 1.2 to 1 trip (which needs to be allocated to an hour
# in the day, presumably an hour in the period with the highest hourly rate).

# total number of trips, rounded
sum(round(total.freight$Demand.am)) +
  sum(round(total.freight$Demand.ip)) +
  sum(round(total.freight$Demand.pm)) +
  sum(round(total.freight$Demand.op))  # 200835


# total number of trips, rounded, within Melbourne
freight.melb <- total.freight %>%
  filter(Origin %in% zones.melb & Destination %in% zones.melb)

sum(round(freight.melb$Demand.am)) +
  sum(round(freight.melb$Demand.ip)) +
  sum(round(freight.melb$Demand.pm)) +
  sum(round(freight.melb$Demand.op))  # 177470


# total number of trips, rounded, based on total demand for day
sum(round(total.freight$Demand.total))  # 269,626

# total number of trips, rounded, baded on total demand for day, within Melbourne
sum(round(freight.melb$Demand.total))  #238,610


# 4 Verify that meshblocks in addresses match the meshblock collection ----
# -----------------------------------------------------------------------------#
meshblocks <- st_read("../data/processed/mb.sqlite")

addresses <- st_read("../data/vic_address.sqlite")  # 2932530 addresses

address.check <- st_intersection(addresses, meshblocks)

exception.check <- address.check %>%
  filter(mesh_block != mb_code)  # 471459 non-matching meshblocks

# exception.check written to a temporary file and checked in QGIS, overlaying
# on various meshblock files.  The addresses with non-matching meshblocks
# are using the 2011 meshblock codes

# Conclusion: don't use the mesh_block field in 'addresses', as it is 2011
# meshblock coders.  Instead, intersect with 2016 meshblocks to find the code.


# 5 Check how many zones lack addresses, and whether they have traffic ----
# -----------------------------------------------------------------------------#
# note that at the time of running this, the 'address' file is the one that
# covers a rectangular area which is slightly larger (to east and south) than the
# greater melbourne region, but slightly smaller than the buffered greater
# melbourne region; so it omits some meshblocks for that reaason.

# read in addresses, and limit to freight categories
addresses <- st_read("../data/vic_address.sqlite") %>%
  # intersect with 2016 meshblocks (don't use the 'mesh_block' field in the
  # vic_address.sqlite file: they are 2011 meshblock codes)
  st_intersection(., meshblocks) %>%
  dplyr::select(-c(mesh_block))

freight.addresses <- addresses %>%
  filter(category %in% c("Commercial", "Industrial", "Primary Production"))

# read in zones, assuming we're using the 'old' zone file
study.area <- st_read("../data/processed/region_buffer.sqlite")

zones <- read_zipped_GIS(zipfile = "../data/original/DTP freight/_Spatial.zip",
                         file = "/VITM2974_Metro_Reference_v3_AGD66_AMG55.shp") %>%
  st_transform(st_crs(study.area))

# find the zone numbers that are inside and outside the study area (buffered region)
zones.melb <- st_filter(st_centroid(zones), study.area, .predicate = st_intersects) %>%
  pull(TZN)

zones.other <- zones$TZN[!zones$TZN %in% zones.melb]

# read in freight, and sum for total day
freight.AM <- read.csv("../data/original/DTP freight/Y2018/HeavyVehicle_AM_Demand_Y2018_RC23v1_05.csv.gz")
freight.IP <- read.csv("../data/original/DTP freight/Y2018/HeavyVehicle_IP_Demand_Y2018_RC23v1_05.csv.gz")
freight.PM <- read.csv("../data/original/DTP freight/Y2018/HeavyVehicle_PM_Demand_Y2018_RC23v1_05.csv.gz")
freight.OP <- read.csv("../data/original/DTP freight/Y2018/HeavyVehicle_OP_Demand_Y2018_RC23v1_05.csv.gz")

total.freight <- freight.AM %>%
  rename(Demand.am = Demand) %>%
  full_join(freight.IP %>%rename(Demand.ip = Demand), by = c("Origin", "Destination"),) %>%
  full_join(freight.PM %>%rename(Demand.pm = Demand), by = c("Origin", "Destination")) %>%
  full_join(freight.OP %>%rename(Demand.op = Demand), by = c()) %>%
  mutate(Demand.total = Demand.am + Demand.ip + Demand.pm + Demand.op)


# intersect zones with freight addresses
zone.addresses <- st_intersection(freight.addresses, zones)

# zones with addresses
zones.with.addresses <- zone.addresses$TZN %>% unique() %>% sort() # 1678
# zones without addresses (all 2974, even the 15 missing from the zones file)
all.zones <- 1:2974
zones.without.addresses <- all.zones[!(all.zones %in% zones.with.addresses)]


# freight where zones lack addresses
freight.no.address <- total.freight %>%
  filter(Origin %in% zones.without.addresses | 
           Destination %in% zones.without.addresses) # 6028992 out of 8844676

freight.no.address.melb <- freight.no.address %>%
  filter(Origin %in% zones.melb & Destination %in% zones.melb) # 3517792

# total freight demand
total.freight.demand <- sum(total.freight$Demand.total)  # 362712

# total freight demand melb
total.freight.demand.melb <- 
  sum(total.freight %>%
        filter(Origin %in% zones.melb & Destination %in% zones.melb) %>%
        pull(Demand.total)) # 315475

# total freight demand without addresses
total.freight.demand.no.address <- 
  sum(freight.no.address$Demand.total) # 94649

# total freight demand without addresses melb
total.freight.demand.no.address.melb <- 
  sum(freight.no.address.melb$Demand.total) # 51091

# Conclusions: 
# •	Out of the total freight demand of 362,712, there are 94,649 trips which 
#   lack an address fort at least one end. 
# •	Out of total freight demand between Melbourne zones of 315,475, there are 
#   51,091 trips which lack an address for at least one end.
# •	Note these calculations are made by just summing the total demand, fractions 
#   and all, so it will be a bit different when fractions are rounded.

