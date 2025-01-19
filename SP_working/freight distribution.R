# examining hourly distribution of traffic, to inform decisions about whether
# hourly distribution of freight traffic should be non-uniform

library(dplyr)
library(ggplot2)
options(scipen = 1000)


# 1 Distribution of traffic in 'Typical Traffic Daily Volume Profile' ----
# -----------------------------------------------------------------------------#
TTDVP <- read.csv("../data/original/2018.csv", header = FALSE)

# temp_dir <- tempdir()
# unzip("../data/original/Typical_Daily_Traffic_Volume_Profile_2018.zip", exdir = temp_dir)

# day types
TTDVP$V10 %>% unique()
# [1] "SCHOOL TERM/NORMAL"      "LONG WKND/PUB HOL/OTHER" "SCHOOL HOLIDAY" 

hourly.counts <- TTDVP %>%
  # filter to 'normal'
  filter(V10 == "SCHOOL TERM/NORMAL") %>%
  # hour column
  mutate(hour = as.numeric(substr(V11, 1, 2))) %>%
  # sum total traffic measurement counts for hour
  group_by(hour) %>%
  summarise(traffic = sum(V13)) %>%
  ungroup()


ggplot(hourly.counts, aes(x = hour, y = traffic)) +
  geom_bar(stat = "identity") +
  labs(x = "Hour", y = "Traffic Count") +
  ggtitle("Traffic Count by Hour (Typical Traffic Daily Volume Profile)")


# 2 Distribution of traffic in VISTA ----
# -----------------------------------------------------------------------------#
trips <- read.csv(unzip("../data/original/VISTA_2012-20_LGA_V1.zip", 
                        "VISTA_2012-20_LGA_V1/T_VISTA_1220_LGA_V1.csv"))

car.driver.hourly.counts <- trips %>%
  # filter to vehicle drivers
  filter(linkmode == "Vehicle Driver") %>%
  # tidy hours over 23
  mutate(hour = ifelse(starthour > 23,
                       starthour - 24,
                       starthour)) %>%
  # sum total counts for hour
  group_by(hour) %>%
  summarise(traffic = n()) %>%
  ungroup

ggplot(car.driver.hourly.counts, aes(x = hour, y = traffic)) +
  geom_bar(stat = "identity") +
  labs(x = "Hour", y = "Trip Count") +
  ggtitle("Trip Count by Starting Hour (VISTA 2012-20, Vehicle Driver trips)")



# 2 Distribution of car and truck traffic in QTS ----
# -----------------------------------------------------------------------------#
QTS_location_2017_20 = "../data/original/QTS/pooled_2017-20/"
QTS_location_2019_22 = "../data/original/QTS/pooled_2019-22/"

# read in region, lga and agegroup (same in both pooled files, use 2017-20)
region_QTS <- read.csv(paste0(QTS_location_2017_20, "R_REGION.csv"))

# read in households
# households from each pool
hh_QTS_2017_20 <- 
  read.csv(paste0(QTS_location_2017_20, "1_QTS_HOUSEHOLDS.csv"))  %>%
  dplyr::select(HHID, HHWGT = HHWGT_19, TRAVDATE, TRAVMONTH, TRAVYEAR, stratagroupid = STRATA_LGA)
hh_QTS_2019_22 <- 
  read.csv(paste0(QTS_location_2019_22, "1_QTS_HOUSEHOLDS.csv"))  %>%
  dplyr::select(HHID, HHWGT = HHWGT_21, TRAVDATE, TRAVMONTH, TRAVYEAR, stratagroupid = STRATA_LGA)


# identify 2019-20 households (in both pools; to be omitted from first)
HHID_2019_20 <- hh_QTS_2017_20 %>%
  filter((TRAVYEAR == 2019 & TRAVMONTH %in% c(7, 8, 9, 10, 11, 12)) | TRAVYEAR == 2020) %>%
  .$HHID

# households - begin with 2017-20
hh_QTS <- hh_QTS_2017_20 %>%
  # omit 2019-20, as they are in 2019-22 pool
  filter(!HHID %in% HHID_2019_20) %>%
  # add 2019-22 pool
  rbind(., hh_QTS_2019_22) %>%
  
  # determine weekday/weekend days
  mutate(date = paste0(TRAVYEAR, "-", TRAVMONTH, "-", TRAVDATE),
         day = weekdays(as.Date(date)),
         DayType = as.factor(
           case_when(day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday") ~ "weekday",
                     day %in% c("Saturday", "Sunday") ~ "weekend day"))) %>%
  filter(DayType == "weekday") %>%
  
  # add regions
  left_join(region_QTS, by = "stratagroupid")


# read in trips, beginning with the 2017-20 pool
trips_QTS <- 
  read.csv(paste0(QTS_location_2017_20, "5_QTS_TRIPS.csv")) %>%
  # omit 2019-20, as they are in 2019-22 pool
  filter(!HHID %in% HHID_2019_20) %>%
  # add 2019-22 pool
  rbind(read.csv(paste0(QTS_location_2019_22, "5_QTS_TRIPS.csv"))) %>%
  # join households and filter to Brisbane (excluding Gold Coast & Sunshine Coast)
  left_join(hh_QTS, by = "HHID") %>%
  filter(REGION == "Greater Brisbane") %>%
  # convert start time in mins to hour
  mutate(hour = floor(STARTIME / 60))

# car drivers
car.QTS <- trips_QTS %>%
  filter(MAINMODE == "Car driver") %>%
  # sum total counts for hour
  group_by(hour) %>%
  summarise(traffic = n()) %>%
  ungroup

# truck drivers
truck.QTS <- trips_QTS %>%
  filter(MAINMODE == "Truck driver") %>%
  # sum total counts for hour
  group_by(hour) %>%
  summarise(traffic = n()) %>%
  ungroup

# combined car and truck, with each as percentage of daily totals
car.truck.QTS <- car.QTS %>%
  left_join(truck.QTS, by = "hour", suffix = c("_car", "_truck")) %>%
  mutate(traffic_car_hour_pct = traffic_car / sum(traffic_car) * 100,
         traffic_truck_hour_pct = traffic_truck / sum(traffic_truck) * 100)

# plots for the above
ggplot(car.QTS, aes(x = hour, y = traffic)) +
  geom_bar(stat = "identity") +
  labs(x = "Hour", y = "Trip Count") +
  ggtitle("Trip Count by Starting Hour (QTS 2017-22, Car Driver trips)")

ggplot(truck.QTS, aes(x = hour, y = traffic)) +
  geom_bar(stat = "identity") +
  labs(x = "Hour", y = "Trip Count") +
  ggtitle("Trip Count by Starting Hour (QTS 2017-22, Truck Driver trips)")

ggplot(car.truck.QTS) +
  geom_bar(aes(x = hour, y = traffic_car_hour_pct,
               fill = "r"), stat = "identity", alpha = 0.3) +
  geom_bar(aes(x = hour, y = traffic_truck_hour_pct,
               fill = "b"), stat = "identity", alpha = 0.3) +
  scale_fill_manual(name = "Driver", values = c("r" = "red", "b" = "blue"), 
                    labels=c("b" = "truck", "r" = "car")) +
  labs(x = "Hour", y = "Percentage of daily trips per hour") +
  ggtitle("Trip Count by Starting Hour (QTS 2017-22, Car and Truck Driver trips)")


# vary large car trip numbers before 5am, are we sure?
car.check <- trips_QTS %>%
  filter(MAINMODE == "Car driver")

ggplot(car.check) +
  geom_histogram(aes(x = STARTIME), binwidth = 60)
# looks much the same - with 4am being the highest - but not exactly; I suspect
# 'binwidth' isn't exactly lining up with STARTime of 60 = 1 hr
