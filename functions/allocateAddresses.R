# function to allocate random origin and destination addresses to freight trips, 
# based on freight addresses in the zone

allocateAddresses <- function(freight.trips, freight.meshblock.addresses, centroid.addresses,
                              freight.meshblock.address.TZNs) {
  
  # Setup for parallel processing (using doSNOW instead of doParallel, because
  # allows progress reporting)
  # Detect the number of available cores and create cluster
  cores <- detectCores()
  cluster <- parallel::makeCluster(cores)
  # Activate cluster for foreach library
  doSNOW::registerDoSNOW(cluster)
  
  print(paste(Sys.time(), "| Using parallel processing with", cores, 
              "cores to allocate origin and destination nodes for", 
              nrow(freight.trips), "freight trips"))
  
  # set up progress reporting
  # https://stackoverflow.com/questions/5423760/how-do-you-create-a-progress-bar-when-using-the-foreach-function-in-r
  pb <- txtProgressBar(max = nrow(freight.trips), style = 3)
  progress <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress = progress)
  
  allocatedAddresses <- foreach(i = 1:nrow(freight.trips),
                                # allocatedAddresses <- foreach(i = 1:1000,
                                .packages = c("dplyr", "sf"),
                                .combine = rbind,
                                # .verbose = TRUE,
                                .options.snow = opts) %dopar% {
                                  
                                  # address for origin zone
                                  if (freight.trips$Origin[i] %in% freight.meshblock.address.TZNs) {
                                    orig.addresses <- freight.meshblock.addresses %>%
                                      filter(TZN == freight.trips$Origin[i])
                                    orig <- orig.addresses[sample(nrow(orig.addresses), size = 1), ]
                                  } else {
                                    orig <- centroid.addresses %>%
                                      filter(TZN == freight.trips$Origin[i])
                                  }
                                  orig.x <- st_coordinates(orig)[[1]]
                                  orig.y <- st_coordinates(orig)[[2]]
                                  
                                  
                                  # address for destination zone
                                  if (freight.trips$Destination[i] %in% freight.meshblock.address.TZNs) {
                                    dest.addresses <- freight.meshblock.addresses %>%
                                      filter(TZN == freight.trips$Destination[i])
                                    dest <- dest.addresses[sample(nrow(dest.addresses), size = 1), ]
                                  } else {
                                    dest <- centroid.addresses %>%
                                      filter(TZN == freight.trips$Destination[i])
                                  }
                                  dest.x <- st_coordinates(dest)[[1]]
                                  dest.y <- st_coordinates(dest)[[2]]
                                  
                                  # check for same orig and dest, and if so then find another if possible
                                  if (orig.x == dest.x & orig.y == dest.y) {
                                    
                                    # check whether origin or destination has meshblock addresses
                                    # (so loop doesn't run if they're the same because using centroids)
                                    if (freight.trips$Origin[i] %in% freight.meshblock.address.TZNs | 
                                        freight.trips$Destination[i] %in% freight.meshblock.address.TZNs) {
                                      
                                      # where there are meshblock addresses, find how many distinct
                                      if (freight.trips$Origin[i] %in% freight.meshblock.address.TZNs) {
                                        orig.coords <- st_coordinates(orig.addresses) %>% as.data.frame()
                                        unique.orig.coords <- nrow(distinct(orig.coords, X, Y))
                                      } else {
                                        unique.orig.coords <- 0
                                      }
                                      if (freight.trips$Destination[i] %in% freight.meshblock.address.TZNs) {
                                        dest.coords <- st_coordinates(dest.addresses) %>% as.data.frame()
                                        unique.dest.coords <- nrow(distinct(dest.coords, X, Y))
                                      } else {
                                        unique.dest.coords <- 0
                                      }
                                      
                                      # if multiple distinct orig addresses, select a new one; 
                                      # otherwise if multiple distinct dest addresses, select a new one
                                      if (unique.orig.coords > 1) {
                                        while (orig.x == dest.x & orig.y == dest.y) {
                                          orig <- orig.addresses[sample(nrow(orig.addresses), size = 1), ]
                                          orig.x <- st_coordinates(orig)[[1]]
                                          orig.y <- st_coordinates(orig)[[2]]
                                        }
                                      } else if (unique.dest.coords > 1) {
                                        while (orig.x == dest.x & orig.y == dest.y) {
                                          dest <- dest.addresses[sample(nrow(dest.addresses), size = 1), ]
                                          dest.x <- st_coordinates(dest)[[1]]
                                          dest.y <- st_coordinates(dest)[[2]]
                                          
                                        }
                                      } 
                                    }
                                  }
                                  
                                  output.row <- freight.trips[i, ] %>%
                                    mutate(orig_x = orig.x, orig_y = orig.y,
                                           dest_x = dest.x, dest_y = dest.y)
                                  
                                  
                                  return(output.row)
                                  
                                }
  close(pb)
  stopCluster(cluster)
  
  return(allocatedAddresses)
}

