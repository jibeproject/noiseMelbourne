# function to allocate random origin and destination nodes to traffic zones, 
# based on freight addresses in the zone

allocateNodes <- function(freight.trips, freight.addresses) {
  
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
  
  allocatedNodes <- foreach(i = 1:nrow(freight.trips),
  # allocatedNodes <- foreach(i = 1:10,
                            .packages = c("dplyr", "sf"),
                            .combine = rbind,
                            # .verbose = TRUE,
                            .options.snow = opts) %dopar% {
                              
                              # addresses for orig and dest zones
                              orig.addresses <- freight.addresses %>%
                                st_drop_geometry() %>%
                                filter(TZN == freight.trips$Origin[i])
                              
                              dest.addresses <- freight.addresses %>%
                                st_drop_geometry() %>%
                                filter(TZN == freight.trips$Destination[i])
                              
                              # pick random orig and dest addresses, find their nodes
                              orig.n <- sample(nrow(orig.addresses), size = 1)
                              orig.address.node <- orig.addresses$n.node[orig.n]
                              
                              dest.n <- sample(nrow(dest.addresses), size = 1)
                              dest.address.node <- dest.addresses$n.node[dest.n]
                              
                              # if orig and dest nodes are the same, find
                              # another pair if possible
                              if (orig.address.node == dest.address.node) {
                                
                                # see whether another choice is possible
                                n.orig.nodes <- length(unique(orig.addresses$n.node))
                                n.dest.nodes <- length(unique(dest.addresses$n.node))
                                if (n.orig.nodes > 1 | n.dest.nodes > 1) {
                                  
                                  # and if it is, try another pick until they are different
                                  while(orig.address.node == dest.address.node) {
                                    
                                    orig.n <- sample(nrow(orig.addresses), size = 1)
                                    orig.address.node <- orig.addresses$n.node[orig.n]
                                    
                                    dest.n <- sample(nrow(dest.addresses), size = 1)
                                    dest.address.node <- dest.addresses$n.node[dest.n]
                                    
                                  }
                                }
                              }
                              
                              output.row <- freight.trips[i, ] %>%
                                mutate(orig.node = orig.address.node,
                                       dest.node = dest.address.node)
                              
                                                            
                              return(output.row)
                              
                            }
  close(pb)
  stopCluster(cluster)
  
  return(allocatedNodes)
}
