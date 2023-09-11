# function to round demand figures in freight file, and to adjust rounding where
# rounded period demand totals don't match the rounded total demand, by adjusting
# the period demand figure(s) that were rounded down the most (if increase is
# needed to match total) or up the most (if decrease is needed to match total)

adjustedRoundedFreight <- function(freight.combined) {
  
  # add id, so original and rounded freight can be matched
  freight <- freight.combined %>%
    mutate(id = row_number())
  
  # round all demand figures, and filter to non-zero totals
  r.freight <- round(freight) %>%
    filter(Demand.total > 0)
  
  # keep only the rows from 'freight' that correspond to the non-zero r.freight rows
  freight <- freight %>%
    filter(id %in% r.freight$id)
  
  # Setup for parallel processing (using doSNOW instead of doParallel, because
  # allows progress reporting)
  # Detect the number of available cores and create cluster
  cores <- detectCores()
  cluster <- parallel::makeCluster(cores)
  # Activate cluster for foreach library
  doSNOW::registerDoSNOW(cluster)
  
  print(paste(Sys.time(), "| Using parallel processing with", cores, 
              "cores to check whether rounding adjustments are required for", 
              nrow(freight), "rows of freight data"))

  # set up progress reporting
  # https://stackoverflow.com/questions/5423760/how-do-you-create-a-progress-bar-when-using-the-foreach-function-in-r
  pb <- txtProgressBar(max = nrow(freight), style = 3)
  progress <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress = progress)
  
  adjusted.r.freight <- foreach(i = 1:nrow(r.freight),
                                .packages = c("dplyr"),
                                .combine = rbind,
                                # .verbose = TRUE,
                                .options.snow = opts) %dopar% {
                                  
                                  input.row <- r.freight[i, ]
                                  
                                  period.demand.sum <- input.row$Demand.am +
                                    input.row$Demand.ip + 
                                    input.row$Demand.pm +
                                    input.row$Demand.op
                                  
                                  total.demand <- input.row$Demand.total
                                  
                                  if (period.demand.sum != total.demand) {
                                    
                                    # find the original and rounded values for the row
                                    orig.values <- c(freight[i, "Demand.am"], 
                                                     freight[i, "Demand.ip"], 
                                                     freight[i, "Demand.pm"], 
                                                     freight[i, "Demand.op"])
                                    # orig.total <- freight[i, "Demand.total"]
                                    r.values <- c(input.row$Demand.am,
                                                    input.row$Demand.ip, 
                                                    input.row$Demand.pm,
                                                    input.row$Demand.op)
                                    # r.total <- total.demand
                                    
                                    # find the residuals (amounts by which the values were rounded)
                                    # residuals are negative values if rounded down, positive if up 
                                    residuals <- r.values - orig.values
                                    
                                    # find the indices of the values, in order from smallest to largest
                                    ranked <- order(residuals)
                                    
                                    # number of r.values to adjust
                                    diff = abs(total.demand - period.demand.sum)
                                    
                                    # increase or decrease the values corresponding to the indices as required
                                    if (period.demand.sum < total.demand) {
                                      # need to increase, so select indices for 
                                      # values that were rounded down the most
                                      indices <- ranked[1:diff]
                                      for (idx in indices) {
                                        r.values[idx] <- r.values[idx] + 1
                                      }
                                    } else if (period.demand.sum > total.demand) {
                                      # need to decrease, so select indices for 
                                      # values that were rounded up the most
                                      start <- length(ranked) - diff + 1
                                      end <- length(ranked)
                                      indices <- ranked[start:end]
                                      for (idx in indices) {
                                        r.values[idx] <- r.values[idx] - 1
                                      }
                                    }
                                    
                                    output.row <- input.row %>%
                                      mutate(Demand.am = r.values[1],
                                             Demand.ip = r.values[2],
                                             Demand.pm = r.values[3],
                                             Demand.op = r.values[4])
                                  } else {
                                    output.row <- input.row
                                  }
                                  return(output.row)
                                }
  close(pb)
  stopCluster(cluster)
  
  return(adjusted.r.freight %>%
           # remove id column
           dplyr::select(-id))
}
