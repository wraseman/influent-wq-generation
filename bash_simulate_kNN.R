# Purpose: run k-NN resampling simulation from a bash script
# Author: William Raseman

main() <- function() {

  # clear environment
  rm(list=ls())
  
  # command line arguments from bash script
  args <- commandArgs(trailingOnly = TRUE)
  
  if(lenth(args) != 3) { stop("Exactly three arguments are needed: number of simulations, 
                              random innovations (TRUE/FALSE), and monthly standardization (TRUE/FALSE)")}
  
  # load user-defined functions for each step of the workflow
  load("./lib/import_clean.Rdata")
  load("./lib/create_ts.Rdata")
  load("./lib/visualize_ts.Rdata")
  load("./lib/simulate_kNN.Rdata")
  load("./lib/visualize_statistics.Rdata")
  
  # simulation parameters
  nsims <- as.integer(args[1])
  innov <- as.logical(args[2])
  data.type <- "sw"
  standardize <- as.logical(args[3])
  
  # generate ensembles of synethetic water quality time series using k-NN algorithm
  set.seed(101) # set pseudorandom seed for reproducibile simulations
  simulate_kNN(nsims=nsims, innov=innov, threshold=c(TRUE, FALSE, TRUE, TRUE), 
               data.type=data.type, standardize=standardize)
  ### note: temperature, total organic carbon, and alkalinity constrained to
  ###       be non-negative (variables 1, 3, and 4) via 'threshold' input
  
  # see ~\data\source-water\04_simulate_kNN\ for the results
}

main()