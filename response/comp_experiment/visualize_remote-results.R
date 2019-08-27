# Visualize results from supercomputing
# author: William Raseman

# clear environment
rm(list=ls())

# load libraries
# library(beepr)  # to notify when simulation is done

# load user-defined functions for each step of the workflow
load("./lib/import_clean.RData")
load("./lib/create_ts.RData")
load("./lib/visualize_ts.RData")
load("./lib/simulate_kNN.RData")
load("./lib/visualize_statistics.RData")

# simulation parameters
nsims <- 10
innov <- FALSE
standardize <- FALSE

visualize_statistics(nsims=10, innov=FALSE, standardize=TRUE)