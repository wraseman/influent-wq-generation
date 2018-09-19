# Reproducible workflow to generate results and figures related to
#   "Nearest neighbor bootstrap for generating influent time series for water treatment"
# author: William Raseman

# clear environment
rm(list=ls())

# load libraries
# library(beepr)  # to notify when simulation is done

# load user-defined functions for each step of the workflow
load("./lib/import_clean.Rdata")
load("./lib/create_ts.Rdata")
load("./lib/visualize_ts.Rdata")
load("./lib/simulate_kNN.Rdata")
load("./lib/visualize_statistics.Rdata")

# simulation parameters
nsims <- 2500
## note: to check results, try using 50 simulations for reduced computational time
innov <- TRUE
data.type <- "sw"

# Generate Manuscript Figures

## step 1 - import and clean influent water quality data
import_clean()

## step 2 - create complete, monthly time series
create_ts()

## step 3 - visualize time series dataset
### note: generates figure 1
visualize_ts()

## step 4 - generate ensembles of synethetic water quality time series using k-NN algorithm
set.seed(101) # set pseudorandom seed for reproducibile simulations
simulate_kNN(nsims=nsims, innov=innov, threshold=c(TRUE, FALSE, TRUE, TRUE), data.type=data.type)
### note: temperature, total organic carbon, and alkalinity constrained to
###       be non-negative (variables 1, 3, and 4) via 'threshold' input

## step 5 - visualize statistics of observed and simulated data
### note: generates figures 2 - 6
visualize_statistics(innov=innov, data.type=data.type)

# Generate Supporting Information Figures

## simulate water quality with random innovations disabled
set.seed(101) # set pseudorandom seed for reproducibile simulations
simulate_kNN(nsims=nsims, innov=FALSE, data.type=data.type)
### note: generates figure S1
visualize_statistics(innov=FALSE, data.type=data.type)

## simulate temperature and precipitation with random innovations enabled
set.seed(101) # set pseudorandom seed for reproducibile simulations
simulate_kNN(nsims=nsims, innov=TRUE, threshold=c(TRUE, TRUE), data.type="mine")
### note: generates figures S2, S4 and S5
### note: temperature and precipitation constrained to
###       be non-negative (variables 1 and 2) via 'threshold' input
visualize_statistics(innov=TRUE, data.type="mine")

## simulate temperature and precipitation with random innovations enabled
set.seed(101) # set pseudorandom seed for reproducibile simulations
simulate_kNN(nsims=nsims, innov=FALSE, threshold=c(TRUE, TRUE), data.type="mine")
### note: generates figures S3
visualize_statistics(innov=FALSE, data.type="mine")

# notify when script is done
# beep(sound = "mario")

