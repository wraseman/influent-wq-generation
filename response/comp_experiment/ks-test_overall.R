# Purpose: perform a ks-test (Kolmogorov-Smirnov Test) to test the null hypothesis that 
#   two datasets were drawn from the same continuous distribution. Compare kNN-simulated
#   data with observed data for each water quality variable. 
# Author: William Raseman

# user inputs
nsims <- 50 
innov <- TRUE
standardize <- TRUE
data.type <- "sw"

# load packages
library(tidyverse)  # modern R packages: ggplot2, dplyr, readr, etc.
library(forecast)   # time series analysis and visualization

# load user-defined libraries and functions
source("./lib/time-series-sim_lib.R")  # time series simulation library

# directories
# fig.dir <- "./response/comp_experiment/figures/"
simdata.dir <- "./response/comp_experiment/remote/data/"

# read in time series data
## source water quality
path <- "./data/source-water/02_create_ts/sw_ts.rds"
obs.data <- readr::read_rds(path)

## simulated data
var.names <- colnames(obs.data)

var.full.names <-
  c("Alkalinity (mg/L)", "pH", "Temperature (°C)", "TOC (mg/L)")

nvars <- length(var.names)
model.type <- read.path <- c()
sim.list <- vector(mode = "list", length = nvars)

## read in data for each variable
for (i in 1:nvars) {
  model.type <- str_c("kNN",
                      str_c("nsims", "-", nsims),
                      str_c("innov", "-", innov),
                      str_c("stand", "-", standardize),
                      sep = "_"
  )
  read.path <-
    str_c(
      simdata.dir,
      # "./data/source-water/04_simulate_kNN/",
      model.type,
      "_",
      data.type,
      "_",
      var.names[i],
      ".rds"
    )  # name simulation based on model, data type, and variable
  sim.list[[i]] <- read_rds(read.path)
}

# ks-test for each water quality variable
p.array <- vector(mode = "numeric", length = ncol(obs.data))

for (i in 1:length(p.array)) {
  x1 <- obs.data[,i] %>% as.numeric
  x2 <- sim.list[[i]][,1]
  ks_test <- ks.test(x1, x2)
  p.array[i] <- ks_test$p.value
}
