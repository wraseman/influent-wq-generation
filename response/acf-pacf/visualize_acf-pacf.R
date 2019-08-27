# Plot autocorrelation and partial autocorrelation functions of the time series data
# author: William Raseman

# clear environment
rm(list=ls()) 

# load packages
library(tidyverse)  # modern R packages: ggplot2, dplyr, readr, etc.
library(forecast)   # time series analysis and visualization  
library(gridExtra)  # arrange multiple ggplot2 plots on a single plot

# load user-defined libraries
source("./lib/time-series-sim_lib.R")  # time series simulation library

# read in source water time series data
path <- "./data/source-water/02_create_ts/sw_ts.rds"
wq.ts <- wq.fullnames.ts <- readr::read_rds(path)

colnames(wq.fullnames.ts) <- c("Alkalinity (mg/L)", 
                               "pH", "Temperature (°C)", "TOC (mg/L)")  
## note: not sure if degree symbol will work correctly with all operating systems. 

# plot autocorrelation function (ACF) and partial autocorrelation function(PACF) to determine appropriate lag
for (i in 1:ncol(wq.ts)) {
  p.acf <- ggAcf(wq.ts[,i]) +
    ggtitle(colnames(wq.fullnames.ts)[i])
  
  p.pacf <- ggPacf(wq.ts[,i]) +
    ggtitle(colnames(wq.fullnames.ts)[i]) 
  
  tiff(filename = str_c("./response/acf-pacf/figures/figure-acf-pacf_", i, ".tiff"), 
       height = 12, width = 17, units = 'cm', 
       compression = "lzw", res = 600)

  grid.arrange(p.acf, p.pacf, nrow = 2)

  dev.off()
}

