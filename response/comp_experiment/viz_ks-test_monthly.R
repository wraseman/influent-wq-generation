# Purpose: perform ks-test for a given water quality parameter comparing the monthly 
#   distribution of observed and simulated data. Plot the results of ks-test for each month. 
# Author: William Raseman 

# clear environment
rm(list = ls())

viz_pval_kstest_mon_boxplts <- function(nsims, innov=TRUE, stand=TRUE,
                                          wq.var, obs.path, sim.path)
{
  # Inputs: 
  #   nsims - number of simulations
  #   innov - random innovations? (TRUE/FALSE)
  #   stand - monthly standardized data? (TRUE/FALSE)
  #   wq.var - water quality variable: "alk" for alkalinity, "pH" for pH,
  #     "temp" for temperature (deg C), and "toc" for total organic carbon
  #   obs.path: file path to observed multivariate time series data
  #   sim.path: file path to simulated data (data frame with columns ???)
  
  # load libraries
  library(tidyverse)
  
  # load user-defined libraries and functions
  source("./lib/time-series-sim_lib.R")  # time series simulation library
  
  # read in observed data (format: multivariate time series)
  obs.ts <- readr::read_rds(obs.path)
  obs.ts <- obs.ts[,wq.var]
  
  # read in simulated data (format: dataframe with the following columns - value | month | year | sim )
  sim.df <- read_rds(sim.path) 
  
  # visualize data and save plot
  boxplt <- viz_obs_sim(obs.ts, sim.df, outlier.alpha = 0.1)
  
  # read in combined dataframe
  comb.df <- read_obs_sim_df(obs.path = obs.path, sim.path = sim.path)
  
  # analyze each month with ks-test
  n.months <- 12
  kstest.matrix <- matrix(data = NA, nrow = n.months, ncol = 2)
  colnames(kstest.matrix) <- c("p_value", "month")
  kstest.df <- as_tibble(kstest.matrix)
  
  for (i in 1:n.months) {
    obs.month <- filter(comb.df, obs_sim=="obs", month==i) %>%
      select(value) %>%
      unlist
    sim.month <- filter(comb.df, obs_sim=="sim", month==i)  %>%
      select(value) %>%
      unlist
    kstest.df$p_value[i] <- ks.test(obs.month, sim.month)$p.value
    kstest.df$month[i] <- i
  }
  
  # create boxplot with p-values for labels
  p.title <- str_c("Innov: ", innov, ", ", 
                   "Stand: ", stand, ", ",
                   "Nsims: ", nsims, ", ", 
                   "Var: ", wq.var, "\n", 
    "Monthly p-values for ks-tests between obs and sim\n")
  for (i in 1:n.months) {
    p.title <- str_c(p.title, i, ":", round(kstest.df$p_value[i], digits=2))
    if (i != 12) p.title <- str_c(p.title, ", ")
    if (i == 6) p.title <- str_c(p.title, "\n")
  }
  
  # monthly boxplots with p-values in the title
  ks_boxplt <- boxplt + 
    ggtitle(p.title)
  
  return(ks_boxplt)
}
  
# USER INPUTS
# ## number of simulations
# nsims <- 300
# ## random innovations
# innov <- TRUE
# ## standardization
# stand <- TRUE
## water quality variables
# wq.var <- "temp"
nsims <- 300
innov.array <- c(TRUE, TRUE, FALSE)
stand.array <- c(TRUE, FALSE, FALSE)
wqvar.array <- c("alk", "pH", "temp", "toc")

## observed data file path
obs.dir <- "./data/source-water/02_create_ts/"  # observed data directory
obs.path <- str_c(obs.dir, "sw_ts.rds")  # observed data file path

# test all combinations
for (i in 1:length(innov.array)) {
  for (wq.var in wqvar.array) {
    innov <- innov.array[i]
    stand <- stand.array[i]
    
    ## simulated data file path
    model.type <- str_c("kNN", str_c("nsims", "-", nsims),
                        str_c("innov", "-", innov),
                        str_c("stand", "-", stand),
                        sep = "_")
    sim.dir <- "./response/comp_experiment/remote/data/source-water/04_simulate_kNN/"
    sim.path <- str_c(sim.dir,
                      model.type, "_sw_",
                      wq.var, ".rds")  # name simulation based on model, data type, and variable

    # visualize results and save
    fig.resolution <- 300
    fig.dir <- "./response/comp_experiment/figures/ks_test/"
    plt.name <- str_c("kstest-mon_", model.type, "_", wq.var, ".tiff")
    
    tiff(filename = str_c(fig.dir, plt.name),
         height = 12, width = 17, units = 'cm',
         compression = "lzw", res = fig.resolution)
    
    viz_pval_kstest_mon_boxplts(nsims = nsims, innov = innov, stand = stand, 
                                     wq.var = wq.var, obs.path = obs.path, 
                                     sim.path = sim.path) %>%
      print

    dev.off()
    

  }
}

    # viz_pval_kstest_mon_boxplts(nsims = nsims, innov = innov, stand = stand, 
    #                             wq.var = var, obs.path = obs.path, 
    #                             sim.path = sim.path)

