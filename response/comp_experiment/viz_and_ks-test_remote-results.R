# Purpose: Visualize results from supercomputing and perform a ks-test (Kolmogorov-Smirnov Test) to test the null hypothesis that 
#   two datasets were drawn from the same continuous distribution. Compare kNN-simulated
#   data with observed data for each water quality variable. 
# Author: William Raseman

# clear environment
rm(list = ls())

# create visualize statistics function
visualize_statistics <-
  function(nsims,
           innov = TRUE,
           standardize = TRUE, 
           data.type = "sw") {
    # inputs
    #   nsims: number of simulations (integer)
    #   innov: random innovations on or off (TRUE/FALSE)
    #   standardize: standardize data on or off (TRUE/FALSE)
    #   data.type: source water ("sw") [currently the only option]
    
    # load packages
    library(tidyverse)  # modern R packages: ggplot2, dplyr, readr, etc.
    library(forecast)   # time series analysis and visualization
    library(gridExtra)  # arrange multiple ggplot2 plots on a single plot
    library(zoo)        # rolling mean calculations
    
    # load user-defined libraries and functions
    source("./lib/time-series-sim_lib.R")  # time series simulation library
    
    # directories
    fig.dir <- "./response/comp_experiment/figures/"
    simdata.dir <- "./response/comp_experiment/remote/data/"
    
    fig.resolution = 300  # 300 dpi required for color art
    
    # read in time series data
    ## source water quality
    path <- "./data/source-water/02_create_ts/sw_ts.rds"
    ts.data <- readr::read_rds(path)
    
    ## simulated data
    var.names <- colnames(ts.data)
    
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
    
    n.sims <-
      dim(sim.list[[1]])[1] / length(ts.data[, 1])  # get number of simulations from multivariate data
    
    # Visualize boxplots of observed and simulated data
    
    file.boxplt <-
      str_c(fig.dir, "obs-sim_boxplt_", model.type, ".tiff", sep = "")  # filename for boxplts
    
    p.list <- vector(mode = "list", length = nvars)
    
    for (i in 1:nvars) {
      p.list[[i]] <- viz_obs_sim(ts.data[, i], sim.list[[i]]) +
        ylab(var.full.names[i]) +
        xlab("Month") +
        theme(legend.position = "none")
    }
    
    ## save figure
    tiff(
      filename = file.boxplt,
      # tiff(filename = "./figures/figure-2.tiff",
      height = 12,
      width = 17,
      units = 'cm',
      compression = "lzw",
      res = fig.resolution
    )
    grid.arrange(grobs = p.list)
    dev.off()
    
    # Visualize mean, standard deviation, minimum, and maximum of Total Organic Carbon
    
    file.tocstats <-
      str_c(fig.dir, "toc-stats_", model.type, ".tiff", sep = "")  # filename for boxplts
    
    tiff(
      filename = file.tocstats,
      # tiff(filename = "./figures/figure-3.tiff",
      height = 12,
      width = 17,
      units = 'cm',
      compression = "lzw",
      res = fig.resolution
    )
    
    viz_ts_sample_stats(ts.data[, "toc"], sim.list[[4]], title = var.full.names[4])
    dev.off()
    
    # Visualize pairwise correlation between all variables
    
    file.paircorr <-
      str_c(fig.dir, "pairwise-corr_", model.type, ".tiff", sep = "")
    
    tiff(
      filename = file.paircorr,
      height = 12,
      width = 17,
      units = 'cm',
      compression = "lzw",
      res = fig.resolution
    )
    viz_pair_corr(y = ts.data,
                  data = sim.list,
                  data.type = data.type)
    dev.off()
    
    # Visualize lag-1 correlation for each variable
    
    file.lag1corr <-
      str_c(fig.dir, "lag1-corr_", model.type, ".tiff", sep = "")
    
    tiff(
      filename = file.lag1corr,
      # filename = "./figures/figure-5.tiff",
      height = 12,
      width = 17,
      units = 'cm',
      compression = "lzw",
      res = fig.resolution
    )
    viz_ts_lag1(
      y = ts.data,
      data = sim.list,
      data.type = data.type,
      var.names = var.full.names
    )
    dev.off()
    
    # Plot maximum running annual average
    
    ## for source water data, plot TOC
    ### get just toc data and make sure data is in proper order
    toc.df <- arrange(sim.list[[4]], sim, year, month) %>%
      transform(
        value = as.numeric(value),
        month = as.integer(month),
        year = as.integer(year),
        sim = as.integer(sim)
      )
    
    ### determine maximum running annual average for each simulation
    toc.df2 <- toc.df %>%
      group_by(sim) %>%
      mutate(run_avg = rollmean(
        x = value,
        k = 12,
        align = "right",
        fill = NA
      )) %>%
      summarize(max_run_avg = max(run_avg, na.rm = TRUE))
    
    mean.max_run_avg <-
      summarize(toc.df2, mean_max_run_avg = mean(max_run_avg, na.rm = TRUE))
    
    ggplot(data = toc.df2) +
      geom_histogram(aes(max_run_avg))
    
    ### determine maximum running annual average for observed data
    toc.df3 <- ts.data[, "toc"] %>%
      as.data.frame %>%
      mutate(run_avg = rollmean(
        x = x,
        k = 12,
        align = "right",
        fill = NA
      )) %>%
      summarize(max_run_avg = max(run_avg, na.rm = TRUE))
    
    # set x axis limits
    if (min(toc.df2$max_run_avg) < toc.df3$max_run_avg) {
      x.min <- min(toc.df2$max_run_avg) %>% floor
    } else {
      x.min <- toc.df3$max_run_avg %>% floor
    }
    
    if (max(toc.df2$max_run_avg) > toc.df3$max_run_avg) {
      x.max <- max(toc.df2$max_run_avg) %>% ceiling
    } else {
      x.max <- toc.df3$max_run_avg %>% ceiling
    }
    
    p.raa <- ggplot() +
      geom_density(data = toc.df2, aes(max_run_avg)) +
      geom_vline(
        data = mean.max_run_avg,
        aes(xintercept = mean_max_run_avg),
        size = 1,
        color = "black"
      ) + # mean of simulated data
      geom_vline(
        data = toc.df3,
        aes(xintercept = max_run_avg),
        size = 1,
        color = "#FF9999"
      ) +  # observed value
      xlim(x.min, x.max) +
      ylab("Density") +
      xlab("Maximum Running Annual Average TOC (mg/L)")
    
    file.tocraa <- str_c(fig.dir, "toc-raa_", model.type, ".tiff", sep = "")
    
    tiff(
      filename = file.tocraa,
      # filename = "./figures/figure-6.tiff",
      height = 12,
      width = 17,
      units = 'cm',
      compression = "lzw",
      res = fig.resolution
    )
    print(p.raa)
    dev.off()
    
    # calculate percentile of observed value compared to simulated
    ecdf(toc.df2$max_run_avg)(toc.df3$max_run_avg)
  }

knn_ks_test <-   function(nsims,
                          innov = TRUE,
                          standardize = TRUE, 
                          data.type = "sw") {
  
  # inputs
  #   nsims: number of simulations (integer)
  #   innov: random innovations on or off (TRUE/FALSE)
  #   standardize: standardize data on or off (TRUE/FALSE)
  
  # load libraries
  library(tidyverse)
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
  
  return(p.array)
}

# plot histograms of observed and simulated data for a given simulation
hist_obs_sim <-   function(nsims,
                           innov = TRUE,
                           standardize = TRUE,
                           data.type = "sw") {
  
  
  # inputs
  #   nsims: number of simulations (integer)
  #   innov: random innovations on or off (TRUE/FALSE)
  #   standardize: standardize data on or off (TRUE/FALSE)
  
  # load libraries
  library(tidyverse)
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
  
  for (i in 1:ncol(obs.data)) {
    ## get all the data for observed and simulated for each water quality variable
    obs.all <- obs.data[,i] %>% as.numeric %>%
      as_tibble()
    colnames(obs.all) <- "obs"
    sim.all <- sim.list[[i]]$value %>% as.numeric %>%
      as_tibble()
    colnames(sim.all) <- "sim"
    
    ## plot histogram of observed and simulated for each water quality variable
    p <- ggplot() + 
      geom_density(data = obs.all, aes(x=obs), color='red') +
      geom_density(data = sim.all, aes(x=sim), color='black') +
      ggtitle(str_c(var.full.names[i], ", nsims-", nsims, ", innov-", innov, ", stand-", stand))
    
    print(p)
  }
}



# simulation parameters
nsims.array <-
  c(100, 1000)
  # c(50, 100, 300, 1000)  # number of simulations array
  # c(50, 100, 300, 1000, 3000)  # number of simulations array
innov.array <- c(FALSE, TRUE)
stand.array <- c(FALSE, TRUE)

# initialize matrix for p-values
col.names <- c("nsims", "innov", "stand", "alk", "pH", "temp", "toc")
p.matrix <- matrix(data = NA, 
                   nrow = length(nsims.array)*length(innov.array)*length(stand.array), 
                   ncol = length(col.names))
colnames(p.matrix) <- col.names

# visualize all combinations of simulations
count <- 0
for (nsims in nsims.array) {
  for (innov in innov.array) {
    for (stand in stand.array) {
      count <- count + 1
      
      str_c(
        "number of simulations: ",
        nsims,
        ", ",
        "random innovations: ",
        innov,
        ", ",
        "standardized: ",
        stand,
        sep = ""
      ) %>%
        print
      
      # visualize statistics
      visualize_statistics(nsims, innov = innov, standardize = stand)
      
      # ks-test for each water quality variable 
      p.array <- knn_ks_test(nsims, innov = innov, standardize = stand)
      
      p.matrix[count,] <- c(nsims, innov, stand, p.array)
      
    }
  }
}

# visualize p-values 
p.df <- as_tibble(p.matrix) %>%
  mutate(innov = as.logical(innov), 
         stand = as.logical(stand)) 

p.tidy <- gather(p.df, key = "wq_variable", value = "p_value", 
                 alk, pH, temp, toc) %>%
  filter(!(innov == FALSE & stand == TRUE))  # innov=FALSE and stand=TRUE give same result as innov=FALSE and stand=FALSE
p.tidy$innov_stand <- paste("innov-",p.tidy$innov, "_", "stand-", p.tidy$stand, sep="")

ggplot(data = p.tidy) +
  geom_point(aes(x = log(nsims), y = p_value,
                 color = factor(wq_variable),
                 shape = factor(innov_stand)))

# look at histograms for each simulation
hist_obs_sim(nsims = nsims, 
             innov = innov, 
             standardize = stand)


# # look at no random innovations
# ggplot(data = filter(p.tidy, innov == FALSE)) + 
#   geom_point(aes(x = log(nsims), y = p_value, 
#                  color = factor(wq_variable),
#                  shape = factor(innov_stand))) +
#   ggtitle("No random innovations")
# 
# # random innovations and no standardization
# ggplot(data = filter(p.tidy, innov == TRUE, stand == FALSE)) + 
#   geom_point(aes(x = log(nsims), y = p_value, color = factor(wq_variable))) +
#   ggtitle("Random innovations and no standardization")
# 
# # random innovations and no standardization
# ggplot(data = filter(p.tidy, innov == TRUE, stand == TRUE)) + 
#   geom_point(aes(x = log(nsims), y = p_value, color = factor(wq_variable))) +
#   ggtitle("Random innovations and standardization")

# do a ks-test for each month 




