# Purpose: create bash scripts to run kNN simulations on remote computer
# Author: William Raseman

########### example output ###########
# #!/bin/bash
# 
# #SBATCH -J <job_name>
# #SBATCH --ntasks 1
# #SBATCH --output <job_name>.out
# #SBATCH --time 00:30:00
# #SBATCH --qos testing
# #SBATCH --mail-user=<email>
# #SBATCH --mail-type=BEGIN
# #SBATCH --mail-type=END
# 
# echo The job has begun
# 
# # load R programming
# module load R
# 
# # run R script
# ## command line arguments:
# ## 1) number of simulations (integer)
# ## 2) random innovations? (TRUE/FALSE)
# ## 3) standardize data? (TRUE/FALSE)
# Rscript <nsims> <innov> <stand>
# 
# wait
# echo The job has finished
########### example output ###########

# clear environment
rm(list=ls())

# load libraries
library(tidyverse)

# simulation parameters
nsim.array <- c(50, 100, 300, 1000, 3000)  # number of simulations array
innov.array <- c(FALSE, TRUE)  
stand.array <- c(FALSE, TRUE)

# count number of simulation combinations
count <- 0
for (nsims in nsim.array) {
  for (innov in innov.array) {
    for (stand in stand.array) {
      count = count + 1
    }
  }
}

# create array of bash scripts
sbatch.array <- vector(mode="character", length=count)
count <- 0  # reset count

# create bash script for all of simulations
for (nsims in nsim.array) {  
  for (innov in innov.array) {
    for (stand in stand.array) {
      
      # define model name
      model_type <- str_c(str_c("nsims", "-", nsims),
                          str_c("innov", "-", innov), 
                          str_c("stand", "-", stand),
                          sep="_")
      
      # unchanging for bash script
      shebang <- "#!/bin/bash"
      sbatch <- "#SBATCH"

      # user input SLURM parameters
      ## determine time and quality of service (qos) based on number of simulations
      ## qos options: https://curc.readthedocs.io/en/latest/running-jobs/job-resources.html
      # if (nsims < 1800) { # equiv. of 30 minutes if 1 simulates takes 3 seconds
      #   # qos <- "testing"  # max wall time is 30 minutes for testing queue
      #   qos <- "normal"
      #   time <- "00:30:00"
      # } else {
      #     qos <- "normal"
      #     time <- "03:00:00"  # this amount of time should work for up to 3600 simulations
      # }
      qos <- "normal"
      time <- "03:00:00"
      ntasks <- 1
      # email <- type email here
      
      # SLURM commands  
      s_job <- str_c(sbatch, " -J ", model_type, "_%j")    #SBATCH -J <model_type>
      s_tasks <- str_c(sbatch, " --ntasks ", ntasks)       #SBATCH --ntasks <ntasks>
      s_output <- str_c(sbatch, " --output ", model_type, "_%j.out")    #SBATCH --output <model_type>.out
      s_time <- str_c(sbatch, " --time ", time)  #SBATCH --time <time>
      s_qos <- str_c(sbatch, " --qos ", qos)     #SBATCH --qos <qos>
      # s_email <- str_c(sbatch, " --mail-user=", email)    #SBATCH --mail-user=<email>
      # s_mail_begin <- "#SBATCH --mail-type=BEGIN"
      # s_mail_end <- "#SBATCH --mail-type=END"

      
      # save bash script in /remote
      ## source: https://stackoverflow.com/questions/2470248/write-lines-of-text-to-a-file-in-r
      bash_dir <- "./response/comp_experiment/remote/bash/"
      bash_filename <- str_c(model_type, ".bash")
      bash_path <- str_c(bash_dir, bash_filename)
      conn_bash <- file(bash_path, "wb")  # "wb" for Unix line endings
      
      # create command to run R script       
      rscript_cmd <- str_c("Rscript ./remote_simulate_kNN.R", 
      nsims, innov, stand, 
      sep = " ")
      
      ## write SLURM and bash commands
      writeLines(c(shebang, 
                   "", # newline
                   s_job,
                   s_tasks, 
                   s_output,
                   s_time,
                   s_qos,
                   # s_email,
                   # s_mail_begin,
                   # s_mail_end,
                   "",
                   "echo The job has begun",
                   "",
                   "# load R programming",
                   "module load R",
                   "",
                   "# run R script",
                   "## command line arguments:",
                   "## 1) number of simulations (integer)",
                   "## 2) random innovations? (TRUE/FALSE)",
                   "## 3) standardize data? (TRUE/FALSE)",
                   rscript_cmd,
                   "",
                   "wait",
                   "echo The job has finished"),
                 con = conn_bash,
                 sep = "\n")
      
      close(conn_bash) # close file connection

      # # print out simulations for debugging
      # str_c("number of simulations: ", nsims, ", ", 
      #       "random innovations: ", innov, ", ", 
      #       "standardized: ", stand,
      #       sep = "") %>%
      #   print
      
      # keep track of bash script file names
      count = count + 1
      sbatch.array[count] <- str_c("sbatch ./bash/", bash_filename)
    }
  }
}

# create bash script that submits all bash scripts as jobs
conn_submit_all <- file(str_c("./response/comp_experiment/remote/bash/", "submit_all_jobs.bash"),
                        "wb")  # "wb" for Unix line endings
writeLines(c(shebang,
           "",
           sbatch.array),
           con = conn_submit_all)
close(conn_submit_all)


