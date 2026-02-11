

library(data.table)
library(dplyr)
library(bit64)
library(parallel)

# read in the codelist
codes <- readRDS("codelists/all/ast_copd_hyp_smok.RDS")

# make sure it's a data table
setDT(codes)
# # # #  do any require processing of codelist here





# read in our cohort
dat <- readRDS("X:/QOF_project/Data/cohort/cohort_all_prelim.RDS")

# make sure it's a data table

setDT(dat)

# do parallel processing to extract the main dates etc we need for all codelists:

# load directory for RDS
rds_dir <- "X:/QOF_project/Data/tidy_data/RDS/"

rds_dir_files <- dir(rds_dir)

obs_files <- rds_dir_files[grepl("Observation", rds_dir_files) == TRUE]

#--------- start of the pipe for parallel processing
code_pipe_obs <- function(x, rds_dir, codelist) {

# read in our observation file
# obs_keep <- readRDS("X:/QOF_project/Data/tidy_data/RDS/Observation_1.RDS") # if you want to see what's happening
  
  # this will work best with one column for medcodeid, and one column for include/exclude
  obs <- readRDS(paste0(rds_dir, x)) # read in obs
  obs <- obs[codelist, on = "medcodeid"] # just keep observations with the required medcodeid
  
  # you can drop the columns you don't want here in the function, or do it later on
  
  return(obs)
  
}



# then do the actual parallel processing

Sys.time()


detectCores()
ncores <- 80

# Create a cluster
cl <- makeCluster(ncores)

# we should parallelise this process

clusterEvalQ(cl, { # these need to be read into each cluster
  library(data.table)
  library(dplyr)
  library(bit64)
})


# these R objects are necessary in each cluster
clusterExport(cl, varlist = c("obs_files", "rds_dir", "codes",
                              "code_pipe_obs"), envir = environment())

bigobs <- parLapply(cl, obs_files, function(x) {
  code_pipe_obs(x, rds_dir = rds_dir, codes = codes)
})

stopCluster(cl)


bigobs <- rbindlist(bigobs)

# here, you can apply your inclusion criteria for the observations
# usually, covariates required before the start date, outcomes required after the start date but before the end date
# to access them, you need to bind the relevant variables from the cohort to what you have here

dat_criteria <- dat[, .(patid, ACHV)]

bigobs <- dat_criteria[bigobs, on = "patid"]

# then apply whatever you need to do on all the observations
# this is often filtering based on dates, just taking the first observation if there are multiple, etc
# sometimes you will need to clean up the value from an observation
# then, once you've done this, you apply what you've done back to the cohort:

dat <- bigobs[dat, on = "patid"]


# then save your new cohort with the new column added
# I usually add a number at the end to indicate the order it's been created

# saveRDS(dat, filepath)

