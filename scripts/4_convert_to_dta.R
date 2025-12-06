#

library(data.table)
library(dplyr)
library(bit64)
library(readstata13)
library(parallel)

rds_dir <- "X:/QOF_project/Data/tidy_data/RDS/"

rds_dir_files <- dir(rds_dir)


# need to do this for observations and drugissues (cohort and patient/practice done previously).

obs_files <- rds_dir_files[grepl("Observation", rds_dir_files) == TRUE]
drugs_files <- rds_dir_files[grepl("DrugIssue", rds_dir_files) == TRUE]


obs_pipe <- function(x) {
  datobs <- readRDS(paste0("X:/QOF_project/Data/tidy_data/RDS/", x))

# save Stata
datobs$patid               <- as.character(datobs$patid)
datobs$medcodeid           <- as.character(datobs$medcodeid)
datobs$snomedctconceptid   <- as.character(datobs$snomedctconceptid)


filenameobs <- sub("\\.RDS$", ".dta", x, ignore.case = TRUE)
readstata13::save.dta13(
  datobs,
  paste0("X:/QOF_project/Data/tidy_data/dta/", filenameobs),
  convert.dates = TRUE)
}


drugs_pipe <- function(x) {
  datdrugs <- readRDS(paste0("X:/QOF_project/Data/tidy_data/RDS/", x))
  
  # save Stata
  datdrugs$patid     <- as.character(datdrugs$patid)
  datdrugs$prodcodeid <- as.character(datdrugs$prodcodeid)
  datdrugs$dmdid     <- as.character(datdrugs$dmdid)
  
  
  filenamedrugs <- sub("\\.RDS$", ".dta", x, ignore.case = TRUE)
  readstata13::save.dta13(
    datdrugs,
    paste0("X:/QOF_project/Data/tidy_data/dta/", filenamedrugs),
    convert.dates = TRUE)
}


# # save Stata
# datdrugs$patid     <- as.character(datdrugs$patid)
# datdrugs$prodcodeid <- as.character(datdrugs$prodcodeid)
# datdrugs$dmdid     <- as.character(datdrugs$dmdid)
# 
# readstata13::save.dta13(
#   datdrugs,
#   paste0(dta_dir, filenamedrugs, ".dta"),
#   convert.dates = TRUE
# )


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
  library(readstata13)
})



# these R objects are necessary in each cluster
clusterExport(cl, varlist = c("obs_files", "drugs_files", "obs_pipe", "drugs_pipe"), 
              envir = environment())

parLapply(cl, obs_files, obs_pipe) 
parLapply(cl, drugs_files, drugs_pipe)

stopCluster(cl)



