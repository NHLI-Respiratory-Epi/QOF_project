# we need to sort out our cohort and apply all codelists to it
# Need to sor tout what we need for each code cluster - sometimes it's presence, sometimes it's first date, sometimes it's last date, etc

# Qualifying criteria that are useful across all QOF areas:

# earliest <= ACHV
# latest <= ACHV
# Latest <= ACHV_DAT AND > ASTLAT_DAT # requires ASTLAT
# Latest > (ACHV_DAT - 12 months) AND <= ACHV_DAT
# 'resolved' 
#Earliest >= QSSD AND <= ACHV_DAT:

#what we can do is at least create the earliest, latest, and earliest within 1 year and latest within 1 year,
# for all variables. 

# more bespoke measurements (not exhaustive):

# all between QSSD and ACHV: REV_DAT # special
# Earliest >= (ASTINVITE1_DAT + 7 days) AND <= ACHV_DAT # requires ASTINVITE1
# Earliest >= (EUNRESAST_DAT - 93 days) AND <= ACHV_DAT # requires EUNRESAST_DAT, which requires AST_DAT and ASTRES_DAT
# fev1fvc value required

# can also create the earliest 'unresolved' variables



library(data.table)
library(dplyr)
library(bit64)
library(parallel)

# read in all codelists for our QOF areas of interest
codes <- readRDS("codelists/all/ast_copd_hyp_smok.RDS")


# get our cohort and apply GMS registration status rule

cohort <- readRDS("X:/QOF_project/Data/tidy_data/RDS/Patient_practice.RDS")

cohort$ACHV <- as.Date("2024-03-31") # achievement date is 31st March 2024
cohort$QSSD <- as.Date("2023-04-01") # first date service provides 'quality service' i.e. start of reporting period

nrow(cohort)
cohort <- cohort %>% filter(regstartdate <= ACHV) # remove those registered afterwards (done at data extraction)
nrow(cohort)
cohort <- cohort %>% filter(is.na(regenddate) | regenddate > ACHV) # remove those who deregistered before ACHV
nrow(cohort)



# do parallel processing to extract the main dates etc we need for all codelists:

# load directory for RDS
rds_dir <- "X:/QOF_project/Data/tidy_data/RDS/"

rds_dir_files <- dir(rds_dir)

obs_files <- rds_dir_files[grepl("Observation", rds_dir_files) == TRUE]

#--------- start of the pipe for parallel processing
code_pipe_obs <- function(x, rds_dir, codes) {

# read in our observation file
# obs_keep <- readRDS("X:/QOF_project/Data/tidy_data/RDS/Observation_1.RDS") # if you want to see what's happening
  obs_keep <- readRDS(paste0(rds_dir, x))
  obs_keep$ACHV <- as.Date("2024-03-31") # add ACHV date
  obs_keep$QSSD <- as.Date("2023-04-01") # add QSSD / PPED / 1 year prior from ACHV
  obs_keep <- obs_keep[obsdate <= ACHV] # keep obs before ACHV

# create our cohort that we are binding our columns to
  cohort_obs <- obs_keep[ , .(patid)] %>% unique()





# loop that extracts through every codelist and returns a date for earliest, latest,
# earliest within 1 year of ACHV, latest within 1 year of ACHV

for (i in unique(codes$Cluster_ID)) {
  extract_codes <- codes %>% filter(Cluster_ID == i) %>% pull(snomedctconceptid) %>% unique()
  obs <- obs_keep[snomedctconceptid %in% extract_codes]
  
  if (nrow(obs) == 0L) {
    # Return an *empty* table with the right columns,
    # keeping patid for joining later
  obs <-  data.table(
      patid           = integer64(),
      earliest  = as.Date(character()),
      latest   = as.Date(character()),
      earliest_1yr = as.Date(character()),
      latest_1yr   = as.Date(character())
    )
  } else {

  obs <- obs %>% arrange(patid, obsdate) # arrange by patient and date
  setorder(obs, patid, obsdate)
  obs <- obs[, {
    # indices within the window
    idx_win <- which(obsdate >= QSSD & obsdate <= ACHV)
    
    list(
      earliest   = obsdate[1L],
      latest    = obsdate[.N],
      earliest_1yr  = if (length(idx_win)) obsdate[min(idx_win)] else as.Date(NA),
      latest_1yr    = if (length(idx_win)) obsdate[max(idx_win)] else as.Date(NA)
    )
  }, by = patid]
  
  }

  # ensure the codelist gets into the column name
  colnames(obs)[-1] <- paste0(i, "_", colnames(obs)[-1])
  
  # join onto the cohort
  cohort_obs <- obs[cohort_obs, on = "patid"]
}  
  
return(cohort_obs)
  
}

# ----------- end of the obs pipe for parallel processing


drugs_files <- rds_dir_files[grepl("DrugIssue", rds_dir_files) == TRUE]

#--------- start of the pipe for drugs parallel processing
code_pipe_drugs <- function(drugx, rds_dir, codes) {
  
  # read in our observation file
  # obs_keep <- readRDS("X:/QOF_project/Data/tidy_data/RDS/Observation_1.RDS") # if you want to see what's happening
  drugs_keep <- readRDS(paste0(rds_dir, drugx))
  drugs_keep$ACHV <- as.Date("2024-03-31") # add ACHV date
  drugs_keep$QSSD <- as.Date("2023-04-01") # add QSSD / PPED / 1 year prior from ACHV
  drugs_keep <- drugs_keep[issuedate <= ACHV] # keep obs before ACHV
  
  # create our cohort that we are binding our columns to
  cohort_drugs <- drugs_keep[ , .(patid)] %>% unique()
  
  
  
  
  
  # loop that extracts through every codelist and returns a date for earliest, latest,
  # earliest within 1 year of ACHV, latest within 1 year of ACHV
  
  for (i in unique(codes$Cluster_ID)) {
    extract_codes <- codes %>% filter(Cluster_ID == i) %>% pull(snomedctconceptid) %>% unique()
    drugs <- drugs_keep[dmdid %in% extract_codes]
    
    if (nrow(drugs) == 0L) {
      # Return an *empty* table with the right columns,
      # keeping patid for joining later
      drugs <-  data.table(
        patid           = integer64(),
        earliest  = as.Date(character()),
        latest   = as.Date(character()),
        earliest_1yr = as.Date(character()),
        latest_1yr   = as.Date(character())
      )
    } else {
      
      drugs <- drugs %>% arrange(patid, issuedate) # arrange by patient and date
      setorder(drugs, patid, issuedate)
      drugs <- drugs[, {
        # indices within the window
        idx_win <- which(issuedate >= QSSD & issuedate <= ACHV)
        
        list(
          earliest   = issuedate[1L],
          latest    = issuedate[.N],
          earliest_1yr  = if (length(idx_win)) issuedate[min(idx_win)] else as.Date(NA),
          latest_1yr    = if (length(idx_win)) issuedate[max(idx_win)] else as.Date(NA)
        )
      }, by = patid]
      
    }
    
    # ensure the codelist gets into the column name
    colnames(drugs)[-1] <- paste0(i, "_", colnames(drugs)[-1])
    
    # join onto the cohort
    cohort_drugs <- drugs[cohort_drugs, on = "patid"]
  }  
  
  return(cohort_drugs)
  
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
                              "code_pipe_obs", "code_pipe_drugs"), envir = environment())

bigobs <- parLapply(cl, obs_files, function(x) {
  code_pipe_obs(x, rds_dir = rds_dir, codes = codes)
})

bigobs <- rbindlist(bigobs)

bigdrugs <- parLapply(cl, drugs_files, function(x) {
  code_pipe_drugs(x, rds_dir = rds_dir, codes = codes)
})

bigdrugs <- rbindlist(bigdrugs)



# there should be no overlap - and there's not
bigobs %>% dplyr::select(where(~ all(is.na(.x)))) %>% colnames() 
bigobs %>% dplyr::select(where(~ !all(is.na(.x)))) %>% colnames() 
bigdrugs %>% dplyr::select(where(~ all(is.na(.x)))) %>% colnames()
bigdrugs %>% dplyr::select(where(~ !all(is.na(.x)))) %>% colnames()



bigobs <- bigobs %>% dplyr::select(where(~ !all(is.na(.x))))
bigdrugs <- bigdrugs %>% dplyr::select(where(~ !all(is.na(.x))))

# check the only similar one is patid. 
intersect(names(bigobs), names(bigdrugs))

stopCluster(cl)


cohort <- bigobs[cohort, on = "patid"]
cohort <- bigdrugs[cohort, on = "patid"]

cohort <- cohort %>% relocate(patid, pracid:QSSD)


saveRDS(cohort, "X:/QOF_project/Data/cohort/cohort_all.RDS")
saveRDS(cohort, "X:/QOF_project/Data/cohort/cohort_COPD.RDS")
saveRDS(cohort, "X:/QOF_project/Data/cohort/cohort_asthma.RDS")
saveRDS(cohort, "X:/QOF_project/Data/cohort/cohort_smoking.RDS")
saveRDS(cohort, "X:/QOF_project/Data/cohort/cohort_hypertension.RDS")

library(readstata13)
cohort$patid <- as.character(cohort$patid)
readstata13::save.dta13(cohort, "X:/QOF_project/Data/cohort/cohort_asthma.dta")
