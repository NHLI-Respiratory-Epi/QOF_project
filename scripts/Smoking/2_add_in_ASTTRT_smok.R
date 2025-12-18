library(data.table)
library(dplyr)
library(bit64)
library(parallel)

dat <- readRDS("Z:/Group_work/QOF_project/Data/Smoking/cohort/1_cohort_smoking_with_DAT_cols.RDS")

dat %>% select(starts_with(c("ASTTRT"
                             , "PHARMDRUG"
                             ))) %>% colnames() # these columns are wrong
dat <- dat %>% select(!(starts_with(c("ASTTRT"
                                     , "PHARMDRUG"
 )))) # remove the erroneous columns

# drugs codelists
codes_drugs <- readRDS("Z:/Group_work/QOF_project/Alex/QOF_project/codelists/all/drugscodes.RDS")

# get directory and files
rds_dir <- "X:/QOF_project/Data/tidy_data/RDS/"
drugs_files <- list.files(rds_dir)[grepl("DrugIssue", list.files(rds_dir)) == TRUE] 


# pipe for ASTTRT - luckily it is very simple and no other variables are based upon it so can just do it here.
DRUGS_EXTRACTION_pipe <- function(drugsfile,
                                  rds_dir = "X:/QOF_project/Data/tidy_data/RDS/",
                                  codes_drugs,
                                  cluster_id,
                                  min_date = TRUE) {
  
  message(paste0("Cluster ID extracted: ", cluster_id))
  
  drugs <- readRDS(paste0(rds_dir, drugsfile))
  EXTRACT_COD <- codes_drugs %>% filter(Cluster_ID == cluster_id) %>%  
    dplyr::pull(snomedctconceptid) %>% unique()
  
  
  drugs <- drugs[!is.na(dmdid)]
  drugs <- drugs[dmdid %in% EXTRACT_COD]
  if(min_date) {
    drugs <- drugs[issuedate <= "2024-03-31" & issuedate > "2023-03-31"]
  } else {
    drugs <- drugs[issuedate <= "2024-03-31"]
  }
  drugs <- drugs[ , .(patid, issuedate)]
  drugs <- unique(drugs)
  drugs <- drugs %>% arrange(patid, desc(issuedate)) %>% # latest within 1 year
    group_by(patid) %>%
    slice(1) %>% ungroup()
  
  return(drugs)
  
}


Sys.time()


detectCores()
ncores <- 10 # others might be using 80...

# Create a cluster
cl <- makeCluster(ncores)

# we should parallelise this process

clusterEvalQ(cl, { # these need to be read into each cluster
  library(data.table)
  library(dplyr)
  library(bit64)
})


# these R objects are necessary in each cluster
clusterExport(cl, varlist = c("drugs_files", "rds_dir", "codes_drugs",
                              "DRUGS_EXTRACTION_pipe"), envir = environment())


bigdrugs <- parLapply(cl, drugs_files, DRUGS_EXTRACTION_pipe, rds_dir = rds_dir, 
                      codes_drugs = codes_drugs, cluster_id = "ASTTRT_COD")


bigdrugs <- rbindlist(bigdrugs)


stopCluster(cl)


bigdrugs <- bigdrugs %>% rename(ASTTRT_DAT = issuedate)

dat <- left_join(dat, bigdrugs, by = "patid")

# PHARMDRUG

# Create a cluster
cl <- makeCluster(ncores)

# we should parallelise this process

clusterEvalQ(cl, { # these need to be read into each cluster
  library(data.table)
  library(dplyr)
  library(bit64)
})


# these R objects are necessary in each cluster
clusterExport(cl, varlist = c("drugs_files", "rds_dir", "codes_drugs",
                              "DRUGS_EXTRACTION_pipe"), envir = environment())


bigdrugs <- parLapply(cl,
                      drugs_files,
                      DRUGS_EXTRACTION_pipe,
                      rds_dir = rds_dir, 
                      codes_drugs = codes_drugs,
                      cluster_id = "PHARMDRUG_COD",
                      min_date = FALSE)

bigdrugs <- rbindlist(bigdrugs)

stopCluster(cl)

bigdrugs <- bigdrugs %>%
  rename(PHARMDRUG_DAT = issuedate)

dat <- left_join(dat,
                 bigdrugs,
                 by = "patid")


dat <- saveRDS(dat, "Z:/Group_work/QOF_project/Data/Smoking/cohort/2_cohort_smoking_with_correct_DAT_cols.RDS")

