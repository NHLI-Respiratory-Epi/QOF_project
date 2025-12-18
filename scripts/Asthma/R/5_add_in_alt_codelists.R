# create the clinical data extraction criteria

library(dplyr)
library(data.table)
library(bit64)
library(parallel)
library(readxl)

dat <- readRDS("Z:/Group_work/QOF_project/Data/Asthma/cohort/2_cohort_asthma_with_correct_DAT_cols.RDS")


colnames(dat)

# new codelists
codes_drugs <- readRDS("Z:/Group_work/QOF_project/Alex/QOF_project/codelists/all/AA_drugs_combined_QOF_JKQ_codelist.RDS")
AA_minimal <- read_xlsx("Z:/Group_work/QOF_project/Alex/QOF_project/codelists/all/Asthma_JKQ_condensed.xlsx")

# get directory and files
rds_dir <- "X:/QOF_project/Data/tidy_data/RDS/"
drugs_files <- list.files(rds_dir)[grepl("DrugIssue", list.files(rds_dir)) == TRUE] 
obs_files <- list.files(rds_dir)[grepl("OBservation", list.files(rds_dir)) == TRUE] 


broad_codelist <- codes_drugs %>% filter(jkq == 1) %>% pull(conceptId)
ics_codelist <- codes_drugs %>% filter(jkq_ics == 1) %>% pull(conceptId)
AA_minimal$conceptId <- as.integer64(AA_minimal$conceptId)

str(broad_codelist)
str(ics_codelist)

# pipe for ASTTRT - luckily it is very simple and no other variables are based upon it so can just do it here.
DRUGS_EXTRACTION_pipe <- function(drugsfile, rds_dir = "X:/QOF_project/Data/tidy_data/RDS/", codelist) {
  # prevent accidentally extracting the wrong thing
  # although tbh this backfired because when it used to be called 'Cluster_ID' it 
  # messed up the pipe completely - had to change to cluster_id
  
  drugs <- readRDS(paste0(rds_dir, drugsfile))
  EXTRACT_COD <- codelist
  
  drugs <- drugs[!is.na(dmdid)]
  drugs <- drugs[dmdid %in% EXTRACT_COD]
  drugs <- drugs[issuedate <= "2024-03-31" & issuedate > "2023-03-31"]
  #  drugs <- drugs[issuedate <= "2024-03-31"] # can remove line above and unhash this for 'pharmdrug_dat'
  drugs <- drugs[ , .(patid, issuedate)]
  drugs <- unique(drugs)
  drugs <- drugs %>% arrange(patid, desc(issuedate)) %>% # latest within 1 year
    group_by(patid) %>%
    slice(1) %>% ungroup()
  
  return(drugs)
  
}


Sys.time()


detectCores()
ncores <- 80 # others might be using 80...

# Create a cluster
cl <- makeCluster(ncores)

# we should parallelise this process

clusterEvalQ(cl, { # these need to be read into each cluster
  library(data.table)
  library(dplyr)
  library(bit64)
})


# these R objects are necessary in each cluster
clusterExport(cl, varlist = c("drugs_files", "rds_dir", "broad_codelist", "ics_codelist",
                              "DRUGS_EXTRACTION_pipe"), envir = environment())


broad_drugs <- parLapply(cl, drugs_files, DRUGS_EXTRACTION_pipe, rds_dir = rds_dir, 
                         codelist = broad_codelist)


broad_drugs <- rbindlist(broad_drugs)

broad_drugs <- broad_drugs %>% rename(ASTTRT_DAT_alt_broad = issuedate)


ics_drugs <- parLapply(cl, drugs_files, DRUGS_EXTRACTION_pipe, rds_dir = rds_dir, 
                       codelist = ics_codelist)


ics_drugs <- rbindlist(ics_drugs)

ics_drugs <- ics_drugs %>% rename(ASTTRT_DAT_alt_ics = issuedate)


stopCluster(cl)



dat <- left_join(dat, broad_drugs, by = "patid")
dat <- left_join(dat, ics_drugs, by = "patid")




saveRDS(dat, "Z:/Group_work/QOF_project/Data/Asthma/cohort/4_cohort_asthma_with_alt_ASTTRT_cols.RDS")



