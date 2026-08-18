#Add frailty at baseline

library(dplyr)
library(data.table)
library(bit64)
library(parallel)

refsets <- fread("Z:/Group_work/QOF_project/QOF_codes_and_rules/PCD_Refset_Content_text_files/20250912_PCD_Refset_Content_by_output.txt")

table(refsets$Cluster_ID)

relevant <- refsets %>%
  filter(Cluster_ID %in% c("LTMILDFRAIL_COD", "MILDFRAIL_COD", "MODFRAIL_COD", "SEVFRAIL_COD")) %>%  #LD added Cluster_IDs for frailty
  rename(PCD_Refset_ID = `PCD Refset ID`,
         snomedctconceptid = SNOMED_code) 

table(relevant$Output_ID)   
table(relevant$Cluster_ID) 


# Apply codelists to cohort to create final set of required variables
dat <- readRDS("X:/QOF_project/Data/cohort/cohort_all_prelim.RDS")
codes <- relevant

#see Cluster_ID lists
table(codes$Cluster_ID)

#we want latest occurrence before our time period (aka 31/03/2023)

# Observation files
rds_dir <- "X:/QOF_project/Data/tidy_data/RDS/"
obs_files <- list.files(rds_dir)[grepl("Observation", list.files(rds_dir)) == TRUE] 


# Needed to run code in parallel
detectCores()
ncores <- 24

# Create a cluster
cl <- makeCluster(ncores)

# we should parallelise this process
clusterEvalQ(cl, { # these need to be read into each cluster
  library(data.table)
  library(dplyr)
  library(bit64)
})


#extraction pipe for codelists using QOF Cluster IDs
QOF_EXTRACTION_pipe <- function(obsfile, rds_dir = "X:/QOF_project/Data/tidy_data/RDS/", codes, cluster_id) {
  message(paste0("Cluster ID extracted: ", cluster_id))
  
  obs <- readRDS(paste0(rds_dir, obsfile))
  EXTRACT_COD <- codes %>% filter(Cluster_ID %in% cluster_id) %>%
    dplyr::pull(snomedctconceptid) %>% unique()
  
  obs <- obs[snomedctconceptid %in% EXTRACT_COD]
  obs <- obs[obsdate <= "2023-03-31"] 
  obs <- obs[ , .(patid, obsdate, snomedctconceptid)]
  obs <- unique(obs)
  
  return(obs)
  
}


clusterExport(cl, varlist = c("obs_files", "rds_dir", "QOF_EXTRACTION_pipe", "codes"), 
              envir = environment())

#apply pipe to codelists
FRAIL_DAT_big <- parLapply(cl, obs_files, QOF_EXTRACTION_pipe, codes = codes,
                         cluster_id = c("LTMILDFRAIL_COD", "MILDFRAIL_COD", "MODFRAIL_COD", "SEVFRAIL_COD"))


#Frailty
FRAIL_DAT <- rbindlist(FRAIL_DAT_big)
FRAIL_DAT <- unique(FRAIL_DAT)
FRAIL_DAT <- left_join(FRAIL_DAT, dplyr::select(dat, patid, ACHV, QSSD), by = "patid")
clustid <- codes %>% mutate(snomedctconceptid = as.integer64(snomedctconceptid)) %>% 
  dplyr::select(snomedctconceptid, Cluster_ID) %>% unique()
FRAIL_DAT <- left_join(FRAIL_DAT, dplyr::select(clustid, snomedctconceptid, Cluster_ID), by = "snomedctconceptid")
FRAIL_DAT <- FRAIL_DAT %>% arrange(patid, desc(obsdate)) %>% #latest
  group_by(patid) %>% slice(1) %>% ungroup()

FRAIL_DAT <- FRAIL_DAT %>% dplyr::select(patid, snomedctconceptid, Cluster_ID)
dat <- left_join(dplyr::select(dat, patid), FRAIL_DAT, by = "patid")

dat <- dat %>% dplyr::mutate(frailty_qof = case_when(
  Cluster_ID == "MILDFRAIL_COD" ~ "Mild Frailty",
  Cluster_ID == "MODFRAIL_COD" ~ "Moderate Frailty",
  Cluster_ID == "SEVFRAIL_COD" ~ "Severe Frailty",
  TRUE ~ "Not Frail"
))
# make factor
dat$frailty_qof <- factor(dat$frailty_qof, 
                                levels = c("Not Frail",
                                           "Mild Frailty",
                                           "Moderate Frailty",
                                           "Severe Frailty"))


#merge onto existing RDS with covars and outcomes
cohort <- readRDS("X:/QOF_project/Data/cohort/5_cohort_with_covars_and_outcomes_updated2.RDS")
cohort <- left_join(cohort, dplyr::select(dat, patid, frailty_qof), by = "patid")
cohort <- saveRDS(cohort, "X:/QOF_project/Data/cohort/5_cohort_with_covars_and_outcomes_updated2.RDS")


stopCluster(cl)


