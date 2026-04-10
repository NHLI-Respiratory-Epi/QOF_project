# Apply codelists to cohort to create AECOPDs and asthma attacks
library(dplyr)
library(data.table)
library(bit64)
library(parallel)

dat <- readRDS("X:/QOF_project/Data/cohort/cohort_all_prelim.RDS")
codes <- readRDS("X:/QOF_project/Hannah_Leah/codelists/all/ast_copd_hyp_smok_andmore.RDS") #This is updated to include Cluster_IDs for covars that weren't originally in
drugscodes <-readRDS("X:/QOF_project/Hannah_Leah/codelists/all/drugscodes.RDS")

#we want year before our qof period - 1st april 2022-31st march 2023

#codelists made by HW
abx_codes <- fread("X:/QOF_project/Creating_codelists/Covariates/excel/abx.csv")
aecopd_codes <- fread("X:/QOF_project/Creating_codelists/Covariates/excel/aecopd.csv")
asthma_attack_codes <- fread("X:/QOF_project/Creating_codelists/Covariates/excel/asthma_attack.csv")
lrti_codes <- fread("X:/QOF_project/Creating_codelists/Covariates/excel/lrti.csv") 
lrti_codes$medcodeid <- as.integer64(lrti_codes$medcodeid)
ocs_codes <- fread("X:/QOF_project/Creating_codelists/Covariates/excel/ocs.csv")
# rest will come from QOF clusters

# Observation files
rds_dir <- "X:/QOF_project/Data/tidy_data/RDS/"
obs_files <- list.files(rds_dir)[grepl("Observation", list.files(rds_dir)) == TRUE] 
drug_files <- list.files(rds_dir)[grepl("DrugIssue", list.files(rds_dir)) == TRUE]


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

#extraction pipe for non-QOF codelists
COVAR_extraction_pipe <- function(obsfile, rds_dir = "X:/QOF_project/Data/tidy_data/RDS/", codes) {
  
  obs <- readRDS(paste0(rds_dir, obsfile))
  EXTRACT_COD <- codes %>% dplyr::pull(medcodeid) %>% unique()
  
  obs <- obs[medcodeid %in% EXTRACT_COD]
  obs <- obs[obsdate <= "2023-03-31"] 
  obs <- obs[ , .(patid, obsdate)]
  obs <- unique(obs)
  
  return(obs)
}

#add everything needed for cluster
clusterExport(cl, varlist = c("obs_files", "rds_dir", "COVAR_extraction_pipe", "codes", 
                              "aecopd_codes", "asthma_attack_codes", "lrti_codes"), 
              envir = environment())

#apply pipe to codelists
AECOPD_DAT_big <- parLapply(cl, obs_files, COVAR_extraction_pipe, codes = aecopd_codes) 
ATTACK_DAT_big <- parLapply(cl, obs_files, COVAR_extraction_pipe, codes = asthma_attack_codes)
LRTI_DAT_big <- parLapply(cl, obs_files, COVAR_extraction_pipe, codes = lrti_codes) 

#AECOPD - want any occurrences in the "baseline" year
AECOPD_DAT <- rbindlist(AECOPD_DAT_big)
AECOPD_DAT <- left_join(AECOPD_DAT, dplyr::select(dat, patid, ACHV, QSSD), by = "patid")
AECOPD_DAT <- AECOPD_DAT %>% filter(obsdate <= "2023-03-31" & obsdate > "2022-03-31") %>% 
  rename(aecopd_date = obsdate) %>%
  dplyr::select(patid, aecopd_date) %>% unique()
AECOPD_DAT <- AECOPD_DAT %>% dplyr::mutate(aecopd = 1)
dat_w_aecopds <- AECOPD_DAT

#ASTHMA ATTACK - want occurrences in the "baseline" year
ATTACK_DAT <- rbindlist(ATTACK_DAT_big)
ATTACK_DAT <- left_join(ATTACK_DAT, dplyr::select(dat, patid, ACHV, QSSD), by = "patid")
ATTACK_DAT <- ATTACK_DAT %>% filter(obsdate <= "2023-03-31" & obsdate > "2022-03-31") %>% 
  rename(asthma_attack_date = obsdate) %>%
  dplyr::select(patid, asthma_attack_date) %>% unique() 
ATTACK_DAT <- ATTACK_DAT %>% dplyr::mutate(asthma_attack = 1)
dat_w_attacks <- ATTACK_DAT

#LRTI - want occurrences in the "baseline" year
LRTI_DAT <- rbindlist(LRTI_DAT_big)
LRTI_DAT <- left_join(LRTI_DAT, dplyr::select(dat, patid, ACHV, QSSD), by = "patid")
LRTI_DAT <- LRTI_DAT %>% filter(obsdate <= "2023-03-31" & obsdate > "2022-03-31") %>% 
  rename(lrti_date = obsdate) %>%
  dplyr::select(patid, lrti_date) %>% unique() 
LRTI_DAT <- LRTI_DAT %>% dplyr::mutate(lrti = 1)
dat_w_aecopds_lrti <- full_join(dat_w_aecopds, LRTI_DAT, by = c("patid", "aecopd_date" = "lrti_date"))


#extraction pipe for medication codelists
COVAR_MEDS_extraction_pipe <- function(drugfile, rds_dir = "X:/QOF_project/Data/tidy_data/RDS/", codes) {
  
  obs <- readRDS(paste0(rds_dir, drugfile))
  EXTRACT_COD <- codes %>% dplyr::pull(prodcodeid) %>% unique()
  
  obs <- obs[prodcodeid %in% EXTRACT_COD]
  obs <- obs[issuedate <= "2023-03-31"]
  obs <- obs[ , .(patid, issuedate, prodcodeid, duration)]
  obs <- unique(obs)
  
  return(obs)
}

#add everything needed for cluster
clusterExport(cl, varlist = c("drug_files", "rds_dir", "COVAR_MEDS_extraction_pipe", "codes", 
                              "abx_codes", "ocs_codes"), 
              envir = environment())

#apply pipe to codelists
ABX_DAT_big <- parLapply(cl, drug_files, COVAR_MEDS_extraction_pipe, codes = abx_codes) 
OCS_DAT_big <- parLapply(cl, drug_files, COVAR_MEDS_extraction_pipe, codes = ocs_codes) 

#ABX - want any occurrences in the "baseline" year
ABX_DAT <- rbindlist(ABX_DAT_big)
ABX_DAT <- left_join(ABX_DAT, dplyr::select(dat, patid, ACHV, QSSD), by = "patid")
ABX_DAT <- ABX_DAT %>% filter(issuedate <= "2023-03-31" & issuedate > "2022-03-31") %>% 
  filter(duration >= 5 & duration <= 14) %>% 
  rename(abx_date = issuedate) %>% 
  dplyr::select(patid, abx_date) %>% unique()
#table(ABX_DAT$duration, useNA = "always") #looks like for at least this year period, no NAs for duration so let's add it in - LD thinks duration == 0 is more the problem
ABX_DAT <- ABX_DAT %>% dplyr::mutate(abx = 1)
dat_w_aecopds_lrti_abx <- full_join(dat_w_aecopds_lrti, ABX_DAT, by = c("patid", "aecopd_date" = "abx_date"))


#OCS - want any occurrences in the "baseline" year
OCS_DAT <- rbindlist(OCS_DAT_big)
OCS_DAT <- left_join(OCS_DAT, dplyr::select(dat, patid, ACHV, QSSD), by = "patid")
OCS_DAT <- OCS_DAT %>% filter(issuedate <= "2023-03-31" & issuedate > "2022-03-31") %>% 
  filter(duration >= 5 & duration <= 14) %>% 
  rename(ocs_date = issuedate) %>% 
  dplyr::select(patid, ocs_date) %>% unique()
#table(OCS_DAT$duration, useNA = "always") #looks like for at least this year period, no NAs for duration so let's add it in - LD thinks duration == 0 is more the problem
OCS_DAT <- OCS_DAT %>% dplyr::mutate(ocs = 1)
#need OCS for both AECOPD and asthma attacks
dat_w_aecopds_lrti_abx_ocs <- full_join(dat_w_aecopds_lrti_abx, OCS_DAT, by = c("patid", "aecopd_date" = "ocs_date"))
dat_w_attacks_ocs <- full_join(dat_w_attacks, OCS_DAT, by = c("patid", "asthma_attack_date" = "ocs_date"))

#extraction pipe for codelists using QOF Cluster IDs
QOF_EXTRACTION_pipe <- function(obsfile, rds_dir = "X:/QOF_project/Data/tidy_data/RDS/", codes, cluster_id) {
  message(paste0("Cluster ID extracted: ", cluster_id))
  
  obs <- readRDS(paste0(rds_dir, obsfile))
  EXTRACT_COD <- codes %>% filter(Cluster_ID %in% cluster_id) %>%
    dplyr::pull(snomedctconceptid) %>% unique()
  
  obs <- obs[snomedctconceptid %in% EXTRACT_COD]
  obs <- obs[obsdate <= "2023-03-31"] 
  obs <- obs[ , .(patid, obsdate)]
  obs <- unique(obs)
  
  return(obs)
  
}


clusterExport(cl, varlist = c("obs_files", "rds_dir", "QOF_EXTRACTION_pipe", "codes"), 
              envir = environment())

#apply pipe to codelists
COPD_REVIEW_DAT_big <- parLapply(cl, obs_files, QOF_EXTRACTION_pipe, codes = codes,
                                 cluster_id = "COPDRVW_COD")
ASTHMA_REVIEW_DAT_big <- parLapply(cl, obs_files, QOF_EXTRACTION_pipe, codes = codes,
                                   cluster_id = "REV_COD")

#COPD_REVIEW - want all occurrences because need this for AECOPD definition
COPD_REVIEW_DAT <- rbindlist(COPD_REVIEW_DAT_big)
COPD_REVIEW_DAT <- unique(COPD_REVIEW_DAT)
COPD_REVIEW_DAT <- left_join(COPD_REVIEW_DAT, dplyr::select(dat, patid, ACHV, QSSD), by = "patid")
COPD_REVIEW_DAT <- COPD_REVIEW_DAT %>% filter(obsdate <= "2023-03-31" & obsdate > "2022-03-31") %>% 
  rename(copd_review_date = obsdate) %>%
  dplyr::select(patid, copd_review_date) %>% unique()
COPD_REVIEW_DAT <- COPD_REVIEW_DAT %>% dplyr::mutate(copd_review_qof = 1)
dat_w_aecopds_lrti_abx_ocs_rev <- full_join(dat_w_aecopds_lrti_abx_ocs, COPD_REVIEW_DAT, by = c("patid", "aecopd_date" = "copd_review_date"))

#ASTHMA_REVIEW - want all occurrences because need this for asthma attack definition
ASTHMA_REVIEW_DAT <- rbindlist(ASTHMA_REVIEW_DAT_big)
ASTHMA_REVIEW_DAT <- unique(ASTHMA_REVIEW_DAT)
ASTHMA_REVIEW_DAT <- left_join(ASTHMA_REVIEW_DAT, dplyr::select(dat, patid, ACHV, QSSD), by = "patid")
ASTHMA_REVIEW_DAT <- ASTHMA_REVIEW_DAT %>% filter(obsdate <= "2023-03-31" & obsdate > "2022-03-31") %>% 
  rename(asthma_review_date = obsdate) %>%
  dplyr::select(patid, asthma_review_date) %>% unique()
ASTHMA_REVIEW_DAT <- ASTHMA_REVIEW_DAT %>% dplyr::mutate(asthma_review_qof = 1)
dat_w_attacks_ocs_rev <- full_join(dat_w_attacks_ocs, ASTHMA_REVIEW_DAT, by = c("patid", "asthma_attack_date" = "asthma_review_date"))





# Save these dfs for AECOPDs and asthma attacks
dat_w_aecopds_lrti_abx_ocs_rev <- saveRDS(dat_w_aecopds_lrti_abx_ocs_rev, "X:/QOF_project/Data/cohort/4_cohort_with_aecopds.RDS")
dat_w_attacks_ocs_rev <- saveRDS(dat_w_attacks_ocs_rev, "X:/QOF_project/Data/cohort/4_cohort_with_attacks.RDS")


stopCluster(cl)


# Looking to the future:
# add in comorbidities etc up to 31st march 2023 (or within year prior as appropriate) -> also compare to specific # of exacs term used in QOF def
# add in copd / asthma exacerbations 1st april 2023-31st march 2024
# add in copd / asthma exacerbations 1st april 2024-end of study period (rates, also date of first event)

# potentially include the QIs in the year before 31st march 2023? 
# potentially include the QIs following achievement date






