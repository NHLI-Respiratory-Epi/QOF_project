# Apply codelists to cohort to create final set of required variables

library(dplyr)
library(data.table)
library(bit64)
library(parallel)

dat <- readRDS("X:/QOF_project/Data/cohort/cohort_all_prelim.RDS")
codes <- readRDS("X:/QOF_project/Hannah_Leah/codelists/all/ast_copd_hyp_smok_andmore.RDS") #This is updated to include Cluster_IDs for covars that weren't originally in
drugscodes <-readRDS("X:/QOF_project/Hannah_Leah/codelists/all/drugscodes.RDS")

#see Cluster_ID lists in both
table(codes$Cluster_ID)
table(drugscodes$Cluster_ID)

#we want latest occurrence before our time period (aka 31/03/2023)

#codelists made by HW
bmi_codes <- fread("X:/QOF_project/Creating_codelists/Covariates/excel/bmi.csv")
gord_codes <- fread("X:/QOF_project/Creating_codelists/Covariates/excel/gord.csv")
#smoke_codes <- Use QOF but hold on until created
#ethnicity in its own script
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
  obs <- obs[obsdate <= "2023-03-31"] #want closest to before that date
  obs <- obs[ , .(patid, obsdate, medcodeid, value)] #may need to merge on terms for BMI to understand values
  obs <- unique(obs)
  
  return(obs)
}

#add everything needed for cluster
clusterExport(cl, varlist = c("obs_files", "rds_dir", "COVAR_extraction_pipe", "codes", 
                              "bmi_codes", "gord_codes"), 
              envir = environment())

#apply pipe to codelists
BMI_DAT_big <- parLapply(cl, obs_files, COVAR_extraction_pipe, codes = bmi_codes) 
GORD_DAT_big <- parLapply(cl, obs_files, COVAR_extraction_pipe, codes = gord_codes) 

#BMI
BMI_DAT <- rbindlist(BMI_DAT_big)
BMI_DAT <- left_join(BMI_DAT, dplyr::select(dat, patid, ACHV, QSSD), by = "patid")
BMI_DAT <- BMI_DAT %>% filter(medcodeid == "100716012") %>% filter(!is.na(value)) %>%  #medcodeid with the most observations
  filter(value >= 10 & value <= 80) %>%
  arrange(patid, desc(obsdate)) %>% #latest
  group_by(patid) %>% slice(1) %>% ungroup() %>% rename(BMI = value) %>% 
  dplyr::select(patid, BMI)
summary(BMI_DAT$BMI)

BMI_DAT2 <- rbindlist(BMI_DAT_big)
BMI_DAT2 <- left_join(BMI_DAT2, dplyr::select(dat, patid, ACHV, QSSD), by = "patid")
BMI_DAT2 <- BMI_DAT2 %>% filter(medcodeid != "100716012") %>% filter(!is.na(value)) %>%
  filter(value >= 10 & value <= 80) %>%
  arrange(patid, desc(obsdate)) %>% #latest
  group_by(patid) %>% slice(1) %>% ungroup() %>% rename(BMI2 = value) %>% 
  dplyr::select(patid, BMI2)
summary(BMI_DAT2$BMI2)

bmi_both <- left_join(dplyr::select(dat, patid, ACHV, QSSD), BMI_DAT, by = "patid")
bmi_both <- left_join(bmi_both, BMI_DAT2, by = "patid")
summary(bmi_both$BMI)
summary(bmi_both$BMI2)
bmi_both <- bmi_both %>% mutate(BMI = if_else(!is.na(BMI), 
                                              BMI, BMI2))
summary(bmi_both$BMI) #adds on about 10k BMIs

bmi_both <- bmi_both %>% dplyr::select(patid, BMI)
dat <- left_join(dat, bmi_both, by = "patid")

#GORD
GORD_DAT <- rbindlist(GORD_DAT_big)
GORD_DAT <- left_join(GORD_DAT, dplyr::select(dat, patid, ACHV, QSSD), by = "patid")
GORD_DAT <- GORD_DAT %>% arrange(patid, desc(obsdate)) %>% #latest
  group_by(patid) %>% slice(1) %>% ungroup()
GORD_DAT <- GORD_DAT %>% dplyr::mutate(gord = 1) %>% dplyr::select(patid, gord)
dat <- left_join(dat, GORD_DAT, by = "patid")


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
CHD_DAT_big <- parLapply(cl, obs_files, QOF_EXTRACTION_pipe, codes = codes,
                         cluster_id = "CHD_COD")
PAD_DAT_big <- parLapply(cl, obs_files, QOF_EXTRACTION_pipe, codes = codes,
                         cluster_id = "PAD_COD")
STROKE_DAT_big <- parLapply(cl, obs_files, QOF_EXTRACTION_pipe, codes = codes, 
                         cluster_id = c("OSTR_COD", "HSTRK_COD", "STRK_COD"))
TIA_DAT_big <- parLapply(cl, obs_files, QOF_EXTRACTION_pipe, codes = codes, 
                         cluster_id = "TIA_COD")
DIABETES_DAT_big <- parLapply(cl, obs_files, QOF_EXTRACTION_pipe, codes = codes,
                              cluster_id = c("DM_COD", "DMTYPE2_COD"))
CKD_DAT_big <- parLapply(cl, obs_files, QOF_EXTRACTION_pipe, codes = codes,
                         cluster_id = c("CKD_COD", "CKD1ANDCKD2_COD"))
MHEALTH_DAT_big <- parLapply(cl, obs_files, QOF_EXTRACTION_pipe, codes = codes,
                             cluster_id = "MH_COD")
HYPERCHOL_DAT_big <- parLapply(cl, obs_files, QOF_EXTRACTION_pipe, codes = codes,
                              cluster_id = "FHYP_COD")
HTN_DAT_big <- parLapply(cl, obs_files, QOF_EXTRACTION_pipe, codes = codes,
                         cluster_id = "HYP_COD")
ASTHMA_PLAN_DAT_big <- parLapply(cl, obs_files, QOF_EXTRACTION_pipe, codes = codes,
                                 cluster_id = "WRITPASTP_COD")
SMOKEREF_DAT_big <- parLapply(cl, obs_files, QOF_EXTRACTION_pipe, codes = codes,
                              cluster_id = "REFERSSSA_COD")
SMOKECESS_DAT_big <- parLapply(cl, obs_files, QOF_EXTRACTION_pipe, codes = codes,
                               cluster_id = "PHARM_COD")


#CHD
CHD_DAT <- rbindlist(CHD_DAT_big)
CHD_DAT <- unique(CHD_DAT)
CHD_DAT <- left_join(CHD_DAT, dplyr::select(dat, patid, ACHV, QSSD), by = "patid")
CHD_DAT <- CHD_DAT %>% arrange(patid, desc(obsdate)) %>% #latest
  group_by(patid) %>% slice(1) %>% ungroup()
CHD_DAT <- CHD_DAT %>% dplyr::mutate(chd_qof = 1) %>% dplyr::select(patid, chd_qof)
dat <- left_join(dat, CHD_DAT, by = "patid")

#PAD
PAD_DAT <- rbindlist(PAD_DAT_big)
PAD_DAT <- unique(PAD_DAT)
PAD_DAT <- left_join(PAD_DAT, dplyr::select(dat, patid, ACHV, QSSD), by = "patid")
PAD_DAT <- PAD_DAT %>% arrange(patid, desc(obsdate)) %>% #latest
  group_by(patid) %>% slice(1) %>% ungroup()
PAD_DAT <- PAD_DAT %>% dplyr::mutate(pad_qof = 1) %>% dplyr::select(patid, pad_qof)
dat <- left_join(dat, PAD_DAT, by = "patid")

#STROKE
STROKE_DAT <- rbindlist(STROKE_DAT_big)
STROKE_DAT <- unique(STROKE_DAT)
STROKE_DAT <- left_join(STROKE_DAT, dplyr::select(dat, patid, ACHV, QSSD), by = "patid")
STROKE_DAT <- STROKE_DAT %>% arrange(patid, desc(obsdate)) %>% #latest
  group_by(patid) %>% slice(1) %>% ungroup()
STROKE_DAT <- STROKE_DAT %>% dplyr::mutate(stroke_qof = 1) %>% dplyr::select(patid, stroke_qof)
dat <- left_join(dat, STROKE_DAT, by = "patid")

#TIA
TIA_DAT <- rbindlist(TIA_DAT_big)
TIA_DAT <- unique(TIA_DAT)
TIA_DAT <- left_join(TIA_DAT, dplyr::select(dat, patid, ACHV, QSSD), by = "patid")
TIA_DAT <- TIA_DAT %>% arrange(patid, desc(obsdate)) %>% #latest
  group_by(patid) %>% slice(1) %>% ungroup() 
TIA_DAT <- TIA_DAT %>% dplyr::mutate(tia_qof = 1) %>% dplyr::select(patid, tia_qof)
dat <- left_join(dat, TIA_DAT, by = "patid")

#DIABETES
DIABETES_DAT <- rbindlist(DIABETES_DAT_big)
DIABETES_DAT <- unique(DIABETES_DAT)
DIABETES_DAT <- left_join(DIABETES_DAT, dplyr::select(dat, patid, ACHV, QSSD), by = "patid")
DIABETES_DAT <- DIABETES_DAT %>% arrange(patid, desc(obsdate)) %>% #latest
  group_by(patid) %>% slice(1) %>% ungroup()
DIABETES_DAT <- DIABETES_DAT %>% dplyr::mutate(diabetes_qof = 1) %>% dplyr::select(patid, diabetes_qof)
dat <- left_join(dat, DIABETES_DAT, by = "patid")

#CKD
CKD_DAT <- rbindlist(CKD_DAT_big)
CKD_DAT <- unique(CKD_DAT)
CKD_DAT <- left_join(CKD_DAT, dplyr::select(dat, patid, ACHV, QSSD), by = "patid")
CKD_DAT <- CKD_DAT %>% arrange(patid, desc(obsdate)) %>% #latest
  group_by(patid) %>% slice(1) %>% ungroup()
CKD_DAT <- CKD_DAT %>% dplyr::mutate(ckd_qof = 1) %>% dplyr::select(patid, ckd_qof)
dat <- left_join(dat, CKD_DAT, by = "patid")

#MHEALTH
MHEALTH_DAT <- rbindlist(MHEALTH_DAT_big)
MHEALTH_DAT <- unique(MHEALTH_DAT)
MHEALTH_DAT <- left_join(MHEALTH_DAT, dplyr::select(dat, patid, ACHV, QSSD), by = "patid")
MHEALTH_DAT <- MHEALTH_DAT %>% arrange(patid, desc(obsdate)) %>% #latest
  group_by(patid) %>% slice(1) %>% ungroup()
MHEALTH_DAT <- MHEALTH_DAT %>% dplyr::mutate(mhealth_qof = 1) %>% dplyr::select(patid, mhealth_qof)
dat <- left_join(dat, MHEALTH_DAT, by = "patid")

#HYPERCHOL
HYPERCHOL_DAT <- rbindlist(HYPERCHOL_DAT_big)
HYPERCHOL_DAT <- unique(HYPERCHOL_DAT)
HYPERCHOL_DAT <- left_join(HYPERCHOL_DAT, dplyr::select(dat, patid, ACHV, QSSD), by = "patid")
HYPERCHOL_DAT <- HYPERCHOL_DAT %>% arrange(patid, desc(obsdate)) %>% #latest
  group_by(patid) %>% slice(1) %>% ungroup()
HYPERCHOL_DAT <- HYPERCHOL_DAT %>% dplyr::mutate(hyperchol_qof = 1) %>% dplyr::select(patid, hyperchol_qof)
dat <- left_join(dat, HYPERCHOL_DAT, by = "patid")

#HTN
HTN_DAT <- rbindlist(HTN_DAT_big)
HTN_DAT <- unique(HTN_DAT)
HTN_DAT <- left_join(HTN_DAT, dplyr::select(dat, patid, ACHV, QSSD), by = "patid")
HTN_DAT <- HTN_DAT %>% arrange(patid, desc(obsdate)) %>% #latest
  group_by(patid) %>% slice(1) %>% ungroup()
HTN_DAT <- HTN_DAT %>% dplyr::mutate(htn_qof = 1) %>% dplyr::select(patid, htn_qof)
dat <- left_join(dat, HTN_DAT, by = "patid")

#ASTHMA_PLAN
ASTHMA_PLAN_DAT <- rbindlist(ASTHMA_PLAN_DAT_big)
ASTHMA_PLAN_DAT <- unique(ASTHMA_PLAN_DAT)
ASTHMA_PLAN_DAT <- left_join(ASTHMA_PLAN_DAT, dplyr::select(dat, patid, ACHV, QSSD), by = "patid")
ASTHMA_PLAN_DAT <- ASTHMA_PLAN_DAT %>% arrange(patid, desc(obsdate)) %>% #latest
  group_by(patid) %>% slice(1) %>% ungroup() 
ASTHMA_PLAN_DAT <- ASTHMA_PLAN_DAT %>% dplyr::mutate(asthma_plan_qof = 1) %>% dplyr::select(patid, asthma_plan_qof)
dat <- left_join(dat, ASTHMA_PLAN_DAT, by = "patid")

#SMOKEREF ***MAY NEED TO NOT JUST DO LATEST OCCURRENCE - ASK AA
SMOKEREF_DAT <- rbindlist(SMOKEREF_DAT_big)
SMOKEREF_DAT <- unique(SMOKEREF_DAT)
SMOKEREF_DAT <- left_join(SMOKEREF_DAT, dplyr::select(dat, patid, ACHV, QSSD), by = "patid")
SMOKEREF_DAT <- SMOKEREF_DAT %>% arrange(patid, desc(obsdate)) %>% #latest
  group_by(patid) %>% slice(1) %>% ungroup() 
SMOKEREF_DAT <- SMOKEREF_DAT %>% dplyr::mutate(smokeref_qof = 1) %>% dplyr::select(patid, smokeref_qof)
dat <- left_join(dat, SMOKEREF_DAT, by = "patid")

#SMOKECESS ***MAY NEED TO NOT JUST DO LATEST OCCURRENCE - ASK AA
SMOKECESS_DAT <- rbindlist(SMOKECESS_DAT_big)
SMOKECESS_DAT <- unique(SMOKECESS_DAT)
SMOKECESS_DAT <- left_join(SMOKECESS_DAT, dplyr::select(dat, patid, ACHV, QSSD), by = "patid")
SMOKECESS_DAT <- SMOKECESS_DAT %>% arrange(patid, desc(obsdate)) %>% #latest
  group_by(patid) %>% slice(1) %>% ungroup() 
SMOKECESS_DAT <- SMOKECESS_DAT %>% dplyr::mutate(smokecess_qof = 1) %>% dplyr::select(patid, smokecess_qof)
dat <- left_join(dat, SMOKECESS_DAT, by = "patid")



#drug extraction pipe for codelists using QOF Cluster IDs - adapted AA's from 2_add_in_ASTTRT
QOF_DRUG_EXTRACTION_pipe <- function(drugfile, rds_dir = "X:/QOF_project/Data/tidy_data/RDS/", codes, cluster_id) {
  message(paste0("Cluster ID extracted: ", cluster_id))
  
  obs <- readRDS(paste0(rds_dir, drugfile))
  EXTRACT_COD <- codes %>% filter(Cluster_ID == cluster_id) %>%  
    dplyr::pull(snomedctconceptid) %>% unique()
  
  obs <- obs[!is.na(dmdid)]
  obs <- obs[dmdid %in% EXTRACT_COD]
  obs <- obs[issuedate <= "2023-03-31"] 
  obs <- obs[ , .(patid, issuedate)]
  obs <- unique(obs)
  
  return(obs)
  
}

clusterExport(cl, varlist = c("drug_files", "rds_dir", "QOF_DRUG_EXTRACTION_pipe", "drugscodes"), 
              envir = environment())

#apply pipe to codelist
SMOKECESSDRUGS_DAT_big <- parLapply(cl, drug_files, QOF_DRUG_EXTRACTION_pipe, codes = drugscodes, 
                                    cluster_id = "PHARMDRUG_COD")

#SMOKECESSDRUGS ***MAY NEED TO NOT JUST DO LATEST OCCURRENCE - ASK AA
SMOKECESSDRUGS_DAT <- rbindlist(SMOKECESSDRUGS_DAT_big)
SMOKECESSDRUGS_DAT <- unique(SMOKECESSDRUGS_DAT)
SMOKECESSDRUGS_DAT <- left_join(SMOKECESSDRUGS_DAT, dplyr::select(dat, patid, ACHV, QSSD), by = "patid")
SMOKECESSDRUGS_DAT <- SMOKECESSDRUGS_DAT %>% arrange(patid, desc(issuedate)) %>% #latest
  group_by(patid) %>% slice(1) %>% ungroup() 
SMOKECESSDRUGS_DAT <- SMOKECESSDRUGS_DAT %>% dplyr::mutate(smokecessdrugs_qof = 1) %>% dplyr::select(patid, smokecessdrugs_qof )
dat <- left_join(dat, SMOKECESSDRUGS_DAT, by = "patid")

#remove old date columns as well
colnames(dat)
dat <- dat %>% dplyr::select(-c(PHARMDRUG_COD_earliest:AST_COD_latest_1yr))

# Save the cohort
#dat <- saveRDS(dat, "X:/QOF_project/Data/cohort/4_cohort_with_covars1.RDS")
dat <- saveRDS(dat, "X:/QOF_project/Data/cohort/4_cohort_with_covars_updated1.RDS")

stopCluster(cl)


# Looking to the future:
# add in comorbidities etc up to 31st march 2023 (or within year prior as appropriate)
# add in copd / asthma exacerbations 1st april 2023-31st march 2024
# add in copd / asthma exacerbations 1st april 2024-end of study period (rates, also date of first event)

# potentially include the QIs in the year before 31st march 2023? 
# potentially include the QIs following achievement date



