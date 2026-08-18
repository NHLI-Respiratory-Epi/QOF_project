# create the clinical data extraction criteria

library(dplyr)
library(data.table)
library(bit64)
library(parallel)

dat <- readRDS("X:/QOF_project/Data/COPD/cohort/cohort_COPD_prelim.RDS")
codes <- readRDS("X:/QOF_project/Hannah_Leah/codelists/all/ast_copd_hyp_smok.RDS")

#COMBO3: Validated codelist, validated process codes
COPD_COD_validated <- fread("X:/QOF_project/Hannah_Leah/codelists/all/COPD_codelist_AA/COPD_COD_validated_LD.csv")
COPD_process_validated <- fread("X:/QOF_project/Hannah_Leah/codelists/all/COPD_codelist_AA/COPD_process_validated_LD.csv")

COPD_combined <- rbind(COPD_COD_validated, COPD_process_validated)

#drop COPD definition-related columns that AA made previously for the original QOF COPD codelist
dat <- subset(dat, select = -c(COPD_COD_earliest, COPD_COD_latest, COPD_COD_earliest_1yr, COPD_COD_latest_1yr))
#didn't end up using COPD_COD_earliest_1yr & COPD_COD_latest_1yr so only need to remake COPD_COD_earliest & COPD_COD_latest



# Needed for rest of script
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

# these R objects are necessary in each cluster
clusterExport(cl, varlist = c("obs_files", "rds_dir", "codes", "COPD_combined"), 
              envir = environment())

#extraction pipe for COPD_DAT & COPDLAT_DAT
COPDDX_EXTRACTION_pipe <- function(obsfile, rds_dir = "X:/QOF_project/Data/tidy_data/RDS/", codes) {
  obs <- readRDS(paste0(rds_dir, obsfile))
  EXTRACT_COD <- codes %>% dplyr::pull(snomedctconceptid) %>% unique()
  
  obs <- obs[snomedctconceptid %in% EXTRACT_COD]
  #   obs <- obs[obsdate <= "2024-03-31" & obsdate > "2023-03-31"]
  obs <- obs[obsdate <= "2024-03-31"] # need all this time
  obs <- obs[ , .(patid, obsdate, snomedctconceptid)] #keeping snomedctconceptid here so can merge on include column
  obs <- unique(obs)
  
  return(obs)
  
}


clusterExport(cl, varlist = c("obs_files", "rds_dir", "COPDDX_EXTRACTION_pipe", "COPD_combined"), 
              envir = environment())


COPD_DAT_big <- parLapply(cl, obs_files, COPDDX_EXTRACTION_pipe, codes = COPD_combined)
COPDLAT_DAT_big <- parLapply(cl, obs_files, COPDDX_EXTRACTION_pipe, codes = COPD_combined)

#COPD_DAT
COPD_DAT <- rbindlist(COPD_DAT_big)

COPD_DAT <- left_join(COPD_DAT, dplyr::select(COPD_combined, snomedctconceptid, include), by = "snomedctconceptid") #add include column

COPD_DAT <- left_join(COPD_DAT, dplyr::select(dat, patid, ACHV, QSSD), by = "patid")

COPD_DAT <- COPD_DAT %>% filter(include == 1 | (obsdate <= "2023-03-31" & obsdate > "2021-03-31" & include == 2))  %>% 
  arrange(patid, obsdate) %>% #earliest
  group_by(patid) %>% slice(1) %>% ungroup() %>% rename(COPD_DAT = obsdate) %>% 
  dplyr::select(patid, COPD_DAT)

# bind it into our cohort
dat <- left_join(dat, COPD_DAT, by = "patid")

#COPDLAT_DAT
COPDLAT_DAT <- rbindlist(COPDLAT_DAT_big)

COPDLAT_DAT <- left_join(COPDLAT_DAT, dplyr::select(COPD_combined, snomedctconceptid, include), by = "snomedctconceptid") #add include column

COPDLAT_DAT <- left_join(COPDLAT_DAT, dplyr::select(dat, patid, ACHV, QSSD), by = "patid")

COPDLAT_DAT <- COPDLAT_DAT %>% filter(include == 1 | (obsdate <= "2023-03-31" & obsdate > "2021-03-31" & include == 2))  %>%
  arrange(patid, desc(obsdate)) %>% #latest
  group_by(patid) %>% slice(1) %>% ungroup() %>% rename(COPDLAT_DAT = obsdate) %>% 
  dplyr::select(patid, COPDLAT_DAT)

# bind it into our cohort
dat <- left_join(dat, COPDLAT_DAT, by = "patid")



##start of 1_create_dat_cols script##

dat <- dat %>% dplyr::mutate(PAT_AGE = as.numeric(ACHV - dob)/365.25)

dat <- dat %>% rename(REG_DAT = regstartdate,
                      DEREG_DAT = regenddate,
                      #COPD_DAT = COPD_COD_earliest, #already done in this script
                      #COPDLAT_DAT = COPD_COD_latest, #already done in this script
                      #COPDRES_DAT needs to be created
                      MRC_DAT = MRC_COD_latest,
                      MRC1_DAT = MRC1_COD_earliest_1yr,
                      COPDRVW_DAT = COPDRVW_COD_latest,
                      COPDPCADEC_DAT = COPDPCADEC_COD_latest,
                      COPDPCAPU_DAT = COPDPCAPU_COD_latest,
                      COPDPCASU_DAT = COPDPCASU_COD_latest,
                      COPDINVITE1_DAT = COPDINVITE_COD_earliest_1yr, # double check with AA
                      #COPDINVITE2_DAT needs to be created
                      #PULRHBATT_DAT needs to be created
                      #PULRHBREF_DAT needs to be created
                      PULRHBDEC_DAT = PULRHBDEC_COD_latest,
                      PULRHBPU_DAT = PULRHBPU_COD_latest,
                      PULRHBSU_DAT = PULRHBSU_COD_latest,
                      #COPDRES1_DAT needs to be created
                      #COPD1_DAT needs to be created
                      #EUNRESCOPD_DAT needs to be created
                      COPDEXACB_DAT = COPDEXACB_COD_latest)
                      #FEV1FVCDIAG_DAT needs to be created **requires value
                      #FEV1FVCL70DIAG_DAT needs to be created
                      #FEV1FVCREG_DAT needs to be created **requires value
                      #FEV1FVCL70REG_DAT needs to be created

# Create COPDRES_DAT
# We have already have COPDLAT_DAT. if COPDLAT_DAT is later than COPDRES_COD_latest, then missing, otherwise, COPDRES_DAT.
dat <- dat %>% mutate(COPDRES_DAT = if_else(COPDRES_COD_latest > COPDLAT_DAT, 
                                            COPDRES_COD_latest, NA))

# check
#dat %>% filter(!is.na(COPDRES_DAT)) %>%  select(COPDRES_COD_latest, COPDLAT_DAT, COPDRES_DAT) %>% print(100)






# now we create COPDINVITE2_DAT, which requires COPDINVITE1_DAT.
# this means we need to join our cohort back to the obsfiles we are reading in. 
# I think the quickest way to do this will just be to extract all COPD invite codes to the environment and go from there.

COPDINVITE2_DAT_pipe <- function(obsfile, rds_dir = "X:/QOF_project/Data/tidy_data/RDS/", codes, cluster_id) {
  message(paste0("Cluster ID extracted: ", cluster_id)) # now an argument in the function to 
  # prevent accidentally extracting the wrong thing
  # although tbh this backfired because when it used to be called 'Cluster_ID' it 
  # messed up the pipe completely - had to change to cluster_id
  
  obs <- readRDS(paste0(rds_dir, obsfile))
  EXTRACT_COD <- codes %>% filter(Cluster_ID == cluster_id) %>%  
    dplyr::pull(snomedctconceptid) %>% unique()
  
  
  
  obs <- obs[snomedctconceptid %in% EXTRACT_COD]
  obs <- obs[obsdate <= "2024-03-31" & obsdate > "2023-03-31"]
  obs <- obs[ , .(patid, obsdate)]
  obs <- unique(obs)
  
  return(obs)
  
}


clusterExport(cl, varlist = c("obs_files", "rds_dir", "COPDINVITE2_DAT_pipe", "codes"), 
              envir = environment())


COPDINVITE2_DAT_big <- parLapply(cl, obs_files, COPDINVITE2_DAT_pipe, codes = codes, cluster_id = "COPDINVITE_COD") 

COPDINVITE2_DAT <- rbindlist(COPDINVITE2_DAT_big)
COPDINVITE2_DAT <- unique(COPDINVITE2_DAT)

# join with subsection of our cohort
COPDINVITE2_DAT <- left_join(COPDINVITE2_DAT, dplyr::select(dat, patid, COPDINVITE1_DAT, ACHV, QSSD), by = "patid")

COPDINVITE2_DAT <- COPDINVITE2_DAT %>% filter(!is.na(COPDINVITE1_DAT)) %>% 
  filter(obsdate >= COPDINVITE1_DAT + 7) %>% arrange(patid, obsdate) %>% 
  group_by(patid) %>% slice(1) %>% ungroup() %>% rename(COPDINVITE2_DAT = obsdate) %>% 
  dplyr::select(patid, COPDINVITE2_DAT)

# bind it into our cohort
dat <- left_join(dat, COPDINVITE2_DAT, by = "patid")


# make COPDRES1_DAT
dat <- dat %>% mutate(COPDRES1_DAT = if_else(COPDRES_COD_latest > COPD_DAT, 
                                                  COPDRES_COD_latest, NA))


# COPD1_DAT requires use of a pipe again - updated in this script for the new COPD codelist
COPD1_DAT_pipe <- function(obsfile, rds_dir = "X:/QOF_project/Data/tidy_data/RDS/", codes) {
  
  obs <- readRDS(paste0(rds_dir, obsfile))
  EXTRACT_COD <- codes %>% dplyr::pull(snomedctconceptid) %>% unique()
  
  
  
  obs <- obs[snomedctconceptid %in% EXTRACT_COD]
  #   obs <- obs[obsdate <= "2024-03-31" & obsdate > "2023-03-31"]
  obs <- obs[obsdate <= "2024-03-31"] # need all this time
  obs <- obs[ , .(patid, obsdate, snomedctconceptid)] #keeping snomedctconceptid here so can merge on include column
  obs <- unique(obs)
  
  return(obs)
  
}



clusterExport(cl, varlist = c("obs_files", "rds_dir", "COPD1_DAT_pipe", "codes", "COPD_combined"), 
              envir = environment())

COPD1_DAT_big <- parLapply(cl, obs_files, COPD1_DAT_pipe, codes = COPD_combined)

COPD1_DAT <- rbindlist(COPD1_DAT_big)

COPD1_DAT <- left_join(COPD1_DAT, dplyr::select(COPD_combined, snomedctconceptid, include), by = "snomedctconceptid") #add include column

COPD1_DAT <- left_join(COPD1_DAT, dplyr::select(dat, patid, COPDRES1_DAT, ACHV, QSSD), by = "patid")

COPD1_DAT <- COPD1_DAT %>% filter(include == 1 | (obsdate <= "2023-03-31" & obsdate > "2021-03-31" & include == 2)) %>% 
  filter(!is.na(COPDRES1_DAT)) %>% 
  filter(obsdate > COPDRES1_DAT) %>% arrange(patid, obsdate) %>% #earliest after COPDRES1
  group_by(patid) %>% slice(1) %>% ungroup() %>% rename(COPD1_DAT = obsdate) %>% 
  dplyr::select(patid, COPD1_DAT)

# bind it into our cohort
dat <- left_join(dat, COPD1_DAT, by = "patid")


#make EUNRESCOPD_DAT
dat <- dat %>% mutate(EUNRESCOPD_DAT = if_else(is.na(COPDRES_DAT) & is.na(COPDRES1_DAT), 
                                              COPD_DAT, COPD1_DAT))

# checking - all looks good to me. 
# dat %>% filter(!is.na(COPD_DAT)) %>%  select(COPD_DAT, COPDLAT_DAT, COPDRES_DAT, COPDRES1_DAT, COPD1_DAT,
#                                             EUNRESCOPD_DAT, COPDRES_COD_latest) %>% print(100) 
# 
# dat %>% filter(!is.na(COPD1_DAT)) %>%  select(COPD_DAT, COPDLAT_DAT, COPDRES_DAT, COPDRES1_DAT, COPD1_DAT,
#                                             EUNRESCOPD_DAT, COPDRES_COD_latest) %>% print(100) 
# 
# dat %>% filter(!is.na(COPD_DAT)) %>%  filter(is.na(EUNRESCOPD_DAT)) %>% select(COPD_DAT, COPDLAT_DAT, COPDRES_DAT, COPDRES1_DAT, COPD1_DAT,
#                                             EUNRESCOPD_DAT, COPDRES_COD_latest) %>% print(100) 



# now that we have the unresolved COPD, we can add in FEV1FVCDIAG_DAT and FEV1FVCL70DIAG_DAT
# additionally can add in FEV1FVCREG_DAT and FEV1FVCL70REG_DAT that are set up similarly
GENERIC_EXTRACTION_pipe <- function(obsfile, rds_dir = "X:/QOF_project/Data/tidy_data/RDS/", codes, cluster_id) {
  message(paste0("Cluster ID extracted: ", cluster_id)) # now an argument in the function to 
  # prevent accidentally extracting the wrong thing
  # although tbh this backfired because when it used to be called 'Cluster_ID' it 
  # messed up the pipe completely - had to change to cluster_id
  
  obs <- readRDS(paste0(rds_dir, obsfile))
  EXTRACT_COD <- codes %>% filter(Cluster_ID == cluster_id) %>%  
    dplyr::pull(snomedctconceptid) %>% unique()
  
  
  
  obs <- obs[snomedctconceptid %in% EXTRACT_COD]
  #   obs <- obs[obsdate <= "2024-03-31" & obsdate > "2023-03-31"]
  obs <- obs[obsdate <= "2024-03-31"] # need all this time
  obs <- obs[ , .(patid, obsdate)]
  obs <- unique(obs)
  
  return(obs)
  
}


clusterExport(cl, varlist = c("obs_files", "rds_dir", "GENERIC_EXTRACTION_pipe", "codes"), 
              envir = environment())

#first two FEV1/FVCs that don't require a value
FEV1FVCL70DIAG_DAT_big <- parLapply(cl, obs_files, GENERIC_EXTRACTION_pipe, codes = codes, cluster_id = "FEV1FVCL70_COD")
FEV1FVCL70REG_DAT_big <- parLapply(cl, obs_files, GENERIC_EXTRACTION_pipe, codes = codes, cluster_id = "FEV1FVCL70_COD")

#also going to add these in
PULRHBATT_DAT_big <- parLapply(cl, obs_files, GENERIC_EXTRACTION_pipe, codes = codes, cluster_id = "PULRHBATT_COD")
PULRHBREF_DAT_big <- parLapply(cl, obs_files, GENERIC_EXTRACTION_pipe, codes = codes, cluster_id = "PULRHBREF_COD")


#make FEV1FVCL70DIAG_DAT
FEV1FVCL70DIAG_DAT <- rbindlist(FEV1FVCL70DIAG_DAT_big)

FEV1FVCL70DIAG_DAT <- left_join(FEV1FVCL70DIAG_DAT, dplyr::select(dat, patid, EUNRESCOPD_DAT, ACHV, QSSD), by = "patid")

FEV1FVCL70DIAG_DAT <- FEV1FVCL70DIAG_DAT %>% filter(!is.na(EUNRESCOPD_DAT)) %>% 
  filter(obsdate >= (EUNRESCOPD_DAT - 93)) %>% arrange(patid, obsdate) %>% #earliest on/after EUNRESCOPD_DAT minus 93 days
  group_by(patid) %>% slice(1) %>% ungroup() %>% rename(FEV1FVCL70DIAG_DAT = obsdate) %>% 
  dplyr::select(patid, FEV1FVCL70DIAG_DAT)

dat <- left_join(dat, FEV1FVCL70DIAG_DAT, by = "patid")

#make FEV1FVCL70REG_DAT
FEV1FVCL70REG_DAT <- rbindlist(FEV1FVCL70REG_DAT_big)

FEV1FVCL70REG_DAT <- left_join(FEV1FVCL70REG_DAT, dplyr::select(dat, patid, REG_DAT, ACHV, QSSD), by = "patid")

FEV1FVCL70REG_DAT <- FEV1FVCL70REG_DAT %>% filter(!is.na(REG_DAT)) %>% 
  filter(obsdate >= (REG_DAT - 93)) %>% arrange(patid, obsdate) %>% #earliest on/after REG_DAT minus 93 days
  group_by(patid) %>% slice(1) %>% ungroup() %>% rename(FEV1FVCL70REG_DAT = obsdate) %>% 
  dplyr::select(patid, FEV1FVCL70REG_DAT)

dat <- left_join(dat, FEV1FVCL70REG_DAT, by = "patid")


#make PULRHBATT_DAT
PULRHBATT_DAT <- rbindlist(PULRHBATT_DAT_big)

PULRHBATT_DAT <- left_join(PULRHBATT_DAT, dplyr::select(dat, patid, COPD_DAT, MRC1_DAT, ACHV, QSSD), by = "patid")

PULRHBATT_DAT <- PULRHBATT_DAT %>% filter(!is.na(MRC1_DAT)) %>% 
  filter(obsdate > COPD_DAT & obsdate <= MRC1_DAT) %>% arrange(patid, obsdate) %>% #earliest after COPD_DAT AND before/on MRC1_DAT
  group_by(patid) %>% slice(1) %>% ungroup() %>% rename(PULRHBATT_DAT = obsdate) %>% 
  dplyr::select(patid, PULRHBATT_DAT)

dat <- left_join(dat, PULRHBATT_DAT, by = "patid")

#make PULRHBREF_DAT
PULRHBREF_DAT <- rbindlist(PULRHBREF_DAT_big)

PULRHBREF_DAT <- left_join(PULRHBREF_DAT, dplyr::select(dat, patid, MRC1_DAT, ACHV, QSSD), by = "patid")

PULRHBREF_DAT <- PULRHBREF_DAT %>% filter(!is.na(MRC1_DAT)) %>% 
  filter(obsdate >= MRC1_DAT) %>% arrange(patid, obsdate) %>% #earliest after/on MRC1_DAT
  group_by(patid) %>% slice(1) %>% ungroup() %>% rename(PULRHBREF_DAT = obsdate) %>% 
  dplyr::select(patid, PULRHBREF_DAT)

dat <- left_join(dat, PULRHBREF_DAT, by = "patid")




#and then two of the FEV1/FVC ones require a value
VALUE_EXTRACTION_pipe <- function(obsfile, rds_dir = "X:/QOF_project/Data/tidy_data/RDS/", codes, cluster_id) {
  message(paste0("Cluster ID extracted: ", cluster_id)) # now an argument in the function to 
  # prevent accidentally extracting the wrong thing
  # although tbh this backfired because when it used to be called 'Cluster_ID' it 
  # messed up the pipe completely - had to change to cluster_id
  
  obs <- readRDS(paste0(rds_dir, obsfile))
  EXTRACT_COD <- codes %>% filter(Cluster_ID == cluster_id) %>%  
    dplyr::pull(snomedctconceptid) %>% unique()
  
  
  
  obs <- obs[snomedctconceptid %in% EXTRACT_COD]
  #   obs <- obs[obsdate <= "2024-03-31" & obsdate > "2023-03-31"]
  obs <- obs[obsdate <= "2024-03-31"] # need all this time
  obs <- obs[ , .(patid, obsdate, value)] # need value as well for these two
  obs <- unique(obs)
  
  return(obs)
  
}

clusterExport(cl, varlist = c("obs_files", "rds_dir", "VALUE_EXTRACTION_pipe", "codes"), 
              envir = environment())

#noticed the FEV1FVC values are in both % and proportions so going to do one without converting value and one with
FEV1FVCDIAG_DAT_big <- parLapply(cl, obs_files, VALUE_EXTRACTION_pipe, codes = codes, cluster_id = "FEV1FVC_COD")
CONV_FEV1FVCDIAG_DAT_big <- parLapply(cl, obs_files, VALUE_EXTRACTION_pipe, codes = codes, cluster_id = "FEV1FVC_COD")
FEV1FVCREG_DAT_big <- parLapply(cl, obs_files, VALUE_EXTRACTION_pipe, codes = codes, cluster_id = "FEV1FVC_COD")
CONV_FEV1FVCREG_DAT_big <- parLapply(cl, obs_files, VALUE_EXTRACTION_pipe, codes = codes, cluster_id = "FEV1FVC_COD")

#make FEV1FVCDIAG_DAT
FEV1FVCDIAG_DAT <- rbindlist(FEV1FVCDIAG_DAT_big)

# check values at this point to see how many are % vs proportions
summary(FEV1FVCDIAG_DAT$value)
hist(FEV1FVCDIAG_DAT$value, xlim=c(0,150), breaks=seq(0,10000,5))

FEV1FVCDIAG_DAT <- left_join(FEV1FVCDIAG_DAT, dplyr::select(dat, patid, EUNRESCOPD_DAT, ACHV, QSSD), by = "patid")

FEV1FVCDIAG_DAT <- FEV1FVCDIAG_DAT %>% filter(!is.na(EUNRESCOPD_DAT)) %>% 
  filter(obsdate >= (EUNRESCOPD_DAT - 93) & value < 0.7) %>% arrange(patid, obsdate) %>% #earliest on/after EUNRESCOPD_DAT minus 93 days
  group_by(patid) %>% slice(1) %>% ungroup() %>% rename(FEV1FVCDIAG_DAT = obsdate) %>% 
  dplyr::select(patid, FEV1FVCDIAG_DAT, value)

summary(FEV1FVCDIAG_DAT$value)

dat <- left_join(dat, FEV1FVCDIAG_DAT, by = "patid")

#make CONV_FEV1FVCDIAG_DAT
CONV_FEV1FVCDIAG_DAT <- rbindlist(CONV_FEV1FVCDIAG_DAT_big)

#going to do one with a conversion too
CONV_FEV1FVCDIAG_DAT <- CONV_FEV1FVCDIAG_DAT %>% mutate(CONV_value = if_else(value < 1.2, 
                                                                   value*100, value))
summary(CONV_FEV1FVCDIAG_DAT$CONV_value)

CONV_FEV1FVCDIAG_DAT <- left_join(CONV_FEV1FVCDIAG_DAT, dplyr::select(dat, patid, EUNRESCOPD_DAT, ACHV, QSSD), by = "patid")

CONV_FEV1FVCDIAG_DAT <- CONV_FEV1FVCDIAG_DAT %>% filter(!is.na(EUNRESCOPD_DAT)) %>% 
  filter(obsdate >= (EUNRESCOPD_DAT - 93) & CONV_value < 70) %>% arrange(patid, obsdate) %>% #earliest on/after EUNRESCOPD_DAT minus 93 days
  group_by(patid) %>% slice(1) %>% ungroup() %>% rename(CONV_FEV1FVCDIAG_DAT = obsdate) %>% 
  dplyr::select(patid, CONV_FEV1FVCDIAG_DAT, CONV_value)

summary(CONV_FEV1FVCDIAG_DAT$CONV_value)

dat <- left_join(dat, CONV_FEV1FVCDIAG_DAT, by = "patid")

#make FEV1FVCREG_DAT
FEV1FVCREG_DAT <- rbindlist(FEV1FVCREG_DAT_big)

FEV1FVCREG_DAT <- left_join(FEV1FVCREG_DAT, dplyr::select(dat, patid, REG_DAT, ACHV, QSSD), by = "patid")

FEV1FVCREG_DAT <- FEV1FVCREG_DAT %>% filter(!is.na(REG_DAT)) %>% 
  filter(obsdate >= (REG_DAT - 93) & value < 0.7) %>% arrange(patid, obsdate) %>% #earliest on/after REG_DAT minus 93 days
  group_by(patid) %>% slice(1) %>% ungroup() %>% rename(FEV1FVCREG_DAT = obsdate) %>% 
  dplyr::select(patid, FEV1FVCREG_DAT, value)

summary(FEV1FVCREG_DAT$value)

dat <- left_join(dat, FEV1FVCREG_DAT, by = "patid")

#make CONV_FEV1FVCREG_DAT
CONV_FEV1FVCREG_DAT <- rbindlist(CONV_FEV1FVCREG_DAT_big)

#going to do one with a conversion too
CONV_FEV1FVCREG_DAT <- CONV_FEV1FVCREG_DAT %>% mutate(CONV_value = if_else(value < 1.2, 
                                                                             value*100, value))
summary(CONV_FEV1FVCREG_DAT$CONV_value)

CONV_FEV1FVCREG_DAT <- left_join(CONV_FEV1FVCREG_DAT, dplyr::select(dat, patid, EUNRESCOPD_DAT, ACHV, QSSD), by = "patid")

CONV_FEV1FVCREG_DAT <- CONV_FEV1FVCREG_DAT %>% filter(!is.na(EUNRESCOPD_DAT)) %>% 
  filter(obsdate >= (EUNRESCOPD_DAT - 93) & CONV_value < 70) %>% arrange(patid, obsdate) %>% #earliest on/after EUNRESCOPD_DAT minus 93 days
  group_by(patid) %>% slice(1) %>% ungroup() %>% rename(CONV_FEV1FVCREG_DAT = obsdate) %>% 
  dplyr::select(patid, CONV_FEV1FVCREG_DAT, CONV_value)

summary(CONV_FEV1FVCREG_DAT$CONV_value)

dat <- left_join(dat, CONV_FEV1FVCREG_DAT, by = "patid")



# at this point... we are finished!!!!!!!!!!!

# Save the cohort!!!!

dat <- saveRDS(dat, "X:/QOF_project/Data/COPD/cohort/1_cohort_COPD_with_DAT_cols_COMBO3.RDS")

stopCluster(cl)



# covert rds to dta
library(readstata13)

datcols <- readRDS("X:/QOF_project/Data/COPD/cohort/1_cohort_COPD_with_DAT_cols_COMBO3.RDS")

datcols$patid               <- as.character(datcols$patid)

save.dta13(datcols, file="X:/QOF_project/Data/COPD/cohort/1_cohort_COPD_with_DAT_cols_COMBO3.dta", convert.dates = TRUE)
