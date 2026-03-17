# create the clinical data extraction criteria

library(dplyr)
library(data.table)
library(bit64)
library(parallel)
library(lubridate)

dat <- readRDS("X:/QOF_project/Data/Smoking/cohort/cohort_smoking_prelim.RDS")
updated_codes <- readRDS("X:/QOF_project/Creating_codelists/Smoking/Codelists/finalised_new_and_old_comparison.RDS")

updated_codes <- updated_codes %>% select(-Cluster_ID) %>% 
  dplyr::select(snomedctconceptid = conceptId, term, Cluster_ID = updated_Cluster_ID) %>% 
  filter(!is.na(Cluster_ID))

dat <- dat %>% dplyr::mutate(PAT_AGE = as.numeric(ACHV - dob)/365.25)


dat <- dat %>% rename(PAT_DOB = dob,
                      AST_DAT = AST_COD_earliest,
                      # SMOK_DAT = SMOK_COD_latest,
                      ASTLAT_DAT = AST_COD_latest,
                      # ASTRES_DAT needs to be created
                      # ASTTRT_DAT = ASTTRT_COD_latest_1yr, ASTTRT_COD is incorrect
                      ASTINVITE1_DAT = ASTINVITE_COD_earliest_1yr,
                      ASTMONDEC_DAT = ASTMONDEC_COD_latest,
                      ASTPCADEC_DAT = ASTPCADEC_COD_latest,
                      # SMOKSTATDEC_DAT = SMOKSTATDEC_COD_latest,
                      # SMOKEXPOS_DAT = SMOKEXPOS_COD_latest,
                      # NOSMOKEXPOS_DAT = NOSMOKEXPOS_COD_latest,
                      MH_DAT = MH_COD_earliest,
                      CKD_DAT = CKD_COD_latest,
                      COPDLAT_DAT = COPD_COD_latest,
                      COPD_DAT = COPD_COD_earliest,
                      DMLAT_DAT = DM_COD_latest,
                      DM_DAT = DM_COD_earliest,
                      HYPLAT_DAT = HYP_COD_latest,
                      HYP_DAT = HYP_COD_earliest,
                      TIA_DAT = TIA_COD_earliest,
                      STRK_DAT = STRK_COD_earliest,
                      PAD_DAT = PAD_COD_earliest,
                      CHD_DAT = CHD_COD_earliest,
                      REG_DAT = regstartdate, 
                      # SMOKINVITE1_DAT = SMOKINVITE_COD_earliest_1yr,
                      # SMOKPCADEC_DAT = SMOKPCADEC_COD_latest,
                      # SMOKPCAPU_DAT = SMOKPCAPU_COD_latest,
                      PHARMDRUG_DAT = PHARMDRUG_COD_latest,
                      # PHARM_DAT = PHARM_COD_latest,
                      # REFERSSSA_DAT = REFERSSSA_COD_latest,
                      # LSMOK_DAT = LSMOK_COD_latest
                      ) 






dat <- dat %>% mutate(PPED = ACHV,
                      ASTRES_DAT = if_else(ASTRES_COD_latest > ASTLAT_DAT, 
                                           ASTRES_COD_latest, NA),
                      COPDRES_DAT = if_else(COPDRES_COD_latest > COPDLAT_DAT, 
                                            COPDRES_COD_latest, NA),
                      CKD1AND2_DAT = if_else(CKD1AND2_COD_latest > CKD_DAT,  
                                             CKD1AND2_COD_latest, NA), 
                      CKDRES_DAT = if_else(CKDRES_COD_latest > CKD_DAT, 
                                           CKDRES_COD_latest, NA),
                      DMRES_DAT = if_else(DMRES_COD_latest > DMLAT_DAT, 
                                          DMRES_COD_latest, NA),
                      HYPRES_DAT = if_else(HYPRES_COD_latest > HYPLAT_DAT, 
                                           HYPRES_COD_latest, NA),
                      )

# isolating review dates within 1 year takes a bit more time

rds_dir <- "X:/QOF_project/Data/tidy_data/RDS/"

obs_files <- list.files(rds_dir)[grepl("Observation", list.files(rds_dir)) == TRUE] 

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

#Creating SMOK_DAT

SMOK_DAT_pipe <- function(obsfile, rds_dir = "X:/QOF_project/Data/tidy_data/RDS/", updated_codes, cluster_id) {
  message(paste0("Cluster ID extracted: ", cluster_id)) 
  
  obs <- readRDS(paste0(rds_dir, obsfile))
  EXTRACT_COD <- updated_codes %>% filter(Cluster_ID == cluster_id) %>%  
    dplyr::pull(snomedctconceptid) %>% unique()
  
  
  
  obs <- obs[snomedctconceptid %in% EXTRACT_COD]
  obs <- obs[obsdate <= "2024-03-31" & obsdate > "2023-03-31"]
  obs <- obs[ , .(patid, obsdate)]
  obs <- unique(obs)
  
  return(obs)
  
}


clusterExport(cl, varlist = c("obs_files", "rds_dir", "SMOK_DAT_pipe", "updated_codes"), 
              envir = environment())


SMOK_DAT_big <- parLapply(cl, obs_files, SMOK_DAT_pipe, updated_codes = updated_codes, cluster_id = "SMOK_COD") 

SMOK_DAT <- rbindlist(SMOK_DAT_big)
SMOK_DAT <- unique(SMOK_DAT)

# join with subsection of our cohort

SMOK_DAT <- left_join(SMOK_DAT, dplyr::select(dat, patid, ACHV, QSSD), by = "patid")

SMOK_DAT <- SMOK_DAT %>%
  filter(!is.na(obsdate)) %>% 
  filter(obsdate <= ACHV) %>% 
  arrange(patid, obsdate) %>% 
  group_by(patid) %>% 
  slice_max(obsdate, n=1) %>%
  ungroup() %>% 
  rename(SMOK_DAT = obsdate) %>%
  dplyr::select(patid, SMOK_DAT)

# bind it into our cohort

dat <- left_join(dat, SMOK_DAT, by = "patid")

#Creating LSMOK_DAT
LSMOK_DAT_pipe <- function(obsfile, rds_dir = "X:/QOF_project/Data/tidy_data/RDS/", updated_codes, cluster_id) {
  message(paste0("Cluster ID extracted: ", cluster_id)) 
  
  obs <- readRDS(paste0(rds_dir, obsfile))
  EXTRACT_COD <- updated_codes %>% filter(Cluster_ID == cluster_id) %>%  
    dplyr::pull(snomedctconceptid) %>% unique()
  
  
  
  obs <- obs[snomedctconceptid %in% EXTRACT_COD]
  obs <- obs[obsdate <= "2024-03-31" & obsdate > "2023-03-31"]
  obs <- obs[ , .(patid, obsdate)]
  obs <- unique(obs)
  
  return(obs)
  
}


clusterExport(cl, varlist = c("obs_files", "rds_dir", "LSMOK_DAT_pipe", "updated_codes"), 
              envir = environment())


LSMOK_DAT_big <- parLapply(cl, obs_files, LSMOK_DAT_pipe, updated_codes = updated_codes, cluster_id = "LSMOK_COD") 

LSMOK_DAT <- rbindlist(LSMOK_DAT_big)
LSMOK_DAT <- unique(LSMOK_DAT)

# join with subsection of our cohort

LSMOK_DAT <- left_join(LSMOK_DAT, dplyr::select(dat, patid, ACHV, QSSD), by = "patid")

LSMOK_DAT <- LSMOK_DAT %>%
  filter(!is.na(obsdate)) %>% 
  filter(obsdate <= ACHV) %>% 
  arrange(patid, obsdate) %>% 
  group_by(patid) %>% 
  slice_max(obsdate, n=1) %>%
  ungroup() %>% 
  rename(LSMOK_DAT = obsdate) %>%
  dplyr::select(patid, LSMOK_DAT)

# bind it into our cohort

dat <- left_join(dat, LSMOK_DAT, by = "patid")

#Creating PHARM_DAT
PHARM_DAT_pipe <- function(obsfile, rds_dir = "X:/QOF_project/Data/tidy_data/RDS/", updated_codes, cluster_id) {
  message(paste0("Cluster ID extracted: ", cluster_id)) 
  
  obs <- readRDS(paste0(rds_dir, obsfile))
  EXTRACT_COD <- updated_codes %>% filter(Cluster_ID == cluster_id) %>%  
    dplyr::pull(snomedctconceptid) %>% unique()
  
  
  
  obs <- obs[snomedctconceptid %in% EXTRACT_COD]
  obs <- obs[obsdate <= "2024-03-31" & obsdate > "2023-03-31"]
  obs <- obs[ , .(patid, obsdate)]
  obs <- unique(obs)
  
  return(obs)
  
}


clusterExport(cl, varlist = c("obs_files", "rds_dir", "PHARM_DAT_pipe", "updated_codes"), 
              envir = environment())


PHARM_DAT_big <- parLapply(cl, obs_files, PHARM_DAT_pipe, updated_codes = updated_codes, cluster_id = "PHARM_COD") 

PHARM_DAT <- rbindlist(PHARM_DAT_big)
PHARM_DAT <- unique(PHARM_DAT)

# join with subsection of our cohort

PHARM_DAT <- left_join(PHARM_DAT, dplyr::select(dat, patid, ACHV, QSSD), by = "patid")

PHARM_DAT <- PHARM_DAT %>%
  filter(!is.na(obsdate)) %>% 
  filter(obsdate <= ACHV) %>% 
  arrange(patid, obsdate) %>% 
  group_by(patid) %>% 
  slice_max(obsdate, n=1) %>%
  ungroup() %>% 
  rename(PHARM_DAT = obsdate) %>%
  dplyr::select(patid, PHARM_DAT)

# bind it into our cohort

dat <- left_join(dat, PHARM_DAT, by = "patid")




#Creating SMOKINVITE1_DAT
SMOKINVITE1_DAT_pipe <- function(obsfile, rds_dir = "X:/QOF_project/Data/tidy_data/RDS/", updated_codes, cluster_id) {
  message(paste0("Cluster ID extracted: ", cluster_id)) # now an argument in the function to 
  # prevent accidentally extracting the wrong thing
  # although tbh this backfired because when it used to be called 'Cluster_ID' it 
  # messed up the pipe completely - had to change to cluster_id
  
  obs <- readRDS(paste0(rds_dir, obsfile))
  EXTRACT_COD <- updated_codes %>% filter(Cluster_ID == cluster_id) %>%  
    dplyr::pull(snomedctconceptid) %>% unique()
  
  
  
  obs <- obs[snomedctconceptid %in% EXTRACT_COD]
  obs <- obs[obsdate <= "2024-03-31" & obsdate > "2023-03-31"]
  obs <- obs[ , .(patid, obsdate)]
  obs <- unique(obs)
  
  return(obs)
  
}


clusterExport(cl, varlist = c("obs_files", "rds_dir", "SMOKINVITE1_DAT_pipe", "updated_codes"), 
              envir = environment())


SMOKINVITE1_DAT_big <- parLapply(cl, obs_files, SMOKINVITE1_DAT_pipe, updated_codes = updated_codes, cluster_id = "SMOKINVITE_COD") 

SMOKINVITE1_DAT <- rbindlist(SMOKINVITE1_DAT_big)
SMOKINVITE1_DAT <- unique(SMOKINVITE1_DAT)

# join with subsection of our cohort

SMOKINVITE1_DAT <- left_join(SMOKINVITE1_DAT, dplyr::select(dat, patid, ACHV, QSSD), by = "patid")

SMOKINVITE1_DAT <- SMOKINVITE1_DAT %>% 
  filter(!is.na(obsdate)) %>% 
  filter(obsdate >= QSSD) %>% 
  arrange(patid, obsdate) %>% 
  group_by(patid) %>% 
  slice(1) %>%
  ungroup() %>% 
  rename(SMOKINVITE1_DAT = obsdate) %>% 
  dplyr::select(patid, SMOKINVITE1_DAT)

# bind it into our cohort

dat <- left_join(dat, SMOKINVITE1_DAT, by = "patid")

#Creating SMOKINVITE2_DAT

SMOKINVITE2_DAT_pipe <- function(obsfile, rds_dir = "X:/QOF_project/Data/tidy_data/RDS/", updated_codes, cluster_id) {
  message(paste0("Cluster ID extracted: ", cluster_id)) # now an argument in the function to 
  # prevent accidentally extracting the wrong thing
  # although tbh this backfired because when it used to be called 'Cluster_ID' it 
  # messed up the pipe completely - had to change to cluster_id
  
  obs <- readRDS(paste0(rds_dir, obsfile))
  EXTRACT_COD <- updated_codes %>% filter(Cluster_ID == cluster_id) %>%  
    dplyr::pull(snomedctconceptid) %>% unique()
  
  
  
  obs <- obs[snomedctconceptid %in% EXTRACT_COD]
  obs <- obs[obsdate <= "2024-03-31" & obsdate > "2023-03-31"]
  obs <- obs[ , .(patid, obsdate)]
  obs <- unique(obs)
  
  return(obs)
  
}


clusterExport(cl, varlist = c("obs_files", "rds_dir", "SMOKINVITE2_DAT_pipe", "updated_codes"), 
              envir = environment())


SMOKINVITE2_DAT_big <- parLapply(cl, obs_files, SMOKINVITE2_DAT_pipe, updated_codes = updated_codes, cluster_id = "SMOKINVITE_COD") 

SMOKINVITE2_DAT <- rbindlist(SMOKINVITE2_DAT_big)
SMOKINVITE2_DAT <- unique(SMOKINVITE2_DAT)

# join with subsection of our cohort

SMOKINVITE2_DAT <- left_join(SMOKINVITE2_DAT, dplyr::select(dat, patid, SMOKINVITE1_DAT, ACHV, QSSD), by = "patid")

SMOKINVITE2_DAT <- SMOKINVITE2_DAT %>% 
  filter(!is.na(SMOKINVITE1_DAT)) %>% 
  filter(obsdate >= SMOKINVITE1_DAT + 7) %>% 
  arrange(patid, obsdate) %>% 
  group_by(patid) %>% 
  slice(1) %>%
  ungroup() %>% 
  rename(SMOKINVITE2_DAT = obsdate) %>% 
  dplyr::select(patid, SMOKINVITE2_DAT)

# bind it into our cohort

dat <- left_join(dat, SMOKINVITE2_DAT, by = "patid")

#Creating SMOKPCADEC_DAT
SMOKPCADEC_DAT_pipe <- function(obsfile, rds_dir = "X:/QOF_project/Data/tidy_data/RDS/", updated_codes, cluster_id) {
  message(paste0("Cluster ID extracted: ", cluster_id)) 
  
  obs <- readRDS(paste0(rds_dir, obsfile))
  EXTRACT_COD <- updated_codes %>% filter(Cluster_ID == cluster_id) %>%  
    dplyr::pull(snomedctconceptid) %>% unique()
  
  
  
  obs <- obs[snomedctconceptid %in% EXTRACT_COD]
  obs <- obs[obsdate <= "2024-03-31" & obsdate > "2023-03-31"]
  obs <- obs[ , .(patid, obsdate)]
  obs <- unique(obs)
  
  return(obs)
  
}


clusterExport(cl, varlist = c("obs_files", "rds_dir", "SMOKPCADEC_DAT_pipe", "updated_codes"), 
              envir = environment())


SMOKPCADEC_DAT_big <- parLapply(cl, obs_files, SMOKPCADEC_DAT_pipe, updated_codes = updated_codes, cluster_id = "SMOKPCADEC_COD") 

SMOKPCADEC_DAT <- rbindlist(SMOKPCADEC_DAT_big)
SMOKPCADEC_DAT <- unique(SMOKPCADEC_DAT)

# join with subsection of our cohort

SMOKPCADEC_DAT <- left_join(SMOKPCADEC_DAT, dplyr::select(dat, patid, ACHV, QSSD), by = "patid")

SMOKPCADEC_DAT <- SMOKPCADEC_DAT %>%
  filter(!is.na(obsdate)) %>% 
  filter(obsdate <= ACHV) %>% 
  arrange(patid, obsdate) %>% 
  group_by(patid) %>% 
  slice_max(obsdate, n=1) %>%
  ungroup() %>% 
  rename(SMOKPCADEC_DAT = obsdate) %>%
  dplyr::select(patid, SMOKPCADEC_DAT)

# bind it into our cohort

dat <- left_join(dat, SMOKPCADEC_DAT, by = "patid")


#Creating SMOKPCAPU_DAT
SMOKPCAPU_DAT_pipe <- function(obsfile, rds_dir = "X:/QOF_project/Data/tidy_data/RDS/", updated_codes, cluster_id) {
  message(paste0("Cluster ID extracted: ", cluster_id)) 
  
  obs <- readRDS(paste0(rds_dir, obsfile))
  EXTRACT_COD <- updated_codes %>% filter(Cluster_ID == cluster_id) %>%  
    dplyr::pull(snomedctconceptid) %>% unique()
  
  
  
  obs <- obs[snomedctconceptid %in% EXTRACT_COD]
  obs <- obs[obsdate <= "2024-03-31" & obsdate > "2023-03-31"]
  obs <- obs[ , .(patid, obsdate)]
  obs <- unique(obs)
  
  return(obs)
  
}


clusterExport(cl, varlist = c("obs_files", "rds_dir", "SMOKPCAPU_DAT_pipe", "updated_codes"), 
              envir = environment())


SMOKPCAPU_DAT_big <- parLapply(cl, obs_files, SMOKPCAPU_DAT_pipe, updated_codes = updated_codes, cluster_id = "SMOKPCAPU_COD") 

SMOKPCAPU_DAT <- rbindlist(SMOKPCAPU_DAT_big)
SMOKPCAPU_DAT <- unique(SMOKPCAPU_DAT)

# join with subsection of our cohort

SMOKPCAPU_DAT <- left_join(SMOKPCAPU_DAT, dplyr::select(dat, patid, ACHV, QSSD), by = "patid")

SMOKPCAPU_DAT <- SMOKPCAPU_DAT %>%
  filter(!is.na(obsdate)) %>% 
  filter(obsdate <= ACHV) %>% 
  arrange(patid, obsdate) %>% 
  group_by(patid) %>% 
  slice_max(obsdate, n=1) %>%
  ungroup() %>% 
  rename(SMOKPCAPU_DAT = obsdate) %>%
  dplyr::select(patid, SMOKPCAPU_DAT)

# bind it into our cohort

dat <- left_join(dat, SMOKPCAPU_DAT, by = "patid")


#Creating SMOKSTATDEC_DAT
SMOKSTATDEC_DAT_pipe <- function(obsfile, rds_dir = "X:/QOF_project/Data/tidy_data/RDS/", updated_codes, cluster_id) {
  message(paste0("Cluster ID extracted: ", cluster_id)) 
  
  obs <- readRDS(paste0(rds_dir, obsfile))
  EXTRACT_COD <- updated_codes %>% filter(Cluster_ID == cluster_id) %>%  
    dplyr::pull(snomedctconceptid) %>% unique()
  
  
  
  obs <- obs[snomedctconceptid %in% EXTRACT_COD]
  obs <- obs[obsdate <= "2024-03-31" & obsdate > "2023-03-31"]
  obs <- obs[ , .(patid, obsdate)]
  obs <- unique(obs)
  
  return(obs)
  
}


clusterExport(cl, varlist = c("obs_files", "rds_dir", "SMOKSTATDEC_DAT_pipe", "updated_codes"), 
              envir = environment())


SMOKSTATDEC_DAT_big <- parLapply(cl, obs_files, SMOKSTATDEC_DAT_pipe, updated_codes = updated_codes, cluster_id = "SMOKSTATDEC_COD") 

SMOKSTATDEC_DAT <- rbindlist(SMOKSTATDEC_DAT_big)
SMOKSTATDEC_DAT <- unique(SMOKSTATDEC_DAT)

# join with subsection of our cohort

SMOKSTATDEC_DAT <- left_join(SMOKSTATDEC_DAT, dplyr::select(dat, patid, ACHV, QSSD), by = "patid")

SMOKSTATDEC_DAT <- SMOKSTATDEC_DAT %>%
  filter(!is.na(obsdate)) %>% 
  filter(obsdate <= ACHV) %>% 
  arrange(patid, obsdate) %>% 
  group_by(patid) %>% 
  slice_max(obsdate, n=1) %>%
  ungroup() %>% 
  rename(SMOKSTATDEC_DAT = obsdate) %>%
  dplyr::select(patid, SMOKSTATDEC_DAT)

# bind it into our cohort

dat <- left_join(dat, SMOKSTATDEC_DAT, by = "patid")

#Creating REFERSSSA_DAT
REFERSSSA_DAT_pipe <- function(obsfile, rds_dir = "X:/QOF_project/Data/tidy_data/RDS/", updated_codes, cluster_id) {
  message(paste0("Cluster ID extracted: ", cluster_id)) 
  
  obs <- readRDS(paste0(rds_dir, obsfile))
  EXTRACT_COD <- updated_codes %>% filter(Cluster_ID == cluster_id) %>%  
    dplyr::pull(snomedctconceptid) %>% unique()
  
  
  
  obs <- obs[snomedctconceptid %in% EXTRACT_COD]
  obs <- obs[obsdate <= "2024-03-31" & obsdate > "2023-03-31"]
  obs <- obs[ , .(patid, obsdate)]
  obs <- unique(obs)
  
  return(obs)
  
}


clusterExport(cl, varlist = c("obs_files", "rds_dir", "REFERSSSA_DAT_pipe", "updated_codes"), 
              envir = environment())


REFERSSSA_DAT_big <- parLapply(cl, obs_files, REFERSSSA_DAT_pipe, updated_codes = updated_codes, cluster_id = "REFERSSSA_COD") 

REFERSSSA_DAT <- rbindlist(REFERSSSA_DAT_big)
REFERSSSA_DAT <- unique(REFERSSSA_DAT)

# join with subsection of our cohort

REFERSSSA_DAT <- left_join(REFERSSSA_DAT, dplyr::select(dat, patid, ACHV, QSSD), by = "patid")

REFERSSSA_DAT <- REFERSSSA_DAT %>%
  filter(!is.na(obsdate)) %>% 
  filter(obsdate <= ACHV) %>% 
  arrange(patid, obsdate) %>% 
  group_by(patid) %>% 
  slice_max(obsdate, n=1) %>%
  ungroup() %>% 
  rename(REFERSSSA_DAT = obsdate) %>%
  dplyr::select(patid, REFERSSSA_DAT)

# bind it into our cohort

dat <- left_join(dat, REFERSSSA_DAT, by = "patid")

#Creating EXSMOK_DAT

EXSMOK_DAT_pipe <- function(obsfile, rds_dir = "X:/QOF_project/Data/tidy_data/RDS/", updated_codes, cluster_id) {
  message(paste0("Cluster ID extracted: ", cluster_id))
  
  obs <- readRDS(paste0(rds_dir, obsfile))
  EXTRACT_COD <- updated_codes %>% filter(Cluster_ID == cluster_id) %>%  
    dplyr::pull(snomedctconceptid) %>% unique()
  
  
  
  obs <- obs[snomedctconceptid %in% EXTRACT_COD]
  obs <- obs[obsdate <= "2024-03-31"]
  obs <- obs[ , .(patid, obsdate)]
  obs <- unique(obs)
  obs <- obs %>% rename(EXSMOK_DAT = obsdate)
  
  return(obs)
  
}


clusterExport(cl, varlist = c("obs_files", "rds_dir", "EXSMOK_DAT_pipe", "updated_codes"), 
              envir = environment())


EXSMOK_DAT_big <- parLapply(cl, obs_files, EXSMOK_DAT_pipe, updated_codes = updated_codes, cluster_id = "EXSMOK_COD") 

EXSMOK_DAT <- rbindlist(EXSMOK_DAT_big)

# bind EXSMOK_DAT array into our cohort

dat_exsmok <- left_join(dat, EXSMOK_DAT, by = "patid")

# create EXSMOK1_DAT and EXSMOK2_DAT (had to use data.table for this)

DT <- as.data.table(dat_exsmok)

# Precompute the row-wise windows (cheap, fully vectorised)
DT[, `:=`(
  EXSMOK1_lb = EXSMOK_DAT - years(2),
  EXSMOK1_ub = EXSMOK_DAT - years(1),
  EXSMOK2_lb = EXSMOK_DAT - years(3),
  EXSMOK2_ub = EXSMOK_DAT - years(2)  # per your code: EXSMOK2_ub = EXSMOK1_lb
)]

# For each patid, compute the group-wise maxima of the row-wise windows ONCE
DT[, `:=`(
  max_EXSMOK1_lb = max(EXSMOK1_lb),
  max_EXSMOK1_ub = max(EXSMOK1_ub),
  max_EXSMOK2_lb = max(EXSMOK2_lb),
  max_EXSMOK2_ub = max(EXSMOK2_ub),
  max_EXSMOK_DAT = max(EXSMOK_DAT)
), by = patid]

# Now classify rows in one pass
DT[, `:=`(
  EXSMOK1_DAT_calc = fifelse(EXSMOK_DAT >= max_EXSMOK1_lb & EXSMOK_DAT < max_EXSMOK1_ub, EXSMOK_DAT, as.Date(NA)),
  EXSMOK2_DAT_calc = fifelse(EXSMOK_DAT >= max_EXSMOK2_lb & EXSMOK_DAT < max_EXSMOK2_ub, EXSMOK_DAT, as.Date(NA))
)]

# Create EXSMOK1_DAT and EXSMOK2_DAT
DT[
  DT[, .(EXSMOK1_DAT = max(EXSMOK1_DAT_calc, na.rm = TRUE),
         EXSMOK2_DAT = max(EXSMOK2_DAT_calc, na.rm = TRUE)),
     by = patid],
  on = "patid",
  `:=`(
    EXSMOK1_DAT = i.EXSMOK1_DAT,
    EXSMOK2_DAT = i.EXSMOK2_DAT
  )
]

# Create L3YREXSMOK_DAT and E3YREXSMOK_DAT
DT[, `:=`(
  L3YREXSMOK_DAT = fifelse(!is.na(EXSMOK1_DAT) & !is.na(EXSMOK2_DAT), max_EXSMOK_DAT, as.Date(NA)),
  E3YREXSMOK_DAT = fifelse(!is.na(EXSMOK2_DAT), EXSMOK2_DAT, as.Date(NA))
)]

# Create E3YREXSMOK2_DAT
DT[, E3YREXSMOK2_DAT := fifelse(is.na(LSMOK_DAT)| E3YREXSMOK_DAT > LSMOK_DAT, E3YREXSMOK_DAT, as.Date(NA))]

DT <- as.data.frame(DT)
DT <- DT %>%
  select(patid, L3YREXSMOK_DAT, E3YREXSMOK_DAT, E3YREXSMOK2_DAT) %>%
  distinct(patid, .keep_all = TRUE)

#Finally bind cols into cohort
dat <- left_join(dat, DT, by = "patid")


# NSMOK_DAT, CSMOK_DAT, and LEXSMOK_DAT need to be created by matching with SMOK_DAT
# given that we've got the latest of all of them, we can just do it in a similar way
# to the creation of the 'resolved' codes


dat <- dat %>% 
  mutate(NSMOK_DAT = if_else(NSMOK_COD_latest == SMOK_DAT, 
                             NSMOK_COD_latest, NA),
         CSMOK_DAT = if_else(LSMOK_DAT == SMOK_DAT, # (LSMOK_DAT was renamed from LSMOK_COD_latest) 
                             LSMOK_DAT, NA),
         LEXSMOK_DAT = if_else(EXSMOK_COD_latest == SMOK_DAT, 
                               EXSMOK_COD_latest, NA))


# Save the cohort!!!!

saveRDS(dat, "Z:/Group_work/QOF_project/Data/Smoking/cohort/5_cohort_smoking_with_updated_DAT_cols.RDS")

stopCluster(cl)
