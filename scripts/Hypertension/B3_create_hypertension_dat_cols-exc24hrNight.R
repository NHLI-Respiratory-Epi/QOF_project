# create the hypertension columns
# new hyp diagnosis codes using updated codelist
# use updated codelists
# excluding night time and 24-hour ABPM

library(dplyr)
library(tidyr)
library(data.table)
library(bit64)
library(parallel)
'%!in%' <- function(x,y)!('%in%'(x,y))


dat <- readRDS("X:/QOF_project/Data/Hypertension/cohort/cohort_hypertension_prelim.RDS")
codes <- readxl::read_xlsx("codelists/all/HYP_CODES_MASTER.xlsx")

# exclude night time and 24-hour ambulatory
exc <- codes %>%
  filter(grepl("night", SNOMED_code_description, ignore.case=T) |
           grepl("24", SNOMED_code_description, ignore.case=T))

codes<-codes %>%filter(snomedctconceptid %!in% exc$snomedctconceptid)
rm(exc)

#
dat <- dat %>% dplyr::mutate(PAT_AGE = as.numeric(ACHV - dob)/365.25)


dat <- dat %>% rename(
  BPDEC_DAT = BPDEC_COD_latest,
  HTMAX_DAT = HTMAX_COD_latest,
  HYPINVITELAT_DAT = HYPINVITE_COD_latest_1yr,
  HYPPCADEC_DAT = HYPPCADEC_COD_latest,
  HYPPCAPU_DAT = HYPPCAPU_COD_latest,
  HOMEBPDEC_DAT = HOMEBPDEC_COD_latest,
  ABPMDEC_DAT = ABPMDEC_COD_latest)

###### HYP DIAGNOSIS CODES ######
# set up the directories and obs files:

rds_dir <- "X:/QOF_project/Data/tidy_data/RDS/"

obs_files <- list.files(rds_dir)[grepl("Observation", list.files(rds_dir)) == TRUE] 


HYPDIAG_DAT_pipe <- function(obsfile, rds_dir = "X:/QOF_project/Data/tidy_data/RDS/", codes, cluster_id) {
  message(paste0("Cluster ID extracted: ", cluster_id)) # now an argument in the function to 
  # prevent accidentally extracting the wrong thing
  # although tbh this backfired because when it used to be called 'Cluster_ID' it 
  # messed up the pipe completely - had to change to cluster_id
  
  obs <- readRDS(paste0(rds_dir, obsfile))
  EXTRACT_COD <- codes %>% filter(Cluster_ID == cluster_id) %>%  
    dplyr::pull(snomedctconceptid) %>% unique()
  
  
  
  obs <- obs[snomedctconceptid %in% EXTRACT_COD]
  obs <- obs
  obs <- obs[ , .(patid, obsdate)]
  obs <- unique(obs)
  
  return(obs)
  
}


detectCores()
ncores <- 24

# Create a cluster
cl <- makeCluster(ncores)

# we should parallelise this process

clusterEvalQ(cl, { # these need to be read into each cluster
  library(data.table)
  library(dplyr)
  library(tidyr) # needed for pivot_wider
  library(bit64)
})


clusterExport(cl, varlist = c("obs_files", "rds_dir", "HYPDIAG_DAT_pipe", "codes"), 
              envir = environment())

HYPDIAGDAT_big <- parLapply(cl, obs_files, HYPDIAG_DAT_pipe, codes = codes, cluster_id = "HYP_COD") 

HYPDIAG_DAT <- rbindlist(HYPDIAGDAT_big)
HYPDIAG_DAT <- unique(HYPDIAG_DAT)

# join with subsection of our cohort

HYPDIAG_DAT <- left_join(HYPDIAG_DAT, dplyr::select(dat, patid, ACHV, QSSD), by = "patid")

head(HYPDIAG_DAT)

# latest code
HYPDIAG_DAT1 <- HYPDIAG_DAT %>% 
  group_by(patid) %>% 
  arrange(patid, desc(obsdate)) %>% slice(1) %>% select(patid, HYPLAT_DAT = obsdate)

# earliest code
HYPDIAG_DAT2 <- HYPDIAG_DAT %>% 
  group_by(patid) %>% 
  arrange(patid, obsdate) %>% slice(1) %>% select(patid, HYP_DAT = obsdate)

dat <- dat %>%
  left_join(HYPDIAG_DAT2, by = "patid") %>%
  left_join(HYPDIAG_DAT1, by = "patid") %>% 
  mutate(PPED = ACHV,
         HYPRES_DAT = if_else(HYPRES_COD_latest > HYPLAT_DAT, 
                              HYPRES_COD_latest, NA))

###### HYP RES, INVITE, BP CODES ######



# isolating invite dates within 1 year takes a bit more time, as does
# extracting the blood pressure values. The pipes are created here,
# along with some codelist tweaking for diastolic and systolic,
# and then the pipes are run later on in the script.


HYPINVITE1_DAT_pipe <- function(obsfile, rds_dir = "X:/QOF_project/Data/tidy_data/RDS/", codes, cluster_id) {
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





# we need to extract both systolic and diastolic values.

codes %>% filter(Cluster_ID == "CLINBP_COD") %>% select(snomedctconceptid) %>% unique() %>% nrow()
codes %>% filter(Cluster_ID == "CLINBP_COD" | Cluster_ID == "HOMEAMBBP_COD") %>% select(Output_ID, Cluster_ID) %>% unique()
codes %>% filter(Cluster_ID == "CLINBP_COD" | Cluster_ID == "HOMEAMBBP_COD") %>% filter(Output_ID == "HYP008") %>% nrow()

# we need to do this for the blood pressure values

bp <- codes %>% filter(Cluster_ID == "CLINBP_COD" | Cluster_ID == "HOMEAMBBP_COD") %>% 
  filter(Output_ID == "HYP008") %>% # HYP008 and HYP009 are identical so filter by either
  select(Cluster_ID, Cluster_Description, snomedctconceptid, SNOMED_code_description) %>%
  mutate(snomedctconceptid = as.integer64(snomedctconceptid)) %>%
  mutate(
    bp_type = case_when(
      grepl("systolic", SNOMED_code_description, ignore.case = TRUE) ~ "systolic",
      grepl("diastolic", SNOMED_code_description, ignore.case = TRUE) ~ "diastolic",
      TRUE ~ NA_character_
    )
  )


CLINBP_pipe <- function(obsfile, rds_dir = "X:/QOF_project/Data/tidy_data/RDS/", bp = bp) {
  
  obs <- readRDS(paste0(rds_dir, obsfile))
#  obs <- readRDS(paste0(rds_dir, obs_files[1]))   
  bp <- bp %>% filter(Cluster_ID == "CLINBP_COD") %>%
    filter(bp_type %in% c("systolic", "diastolic")) %>% 
    dplyr::select(snomedctconceptid, bp_type) %>% unique()
  bp<-setDT(bp)
  
  
  obs <- bp[obs, on = "snomedctconceptid", nomatch = 0]
  obs <- obs[obsdate <= "2024-03-31"]
  obs <- obs[!is.na(value)]
  obs <- obs[ , .(patid, obsdate, bp_type, value)]
  obs <- unique(obs)
  obs <- obs %>% arrange(patid, obsdate) %>%
    group_by(patid, obsdate, bp_type) %>%
    summarise(value = dplyr::last(value), .groups = "drop") %>%
    pivot_wider(names_from = bp_type, values_from = value) %>%
    mutate(
      systolic  = if (!"systolic"  %in% names(.)) NA_real_ else systolic,
      diastolic = if (!"diastolic" %in% names(.)) NA_real_ else diastolic
    ) %>%
    filter(!is.na(systolic) & !is.na(diastolic)) %>%
    group_by(patid) %>%
    slice_max(order_by = obsdate, n = 1, with_ties = FALSE) %>%
    ungroup()
  obs <- obs %>% rename(CLINBPLAT_DAT = obsdate,
                        CLINBPSYSLAT_VAL = systolic,
                        CLINBPDIALAT_VAL = diastolic)
  
  return(obs)
  
}

HOMEAMBBP_pipe <- function(obsfile, rds_dir = "X:/QOF_project/Data/tidy_data/RDS/", bp = bp) {
  
  obs <- readRDS(paste0(rds_dir, obsfile))
  #  obs <- readRDS(paste0(rds_dir, obs_files[1]))   
  bp <- bp %>% filter(Cluster_ID == "HOMEAMBBP_COD") %>%
    filter(bp_type %in% c("systolic", "diastolic")) %>% 
    dplyr::select(snomedctconceptid, bp_type) %>% unique()
  
  bp<-setDT(bp)
  
  obs <- bp[obs, on = "snomedctconceptid", nomatch = 0]
  obs <- obs[obsdate <= "2024-03-31"]
  obs <- obs[!is.na(value)]
  obs <- obs[ , .(patid, obsdate, bp_type, value)]
  obs <- unique(obs)
  obs <- obs %>% dplyr::arrange(patid, obsdate) %>%
    dplyr::group_by(patid, obsdate, bp_type) %>%
    dplyr::summarise(value = dplyr::last(value), .groups = "drop") %>%
    pivot_wider(names_from = bp_type, values_from = value) %>%
    mutate(
      systolic  = if (!"systolic"  %in% names(.)) NA_real_ else systolic,
      diastolic = if (!"diastolic" %in% names(.)) NA_real_ else diastolic
    ) %>%
    dplyr::filter(!is.na(systolic) & !is.na(diastolic)) %>%
    group_by(patid) %>%
    slice_max(order_by = obsdate, n = 1, with_ties = FALSE) %>%
    ungroup()
  obs <- obs %>% rename(HOMEAMBBPLAT_DAT = obsdate,
                        HOMEAMBBPSYSLAT_VAL = systolic,
                        HOMEAMBBPDIALAT_VAL = diastolic)
  
  return(obs)
  
}


detectCores()
ncores <- 24

# Create a cluster
cl <- makeCluster(ncores)

# we should parallelise this process

clusterEvalQ(cl, { # these need to be read into each cluster
  library(data.table)
  library(dplyr)
  library(tidyr) # needed for pivot_wider
  library(bit64)
})


clusterExport(cl, varlist = c("obs_files", "rds_dir", "HYPINVITE1_DAT_pipe", "codes", "bp",
                              "CLINBP_pipe", "HOMEAMBBP_pipe"), 
              envir = environment())


HYPINVITE1_DAT_big <- parLapply(cl, obs_files, HYPINVITE1_DAT_pipe, codes = codes, cluster_id = "HYPINVITE_COD") 

HYPINVITE1_DAT <- rbindlist(HYPINVITE1_DAT_big)
HYPINVITE1_DAT <- unique(HYPINVITE1_DAT)

# join with subsection of our cohort

HYPINVITE1_DAT <- left_join(HYPINVITE1_DAT, dplyr::select(dat, patid, HYPINVITELAT_DAT, ACHV, QSSD), by = "patid")

head(HYPINVITE1_DAT)

HYPINVITE1_DAT <- HYPINVITE1_DAT %>% filter(obsdate <= (HYPINVITELAT_DAT - 7)) %>%
  group_by(patid) %>% 
  arrange(patid, desc(obsdate)) %>% slice(1) %>% select(patid, HYPINVITE1_DAT = obsdate)



dat <- left_join(dat, HYPINVITE1_DAT, by = "patid")

dat <- dat %>% mutate(HYPINVITE2_DAT = if_else(!is.na(HYPINVITE1_DAT), HYPINVITELAT_DAT, NA))

# clinical bp pipe

CLINBP_big <- parLapply(cl, obs_files, CLINBP_pipe, bp = bp) 
CLINBP <- rbindlist(CLINBP_big, use.names = TRUE)
CLINBP <- unique(CLINBP)

head(CLINBP)

dat <- left_join(dat, CLINBP, by = "patid")

# home / amb bp pipe

HOMEAMBBP_big <- parLapply(cl, obs_files, HOMEAMBBP_pipe, bp = bp) 
HOMEAMBBP <- rbindlist(HOMEAMBBP_big, use.names = TRUE)
HOMEAMBBP <- unique(HOMEAMBBP)

head(HOMEAMBBP)

dat <- left_join(dat, HOMEAMBBP, by = "patid")

# and lastly we create the final variable.

dat <- dat %>% mutate(CLHMAMBBPLAT_DAT = pmax(CLINBPLAT_DAT, HOMEAMBBPLAT_DAT, na.rm = TRUE))

# then we save our cohort

saveRDS(dat, "Z:/Group_work/QOF_project/Data/Hypertension/cohort/B3_cohort_hypertension_with_DAT_cols.RDS")

stopCluster(cl)
