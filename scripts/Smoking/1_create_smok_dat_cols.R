# create the clinical data extraction criteria

library(dplyr)
library(data.table)
library(bit64)
library(parallel)

dat <- readRDS("X:/QOF_project/Data/Smoking/cohort/cohort_smoking_prelim.RDS")
codes <- readRDS("codelists/all/ast_copd_hyp_smok.RDS")
#functions <- source("X:/QOF_project/Gloria/QOF_project/scripts/Smoking/smokmake_gi.R")

dat <- dat %>% dplyr::mutate(PAT_AGE = as.numeric(ACHV - dob)/365.25)


dat <- dat %>% rename(AST_DAT = AST_COD_earliest,
                      SMOK_DAT = SMOK_COD_latest,
                      ASTLAT_DAT = AST_COD_latest,
                      # ASTRES_DAT needs to be created
                      # ASTTRT_DAT = ASTTRT_COD_latest_1yr, ASTTRT_COD is incorrect
                      ASTINVITE1_DAT = ASTINVITE_COD_earliest_1yr,
                      ASTMONDEC_DAT = ASTMONDEC_COD_latest,
                      ASTPCADEC_DAT = ASTPCADEC_COD_latest,
                      SMOKSTATDEC_DAT = SMOKSTATDEC_COD_latest,
                      SMOKEXPOS_DAT = SMOKEXPOS_COD_latest,
                      NOSMOKEXPOS_DAT = NOSMOKEXPOS_COD_latest,
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
                      SMOKINVITE1_DAT = SMOKINVITE_COD_earliest_1yr,
                      SMOKPCADEC_DAT = SMOKPCADEC_COD_latest,
                      SMOKPCAPU_DAT = SMOKPCAPU_COD_latest,
                      PHARMDRUG_DAT = PHARMDRUG_COD_latest,
                      PHARM_DAT = PHARM_COD_latest,
                      REFERSSSA_DAT = REFERSSSA_COD_latest,
                      LSMOK_DAT = LSMOK_COD_latest) 

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



# NOTE: ex-smoker variables 41-46 not created but not needed for our current QIs of interest

dat <- dat %>% mutate(PPED = ACHV,
                      ASTRES_DAT = if_else(ASTRES_COD_latest > ASTLAT_DAT, 
                                           ASTRES_COD_latest, NA),
                      COPDRES_DAT = if_else(COPDRES_COD_latest > COPDLAT_DAT, 
                                            COPDRES_COD_latest, NA),
                      CKD1AND2_DAT = if_else(CKD1AND2_COD_latest > CKD_DAT, # needs to be made here 
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

#Creating SMOKINVITE2_DAT

SMOKINVITE2_DAT_pipe <- function(obsfile, rds_dir = "X:/QOF_project/Data/tidy_data/RDS/", codes, cluster_id) {
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


clusterExport(cl, varlist = c("obs_files", "rds_dir", "SMOKINVITE2_DAT_pipe", "codes"), 
              envir = environment())


SMOKINVITE2_DAT_big <- parLapply(cl, obs_files, SMOKINVITE2_DAT_pipe, codes = codes, cluster_id = "SMOKINVITE_COD") 

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


# Save the cohort!!!!

saveRDS(dat, "Z:/Group_work/QOF_project/Data/Smoking/cohort/1_cohort_smoking_with_DAT_cols.RDS")

stopCluster(cl)
