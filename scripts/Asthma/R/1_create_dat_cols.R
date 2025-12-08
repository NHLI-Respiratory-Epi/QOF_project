# create the clinical data extraction criteria

library(dplyr)
library(data.table)
library(bit64)
library(parallel)

dat <- readRDS("X:/QOF_project/Data/Asthma/cohort/cohort_asthma_prelim.RDS")
codes <- readRDS("codelists/all/ast_copd_hyp_smok.RDS")

dat <- dat %>% dplyr::mutate(PAT_AGE = as.numeric(ACHV - dob)/365.25)


dat <- dat %>% rename(AST_DAT = AST_COD_earliest,
                      SMOK_DAT = SMOK_COD_latest,
                      # REV_DAT needs to be created
                      ASTLAT_DAT = AST_COD_latest,
                      # ASTRES_DAT needs to be created
                      ASTTRT_DAT = ASTTRT_COD_latest_1yr,
                      ASTINVITE1_DAT = ASTINVITE_COD_earliest_1yr,
                      # ASTINVITE2_DAT needs to be created
                      ASTMONDEC_DAT = ASTMONDEC_COD_latest,
                      ASTPCADEC_DAT = ASTPCADEC_COD_latest,
                      SMOKSTATDEC_DAT = SMOKSTATDEC_COD_latest,
                      # SPIRPCASU_DAT needs to be created
                      # ASTRES1_DAT needs to be created
                      # AST1_DAT needs to be created
                      # EUNRESAST_DAT needs to be created
                      SMOKEXPOS_DAT = SMOKEXPOS_COD_latest,
                      NOSMOKEXPOS_DAT = NOSMOKEXPOS_COD_latest)
# field numbers 22 - 33 (aprt from rule 29) all still have to be created

# First off - create AST_RES.

# we have already have ASTLAT. if ASTLAST is later than ASTRES, then missing, otherwise, ASTRES.

dat <- dat %>% mutate(ASTRES_DAT = if_else(ASTRES_COD_latest > ASTLAT_DAT, 
                                           ASTRES_COD_latest, NA))

# isolating review dates within 1 year takes a bit more time

rds_dir <- "X:/QOF_project/Data/tidy_data/RDS/"

obs_files <- list.files(rds_dir)[grepl("Observation", list.files(rds_dir)) == TRUE] 

# pipe for when we want to return multiple instances of a code

multiextract_pipe <- function(obsfile, rds_dir = "X:/QOF_project/Data/tidy_data/RDS/", codes, cluster_id, newcolname) {

  obs <- readRDS(paste0(rds_dir, obsfile))
  EXTRACT_COD <- codes %>% filter(Cluster_ID == cluster_id) %>% 
    dplyr::pull(snomedctconceptid) %>% unique()



  obs <- obs[snomedctconceptid %in% EXTRACT_COD]
  obs <- obs[obsdate <= "2024-03-31" & obsdate > "2023-03-31"]
  obs <- obs[ , .(patid, obsdate)]
  obs <- unique(obs)
  
  obs <- dcast(
    obs[order(patid, obsdate)],
    patid ~ rowid(patid),
    value.var = "obsdate"
  )
  
setnames(obs, old = names(obs)[-1],
         new = paste0(newcolname, "_", names(obs)[-1]))

return(obs)

}


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
clusterExport(cl, varlist = c("obs_files", "rds_dir", "multiextract_pipe", "codes"), 
              envir = environment())

REV_DAT_big <- parLapply(cl, obs_files, multiextract_pipe, codes = codes, cluster_id = "REV_COD", 
                         newcolname = "REV_DAT") 

REV_DAT <- rbindlist(REV_DAT_big, fill = TRUE)


dat <- left_join(dat, REV_DAT, by = "patid")


# we do the same thing for the other ones we need:

ASTEXACB_DAT_big <- parLapply(cl, obs_files, multiextract_pipe, codes = codes, cluster_id = "ASTEXACB_COD", 
                         newcolname = "ASTEXACB_DAT") 

ASTEXACB_DAT <- rbindlist(ASTEXACB_DAT_big, fill = TRUE)

WRITPASTP_DAT_big <- parLapply(cl, obs_files, multiextract_pipe, codes = codes, cluster_id = "WRITPASTP_COD", 
                              newcolname = "WRITPASTP_DAT") 

WRITPASTP_DAT <- rbindlist(WRITPASTP_DAT_big, fill = TRUE)


dat <- left_join(dat, ASTEXACB_DAT, by = "patid")
dat <- left_join(dat, WRITPASTP_DAT, by = "patid")


# now we create ASTINVITE2, which requires ASTINVITE1.

# this means we need to join our cohort back to the obsfiles we are reading in. 
# I think the quickest way to do this will just be to extract all asthma invite codes to the environment and go from there.



ASTINVITE2_DAT_pipe <- function(obsfile, rds_dir = "X:/QOF_project/Data/tidy_data/RDS/", codes, cluster_id) {
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


clusterExport(cl, varlist = c("obs_files", "rds_dir", "multiextract_pipe", "ASTINVITE2_DAT_pipe", "codes"), 
              envir = environment())


ASTINVITE2_DAT_big <- parLapply(cl, obs_files, ASTINVITE2_DAT_pipe, codes = codes, cluster_id = "ASTINVITE_COD") 

ASTINVITE2_DAT <- rbindlist(ASTINVITE2_DAT_big)
ASTINVITE2_DAT <- unique(ASTINVITE2_DAT)



# ASTINVITE2_DAT %>% group_by(patid) %>% slice(1) %>% nrow()
# dat %>% filter(!is.na(ASTINVITE1_DAT)) %>% nrow()


# join with subsection of our cohort

ASTINVITE2_DAT <- left_join(ASTINVITE2_DAT, dplyr::select(dat, patid, ASTINVITE1_DAT, ACHV, QSSD), by = "patid")

ASTINVITE2_DAT <- ASTINVITE2_DAT %>% filter(!is.na(ASTINVITE1_DAT)) %>% 
  filter(obsdate >= ASTINVITE1_DAT + 7) %>% arrange(patid, obsdate) %>% 
  group_by(patid) %>% slice(1) %>% ungroup() %>% rename(ASTINVITE2_DAT = obsdate) %>% 
  dplyr::select(patid, ASTINVITE2_DAT)

# bind it into our cohort

dat <- left_join(dat, ASTINVITE2_DAT, by = "patid")



dat <- dat %>% mutate(ASTRES1_DAT = if_else(ASTRES_COD_latest > AST_DAT, 
                                                  ASTRES_COD_latest, NA))

# AST1_DAT requires use of a pipe again

AST1_DAT_pipe <- function(obsfile, rds_dir = "X:/QOF_project/Data/tidy_data/RDS/", codes, cluster_id) {
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



clusterExport(cl, varlist = c("obs_files", "rds_dir", "multiextract_pipe", "ASTINVITE2_DAT_pipe", 
                              "AST1_DAT_pipe", "codes"), 
              envir = environment())

AST1_DAT_big <- parLapply(cl, obs_files, AST1_DAT_pipe, codes = codes, cluster_id = "AST_COD") 

AST1_DAT <- rbindlist(AST1_DAT_big)

AST1_DAT <- left_join(AST1_DAT, dplyr::select(dat, patid, ASTRES1_DAT, ACHV, QSSD), by = "patid")

head(AST1_DAT)

AST1_DAT <- AST1_DAT %>% filter(!is.na(ASTRES1_DAT)) %>% 
  filter(obsdate >= ASTRES1_DAT) %>% arrange(patid, desc(obsdate)) %>% # latest after ASTRES1 
  group_by(patid) %>% slice(1) %>% ungroup() %>% rename(AST1_DAT = obsdate) %>% 
  dplyr::select(patid, AST1_DAT)

# bind it into our cohort

dat <- left_join(dat, AST1_DAT, by = "patid")


dat <- dat %>% mutate(EUNRESAST_DAT = if_else(is.na(ASTRES_DAT) & is.na(ASTRES1_DAT), 
                                              AST_DAT, AST1_DAT))

# checking - all looks good ot me. 
# dat %>% filter(!is.na(AST_DAT)) %>%  select(AST_DAT, ASTLAT_DAT, ASTRES_DAT, ASTRES1_DAT, AST1_DAT,
#                                             EUNRESAST_DAT, ASTRES_COD_latest) %>% print(100) 
# 
# dat %>% filter(!is.na(AST1_DAT)) %>%  select(AST_DAT, ASTLAT_DAT, ASTRES_DAT, ASTRES1_DAT, AST1_DAT,
#                                             EUNRESAST_DAT, ASTRES_COD_latest) %>% print(100) 
# 
# dat %>% filter(!is.na(AST_DAT)) %>%  filter(is.na(EUNRESAST_DAT)) %>% select(AST_DAT, ASTLAT_DAT, ASTRES_DAT, ASTRES1_DAT, AST1_DAT,
#                                             EUNRESAST_DAT, ASTRES_COD_latest) %>% print(100) 





# now that we have the unresolved asthma, we can add in a lot of the other things


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




clusterExport(cl, varlist = c("obs_files", "rds_dir", "multiextract_pipe", "ASTINVITE2_DAT_pipe", 
                              "AST1_DAT_pipe", "GENERIC_EXTRACTION_pipe", "codes"), 
              envir = environment())

SPIRPCASU_DAT_big <- parLapply(cl, obs_files, GENERIC_EXTRACTION_pipe, codes = codes, cluster_id = "SPIRPCASU_COD") 
FENODIAG_DAT_big <- parLapply(cl, obs_files, GENERIC_EXTRACTION_pipe, codes = codes, cluster_id = "FENO_COD") 
ASTSPIRDIAG_DAT_big <- parLapply(cl, obs_files, GENERIC_EXTRACTION_pipe, codes = codes, cluster_id = "ASTSPIR_COD") 
PEFRVAR_DAT_big <- parLapply(cl, obs_files, GENERIC_EXTRACTION_pipe, codes = codes, cluster_id = "PEFRVAR_COD") 
ASTDIAGINVITE_prelim_big <- parLapply(cl, obs_files, GENERIC_EXTRACTION_pipe, codes = codes, cluster_id = "ASTINVITE_COD") 
BRONCCHALENG_DAT_big <- parLapply(cl, obs_files, GENERIC_EXTRACTION_pipe, codes = codes, cluster_id = "BRONCCHALENG_COD") 
FBC_DAT_big <- parLapply(cl, obs_files, GENERIC_EXTRACTION_pipe, codes = codes, cluster_id = "FBC_COD") 
IGE_DAT_big <- parLapply(cl, obs_files, GENERIC_EXTRACTION_pipe, codes = codes, cluster_id = "IGE_COD") 
SKINTEST_DAT_big <- parLapply(cl, obs_files, GENERIC_EXTRACTION_pipe, codes = codes, cluster_id = "SKINTEST_COD")  


# so many of these require the unresolved asthma date, we can do them all together in a pipe.
# some subtle differences though so a few tweaks to the pipes.
# this pipe isn't for parallel processing.


stage2_pipe_SPIRPCASU <- function(bigdat, cohort) {
  x <- rbindlist(bigdat)
  cohort <- cohort %>% dplyr::select(patid, EUNRESAST_DAT)
  x <- left_join(x, cohort, by = "patid")
  x[obsdate >= EUNRESAST_DAT] # this one isn't minus 93 days
  x <- x %>% arrange(patid, obsdate) # earliest
  x <- x %>% group_by(patid) %>% slice(1) %>% ungroup(patid) %>%
    dplyr::select(patid, obsdate)
  return(x)
}

SPIRPCASU_DAT <- stage2_pipe_SPIRPCASU(SPIRPCASU_DAT_big, cohort = dat) %>% 
  dplyr::rename(SPIRPCASU_DAT = obsdate)

stage2_pipe_93days <- function(bigdat, cohort) {
  x <- rbindlist(bigdat)
  cohort <- cohort %>% dplyr::select(patid, EUNRESAST_DAT)
  x <- left_join(x, cohort, by = "patid")
  x[obsdate >= (EUNRESAST_DAT - 93)] # minus 93 days
  x <- x %>% arrange(patid, obsdate) # earliest
  x <- x %>% group_by(patid) %>% slice(1) %>% ungroup(patid) %>%
    dplyr::select(patid, obsdate)
  return(x)
}

FENODIAG_DAT <- stage2_pipe_93days(FENODIAG_DAT_big, dat) %>%
  dplyr::rename(FENODIAG_DAT = obsdate)

ASTSPIRDIAG_DAT <- stage2_pipe_93days(ASTSPIRDIAG_DAT_big, dat) %>%
  dplyr::rename(ASTSPIRDIAG_DAT = obsdate)

PEFRVAR_DAT <- stage2_pipe_93days(PEFRVAR_DAT_big, dat) %>%
  dplyr::rename(PEFRVAR_DAT = obsdate)

BRONCCHALENG_DAT <- stage2_pipe_93days(BRONCCHALENG_DAT_big, dat) %>%
  dplyr::rename(BRONCCHALENG_DAT = obsdate)

FBC_DAT <- stage2_pipe_93days(FBC_DAT_big, dat) %>%
  dplyr::rename(FBC_DAT = obsdate)

IGE_DAT <- stage2_pipe_93days(IGE_DAT_big, dat) %>%
  dplyr::rename(IGE_DAT = obsdate)

SKINTEST_DAT <- stage2_pipe_93days(SKINTEST_DAT_big, dat) %>%
  dplyr::rename(SKINTEST_DAT = obsdate)


# join them all in

dat <- dat %>% 
  left_join(SPIRPCASU_DAT, by = "patid") %>%
  left_join(FENODIAG_DAT, by = "patid") %>%
  left_join(ASTSPIRDIAG_DAT, by = "patid") %>%
  left_join(PEFRVAR_DAT, by = "patid") %>%
  left_join(BRONCCHALENG_DAT, by = "patid") %>%
  left_join(FBC_DAT, by = "patid") %>%
  left_join(IGE_DAT, by = "patid") %>%
  left_join(SKINTEST_DAT, by = "patid") 

# the asthma invite ones take a bit more thought and are easier to do outside a pipe:
ASTDIAGINVITE_obs <- rbindlist(ASTDIAGINVITE_prelim_big)



ASTDIAGINVITE1 <- left_join(ASTDIAGINVITE_obs, dplyr::select(dat, patid, EUNRESAST_DAT), by = "patid")

ASTDIAGINVITE1_DAT <- ASTDIAGINVITE1 %>% filter(!is.na(EUNRESAST_DAT)) %>% 
  filter(obsdate >= EUNRESAST_DAT - 93 & obsdate <= EUNRESAST_DAT + 93) %>% 
  arrange(patid, obsdate) %>% # earliest 
  group_by(patid) %>% slice(1) %>% ungroup() %>% rename(ASTDIAGINVITE1_DAT = obsdate) %>% 
  dplyr::select(patid, ASTDIAGINVITE1_DAT)


# bind it into our cohort

dat <- left_join(dat, ASTDIAGINVITE1_DAT, by = "patid")

# then create the astdiag2

ASTDIAGINVITE2 <- left_join(ASTDIAGINVITE_obs, dplyr::select(dat, patid, EUNRESAST_DAT, ASTDIAGINVITE1_DAT), by = "patid")

ASTDIAGINVITE2_DAT <- ASTDIAGINVITE2 %>% filter(!is.na(EUNRESAST_DAT)) %>% 
  filter(!is.na(ASTDIAGINVITE1_DAT)) %>%
  filter(obsdate >= ASTDIAGINVITE1_DAT + 7 & obsdate <= EUNRESAST_DAT + 93) %>% 
  arrange(patid, obsdate) %>% # earliest 
  group_by(patid) %>% slice(1) %>% ungroup() %>% rename(ASTDIAGINVITE2_DAT = obsdate) %>% 
  dplyr::select(patid, ASTDIAGINVITE2_DAT)

dat <- left_join(dat, ASTDIAGINVITE2_DAT, by = "patid")

# at this point... we are finished!!!!!!!!!!!

# Save the cohort!!!!

dat <- saveRDS(dat, "Z:/Group_work/QOF_project/Data/Asthma/cohort/1_cohort_asthma_with_DAT_cols.RDS")

stopCluster(cl)
