library(dplyr)
library(data.table)
library(bit64)
library(readr)

dat <- readRDS("Z:/Group_work/QOF_project/Data/Smoking/cohort/3_cohort_smoking_with_rules.RDS")
codes <- readRDS("codelists/all/ast_copd_hyp_smok.RDS")

refersssa_codes <- codes %>%
  filter(Cluster_ID == "REFERSSSA_COD")

refersssa_class <- read_csv("codelists/all/REFERSSSA_class.csv",
                            col_types = cols(
                              conceptId = col_character(),
                              term = col_character(),
                              REFERSSSACLASS = col_character(),
                              ...4 = col_logical(),
                              ...5 = col_logical()
                            )) %>%
  select(conceptId,
         term,
         REFERSSSACLASS) %>%
  mutate(conceptId = as.integer64(conceptId))


refersssa_codes <- refersssa_codes %>%
  mutate(snomedctconceptid = as.integer64(snomedctconceptid)) %>% 
  left_join(refersssa_class,
            by = c("snomedctconceptid" = "conceptId"))


# (RS1) have 'smoking cessation education given' without 'smoking cessation
# education declined' on the same day
# (RS2) have 'smoking cessation education referred' without 'smoking cessation
# education declined' on the same day
# (RS3) have 'referral for smoking cessation service' without 'declined smoking
# cessation service' on the same day
# (RS4) have 'patient received a smoking cessation service' without 'declined
# smoking cessation service' on the same day
# (RS5) patient declined smoking cessation education
# (RS6) declined smoking cessation service referral


multiextract_pipe <- function(obsfile, rds_dir = "X:/QOF_project/Data/tidy_data/RDS/", codes, refersssaclass) {
  
  obs <- readRDS(paste0(rds_dir, obsfile))
  
  EXTRACT_COD <- codes %>%
    filter(REFERSSSACLASS == refersssaclass) %>%  
    dplyr::pull(snomedctconceptid) %>% unique()
  
  
  obs <- obs[snomedctconceptid %in% EXTRACT_COD]
  obs <- obs[obsdate <= "2024-03-31" & obsdate > "2022-03-31"]
  obs <- obs[ , .(patid, obsdate)]
  obs <- unique(obs)
  
  if (nrow(obs) == 0) {
    return(select(obs, patid))
  } else {
    
    
    
    
    obs <- dcast(
      obs[order(patid, obsdate)],
      patid ~ rowid(patid),
      value.var = "obsdate"
    )
    
    setnames(obs, old = names(obs)[-1],
             new = paste0(refersssaclass, "_", names(obs)[-1]))
    
    return(obs)
    
  }
}




# REFERSSSA_CLASS_pipe <- function(
    #     obsfile,
#     rds_dir = "X:/QOF_project/Data/tidy_data/RDS/",
#     codes,
#     refersssaclass) {
#   
#   message(paste0("REFERSSSA Class extracted: ", refersssaclass)) 
#   
#   obs <- readRDS(paste0(rds_dir, obsfile))
#   
#   EXTRACT_COD <- codes %>%
#     filter(REFERSSSACLASS == refersssaclass) %>%  
#     dplyr::pull(snomedctconceptid) %>% unique()
#   
#   obs <- obs[snomedctconceptid %in% EXTRACT_COD]
#   obs <- obs[obsdate <= "2024-03-31" & obsdate > "2023-03-31"]
#   obs <- obs[ , .(patid, obsdate)]
#   obs <- unique(obs)
#   
#   return(obs)
#   
# }

rds_dir <- "X:/QOF_project/Data/tidy_data/RDS/"

obs_files <- list.files(rds_dir)[grepl("Observation",
                                       list.files(rds_dir)) == TRUE] 

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

clusterExport(cl,
              varlist = c("obs_files",
                          "rds_dir",
                          "multiextract_pipe",
                          "refersssa_codes"), 
              envir = environment())

### RS6 ########################################################################

SCS_declined_big <- parLapply(cl,
                              obs_files,
                              multiextract_pipe,
                              codes = refersssa_codes,
                              refersssaclass = "SCS_declined") 

SCS_declined <- rbindlist(SCS_declined_big, fill = TRUE)



### RS5 #########################################################################



SCE_declined_big <- parLapply(cl,
                              obs_files,
                              multiextract_pipe,
                              codes = refersssa_codes,
                              refersssaclass = "SC_education_declined") 

SCE_declined <- rbindlist(SCE_declined_big, fill = TRUE)


### RS2 #########################################################################
SCE_referral_big <- parLapply(cl,
                              obs_files,
                              multiextract_pipe,
                              codes = refersssa_codes,
                              refersssaclass = "SC_education_referral") 

SCE_referral <- rbindlist(SCE_referral_big, fill = TRUE)

### RS3 #########################################################################
SCS_referral_big <- parLapply(cl,
                              obs_files,
                              multiextract_pipe,
                              codes = refersssa_codes,
                              refersssaclass = "SCS_referral") 

SCS_referral <- rbindlist(SCS_referral_big, fill = TRUE)
### RS4 #########################################################################
SCS_given_big <- parLapply(cl,
                           obs_files,
                           multiextract_pipe,
                           codes = refersssa_codes,
                           refersssaclass = "SCS_given") 

SCS_given <- rbindlist(SCS_given_big, fill = TRUE)
### RS1 #########################################################################
SCE_given_big <- parLapply(cl,
                           obs_files,
                           multiextract_pipe,
                           codes = refersssa_codes,
                           refersssaclass = "SC_education_given") 

SCE_given <- rbindlist(SCE_given_big, fill = TRUE)




### join them all in
dat <- dat %>% 
  left_join(SCS_declined, by = "patid") %>%
  left_join(SCE_declined, by = "patid") %>%
  left_join(SCE_referral, by = "patid") %>%
  left_join(SCS_referral, by = "patid") %>%
  left_join(SCS_given, by = "patid") %>%
  left_join(SCE_given, by = "patid")


dat <- saveRDS(dat, "Z:/Group_work/QOF_project/Data/Smoking/cohort/4a_cohort_smoking_with_refersssa_cols.RDS")

stopCluster(cl)
