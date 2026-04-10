# Apply ethnicity codelist to cohort

library(dplyr)
library(data.table)
library(bit64)
library(parallel)

#dat <- readRDS("X:/QOF_project/Data/cohort/4_cohort_with_covars1.RDS")
dat <- readRDS("X:/QOF_project/Data/cohort/4_cohort_with_covars_updated1.RDS")

#ethnicity codelist from AA
ethnicity_codes <- fread("X:/QOF_project/Creating_codelists/Covariates/excel/ethnicity.csv")
ethnicity_codes$medcodeid <- as.integer64(ethnicity_codes$medcodeid)

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

#extraction pipe for non-QOF codelists
COVAR_extraction_pipe <- function(obsfile, rds_dir = "X:/QOF_project/Data/tidy_data/RDS/", codes) {
  
  obs <- readRDS(paste0(rds_dir, obsfile))
  EXTRACT_COD <- codes %>% dplyr::pull(medcodeid) %>% unique()
  
  obs <- obs[medcodeid %in% EXTRACT_COD]
  obs <- obs[obsdate <= "2023-03-31"] #want closest to before that date
  obs <- obs[ , .(patid, obsdate, medcodeid)] #going to need to merge on ethnicity variables
  obs <- unique(obs)
  
  return(obs)
}

#add everything needed for cluster
clusterExport(cl, varlist = c("obs_files", "rds_dir", "COVAR_extraction_pipe", "ethnicity_codes"), 
              envir = environment())

#apply pipe to codelists
ETHNICITY_DAT_big <- parLapply(cl, obs_files, COVAR_extraction_pipe, codes = ethnicity_codes)

#ETHNICITY
ETHNICITY_DAT <- rbindlist(ETHNICITY_DAT_big)
ETHNICITY_DAT <- left_join(ETHNICITY_DAT, dplyr::select(ethnicity_codes, medcodeid, eth5, eth19, eth19_grouped), by = "medcodeid")
ETHNICITY_DAT <- left_join(ETHNICITY_DAT, dplyr::select(dat, patid, ACHV, QSSD), by = "patid")

# Using AA's algorithm:

# Ethnicity algorithm

# use the most commonly recorded ethnicity.
# if multiple ethnicities are the most common, use the most recent valid ethnicity
head(ETHNICITY_DAT)

nrow(ETHNICITY_DAT)

length(unique(ETHNICITY_DAT$patid))

table(ETHNICITY_DAT$eth19)
table(ETHNICITY_DAT$eth19_grouped)


# 1) Remove any rows where eth19 == "not stated" (case???insensitive)
dt <- ETHNICITY_DAT[eth19 != "Not Stated"]


# 2) For each (patid, eth19), compute:
#      - count     = number of times that eth19 appears for that patid
#      - last_date = the most recent obsdate when that eth19 was coded
dt_summary19 <- dt[ , .(
  count     = .N,
  last_date = max(obsdate)
), by = .(patid, eth19)]


# 3) Within each patid, figure out:
#      - max_count = maximum of count across all eth19 for that patid
#      - n_ties    = how many ethnicities share that max_count
dt_summary19[ , `:=`(
  max_count = max(count),
  n_ties    = sum(count == max(count))
), by = patid]

dt_final19 <- dt_summary19[
  count == max_count,
  {
    i <- if (.N > 1L) which.max(last_date) else 1L
    .(ethnicity = eth19[i],
      method    = ifelse(.N > 1L, "recent", "mode"))
  },
  by = patid
]

dt_final19 <- dt_final19 %>% rename(eth19 = ethnicity, eth19_meth = method)

# 2) For each (patid, eth19), compute:
#      - count     = number of times that eth19 appears for that patid
#      - last_date = the most recent obsdate when that eth19 was coded
dt_summary5 <- dt[ , .(
  count     = .N,
  last_date = max(obsdate)
), by = .(patid, eth19_grouped)]


# 3) Within each patid, figure out:
#      - max_count = maximum of count across all eth19 for that patid
#      - n_ties    = how many ethnicities share that max_count
dt_summary5[ , `:=`(
  max_count = max(count),
  n_ties    = sum(count == max(count))
), by = patid]

dt_final5 <- dt_summary5[
  count == max_count,
  {
    i <- if (.N > 1L) which.max(last_date) else 1L
    .(ethnicity = eth19_grouped[i],
      method    = ifelse(.N > 1L, "recent", "mode"))
  },
  by = patid
]

dt_final5 <- dt_final5 %>% rename(eth5 = ethnicity, eth5_meth = method)

# add into one data table
final <- dt_final5[dt_final19, on = "patid"]

table(final$eth19)
table(final$eth19_meth)
table(final$eth5)
table(final$eth5_meth)

table(final$eth19, final$eth5)

# almost everyone seems pretty consistent. Almost everyone is identified by the mode,
# not the most recent date. 

#add onto cohort
final <- final %>% select(patid, eth19, eth5)
dat <- left_join(dat, final, by = "patid")

dat$eth5[is.na(dat$eth5)] <- "Unknown / Not Stated"
dat$eth19[is.na(dat$eth19)] <- "Unknown / Not Stated"

table(dat$eth5, dat$eth19)

# Save the cohort
#dat <- saveRDS(dat, "X:/QOF_project/Data/cohort/4_cohort_with_covars2.RDS")
dat <- saveRDS(dat, "X:/QOF_project/Data/cohort/4_cohort_with_covars_updated2.RDS")

stopCluster(cl)



