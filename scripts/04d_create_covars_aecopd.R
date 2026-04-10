# Create AECOPD at baseline

library(dplyr)
library(data.table)
library(bit64)
library(parallel)

dat <- readRDS("X:/QOF_project/Data/cohort/4_cohort_with_aecopds.RDS")

#adapting AECOPD algorithm but only have primary care
#   AECOPD ALGORITHM (see Rothnie et al., 2016):
  #
  # Excluding annual review days:
  #		- ABX and OCS for 5–14 days; or
  #	  - Symptom (2+) definition with prescription of antibiotic or OCS; or
  #	  - LRTI code; or
  #		- AECOPD code
#
#confirmed with HW and JKQ that not using symptoms
#duration of 5-14 days is taken care of in previous script

#already merged on by patid and date
exacs <- dat %>% filter((abx == 1 & ocs == 1) | lrti == 1 | aecopd == 1)
exacs <- exacs %>% filter(is.na(copd_review_qof))
exacs <- exacs %>% mutate(aecopd_all = 1)
exacs <- exacs %>% select(patid, aecopd_date, aecopd_all) %>% unique()

#ensure each event is 14 days apart
exacs <- exacs %>% arrange(patid, aecopd_date)
exacs <- exacs %>% group_by(patid) %>% 
  mutate(diff = aecopd_date - dplyr::lag(aecopd_date, 1, default=NA)) %>% ungroup()
exacs <- exacs %>% filter(diff >= 15 | is.na(diff))
exacs <- exacs %>% select(-diff)

#count number of exacs in year prior to start of study
exacs <- exacs %>% mutate(baseline_aecopd = NA)
exacs <- exacs %>% group_by(patid) %>% mutate(baseline_aecopd = n()) %>% ungroup()
exacs <- exacs %>% select(patid, baseline_aecopd) %>% unique()

length(unique(exacs$patid))

#add onto cohort
#cohort <- readRDS("X:/QOF_project/Data/cohort/4_cohort_with_covars2.RDS")
cohort <- readRDS("X:/QOF_project/Data/cohort/4_cohort_with_covars_updated2.RDS")
cohort <- left_join(cohort, exacs, by = "patid")
table(cohort$baseline_aecopd, useNA = "always")
cohort <- cohort %>% mutate(baseline_aecopd = ifelse(!is.na(baseline_aecopd),
                                                     baseline_aecopd, 0))
table(cohort$baseline_aecopd, useNA = "always")
cohort <- cohort %>% mutate(baseline_aecopd = ifelse(baseline_aecopd >= 3,
                                                     3, baseline_aecopd))
table(cohort$baseline_aecopd, useNA = "always")

#cohort <- saveRDS(cohort, "X:/QOF_project/Data/cohort/4_cohort_with_covars3.RDS")
cohort <- saveRDS(cohort, "X:/QOF_project/Data/cohort/4_cohort_with_covars_updated3.RDS")






#start rough counts here

#merge onto COPD patient list
patid_list <- fread("X:/QOF_project/Data/COPD/cohort/all_COMBOS_ptlist.csv")
patid_list <- left_join(patid_list, exacs, by = "patid")
patid_list <- patid_list %>% mutate(baseline_aecopd = ifelse(!is.na(baseline_aecopd),
                                         baseline_aecopd, 0))
patid_list <- patid_list %>% mutate(baseline_aecopd = ifelse(baseline_aecopd >= 3,
                                                       3, baseline_aecopd))
#rough count by registry
qof_reg <- patid_list %>% filter(COMBO1 == 1)
table(qof_reg$baseline_aecopd, useNA = "always")

COMBO2a <- patid_list %>% filter(COMBO2 == 1)
table(COMBO2a$baseline_aecopd, useNA = "always")

COMBO3a <- patid_list %>% filter(COMBO3 == 1)
table(COMBO3a$baseline_aecopd, useNA = "always")

COMBO4a <- patid_list %>% filter(COMBO4 == 1)
table(COMBO4a$baseline_aecopd, useNA = "always")

COMBO5a <- patid_list %>% filter(COMBO5 == 1)
table(COMBO5a$baseline_aecopd, useNA = "always")

COMBO6a <- patid_list %>% filter(COMBO6 == 1)
table(COMBO6a$baseline_aecopd, useNA = "always")

COMBO7a <- patid_list %>% filter(COMBO7 == 1)
table(COMBO7a$baseline_aecopd, useNA = "always")







