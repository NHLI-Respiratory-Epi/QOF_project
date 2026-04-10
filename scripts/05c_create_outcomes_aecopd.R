# Create AECOPD as outcome

library(dplyr)
library(data.table)
library(bit64)
library(parallel)

dat <- readRDS("X:/QOF_project/Data/cohort/5_cohort_with_aecopds_after_qof.RDS")

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

#count number of exacs in follow-up after qof
exacs <- exacs %>% mutate(outcome_aecopd = NA)
exacs <- exacs %>% group_by(patid) %>% mutate(outcome_aecopd = n()) %>% ungroup()
exacs <- exacs %>% select(patid, outcome_aecopd) %>% unique()


length(unique(exacs$patid))

#add onto cohort
#cohort <- readRDS("X:/QOF_project/Data/cohort/5_cohort_with_covars_and_outcomes1.RDS")
cohort <- readRDS("X:/QOF_project/Data/cohort/5_cohort_with_covars_and_outcomes_updated1.RDS")
cohort <- left_join(cohort, exacs, by = "patid")

#make outcome = 0 rather than NA if patid has follow-up time
table(cohort$outcome_aecopd, useNA = "always")
cohort <- cohort %>% mutate(outcome_aecopd = ifelse(in_follow_up_flag == 0,
                                                            NA, outcome_aecopd))
cohort <- cohort %>% mutate(outcome_aecopd = ifelse((in_follow_up_flag == 1 & is.na(outcome_aecopd)),
                                                            0, outcome_aecopd))

#rates - double check that NAs are handled correctly
#already have follow up time from previous script
cohort <- cohort %>% mutate(outcome_aecopd_rate = outcome_aecopd/followup_time_pys)
cohort <- cohort %>% mutate(outcome_aecopd_rate_1000 = outcome_aecopd_rate*1000)




#LD later adding on IMD to cohort
imd <- fread("X:/raw_data/QOF/IMD_linkage/Results _Type2_25_005708/Results _Type2_25_005708/Aurum_linked/Final/patient_2019_imd_25_005708.txt")
cohort <- left_join(cohort, dplyr::select(imd, patid, e2019_imd_10), by = "patid")
table(cohort$e2019_imd_10, useNA = "always")

#categorize quintiles and deciles
cohort <- cohort %>% mutate(imd_2019_10 = case_when(
  e2019_imd_10 == 1 ~ "1 - Least Deprived",
  e2019_imd_10 == 2 ~ "2",
  e2019_imd_10 == 3 ~ "3",
  e2019_imd_10 == 4 ~ "4",
  e2019_imd_10 == 5 ~ "5",
  e2019_imd_10 == 6 ~ "6",
  e2019_imd_10 == 7 ~ "7",
  e2019_imd_10 == 8 ~ "8",
  e2019_imd_10 == 9 ~ "9",
  e2019_imd_10 == 10 ~ "10 - Most Deprived",
  TRUE ~ "Missing IMD")) %>%
  mutate(imd_2019_10= factor(imd_2019_10,
                          levels = c("1 - Least Deprived",
                                     "2",
                                     "3",
                                     "4",
                                     "5",
                                     "6",
                                     "7",
                                     "8",
                                     "9",
                                     "10 - Most Deprived",
                                     "Missing IMD")))

cohort <- cohort %>% mutate(imd_2019_5 = case_when(
  e2019_imd_10 == 1 | e2019_imd_10 == 2  ~ "1 - Least Deprived",
  e2019_imd_10 == 3 | e2019_imd_10 == 4 ~ "2",
  e2019_imd_10 == 5 | e2019_imd_10 == 6 ~ "3",
  e2019_imd_10 == 7 | e2019_imd_10 == 8 ~ "4",
  e2019_imd_10 == 9 | e2019_imd_10 == 10 ~ "5 - Most Deprived",
  TRUE ~ "Missing IMD")) %>%
  mutate(imd_2019_5= factor(imd_2019_5,
                             levels = c("1 - Least Deprived",
                                        "2",
                                        "3",
                                        "4",
                                        "5 - Most Deprived",
                                        "Missing IMD")))



#cohort <- saveRDS(cohort, "X:/QOF_project/Data/cohort/5_cohort_with_covars_and_outcomes2.RDS")
cohort <- saveRDS(cohort, "X:/QOF_project/Data/cohort/5_cohort_with_covars_and_outcomes_updated2.RDS")




