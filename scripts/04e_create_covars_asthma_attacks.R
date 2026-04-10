# Create asthma attack at baseline

library(dplyr)
library(data.table)
library(bit64)
library(parallel)

dat <- readRDS("X:/QOF_project/Data/cohort/4_cohort_with_attacks.RDS")

#adapting asthma attack algorithm but only have primary care
#
# Excluding annual review days:
#		- OCS for 5–14 days; or
#		- Asthma attack code
#
#duration of 5-14 days is taken care of in previous script

#already merged on by patid and date
exacs <- dat %>% filter(asthma_attack == 1 | ocs == 1)
exacs <- exacs %>% filter(is.na(asthma_review_qof))
exacs <- exacs %>% mutate(attack_all = 1)
exacs <- exacs %>% select(patid, asthma_attack_date, attack_all) %>% unique()

#ensure each event is 14 days apart
exacs <- exacs %>% arrange(patid, asthma_attack_date) 
exacs <- exacs %>% group_by(patid) %>% 
  mutate(diff = asthma_attack_date - dplyr::lag(asthma_attack_date, 1, default=NA)) %>% ungroup()
exacs <- exacs %>% filter(diff >= 15 | is.na(diff))
exacs <- exacs %>% select(-diff)

#count number of attacks in year prior to start of study
exacs <- exacs %>% mutate(baseline_asthma_attacks = NA)
exacs <- exacs %>% group_by(patid) %>% mutate(baseline_asthma_attacks = n()) %>% ungroup()
exacs <- exacs %>% select(patid, baseline_asthma_attacks) %>% unique()

                   
#add onto cohort
#cohort <- readRDS("X:/QOF_project/Data/cohort/4_cohort_with_covars3.RDS")
cohort <- readRDS("X:/QOF_project/Data/cohort/4_cohort_with_covars_updated3.RDS")

cohort <- left_join(cohort, exacs, by = "patid")

cohort <- cohort %>% mutate(baseline_asthma_attacks = ifelse(!is.na(baseline_asthma_attacks),
                                                             baseline_asthma_attacks, 0))
cohort <- cohort %>% mutate(baseline_asthma_attacks = ifelse(baseline_asthma_attacks >= 3,
                                                             3, baseline_asthma_attacks))

#cohort <- saveRDS(cohort, "X:/QOF_project/Data/cohort/4_cohort_with_covars4.RDS")
cohort <- saveRDS(cohort, "X:/QOF_project/Data/cohort/4_cohort_with_covars_updated4.RDS")
