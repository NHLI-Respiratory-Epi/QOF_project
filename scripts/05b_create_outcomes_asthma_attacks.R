# Create asthma attack as outcome

library(dplyr)
library(data.table)
library(bit64)
library(parallel)

dat <- readRDS("X:/QOF_project/Data/cohort/5_cohort_with_attacks_after_qof.RDS")

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

#count number of attacks in follow-up after qof
exacs <- exacs %>% mutate(outcome_asthma_attacks = NA)
exacs <- exacs %>% group_by(patid) %>% mutate(outcome_asthma_attacks = n()) %>% ungroup()
exacs <- exacs %>% select(patid, outcome_asthma_attacks) %>% unique()


#add onto cohort
#cohort <- readRDS("X:/QOF_project/Data/cohort/4_cohort_with_covars4.RDS")
cohort <- readRDS("X:/QOF_project/Data/cohort/4_cohort_with_covars_updated4.RDS")
cohort <- left_join(cohort, exacs, by = "patid")

#need follow-up flag too
fu <- readRDS("X:/QOF_project/Data/cohort/5_cohort_with_fu_flag_after_qof.RDS")
cohort <- left_join(cohort, fu, by = "patid")

#make outcome = 0 rather than NA if patid has follow-up time
table(cohort$outcome_asthma_attacks, useNA = "always")
cohort <- cohort %>% mutate(outcome_asthma_attacks = ifelse(in_follow_up_flag == 0,
                                                            NA, outcome_asthma_attacks))
cohort <- cohort %>% mutate(outcome_asthma_attacks = ifelse((in_follow_up_flag == 1 & is.na(outcome_asthma_attacks)),
                                                            0, outcome_asthma_attacks))

#rates - double check that NAs are handled correctly
cohort <- cohort %>% mutate(followup_time = outcomesenddate - outcomesstartdate)
cohort <- cohort %>% mutate(followup_time_pys = (followup_time/365.25))
cohort$followup_time_pys <- as.numeric(cohort$followup_time_pys)
cohort <- cohort %>% mutate(outcome_asthma_attacks_rate = outcome_asthma_attacks/followup_time_pys)
cohort <- cohort %>% mutate(outcome_asthma_attacks_rate_1000 = outcome_asthma_attacks_rate*1000)

#cohort <- saveRDS(cohort, "X:/QOF_project/Data/cohort/5_cohort_with_covars_and_outcomes1.RDS")
cohort <- saveRDS(cohort, "X:/QOF_project/Data/cohort/5_cohort_with_covars_and_outcomes_updated1.RDS")
