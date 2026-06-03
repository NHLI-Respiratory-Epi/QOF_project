library(dplyr)
library(data.table)
library(bit64)

#Load old QOF smoking indicators
old_smok_QOF <- readRDS("Z:/Group_work/QOF_project/Data/Smoking/cohort/3_cohort_smoking_with_rules.RDS")

old_smok_QOF <- old_smok_QOF %>% 
  dplyr::select(patid, 
                SMOK1_REG, 
                SMOK2_REG,
                SMOK002_numerator,
                SMOK002_denominator,
                SMOK002_inc_exc_type,
                SMOK004_numerator,
                SMOK004_denominator,
                SMOK004_inc_exc_type,
                SMOK005_numerator,
                SMOK005_denominator,
                SMOK005_inc_exc_type)

#Load new QOF smoking indicators
new_smok_QOF <- readRDS("Z:/Group_work/QOF_project/Data/Smoking/cohort/7t_updated_cohort_smoking_with_rules.RDS")

new_smok_QOF <- new_smok_QOF %>% 
  dplyr::select(patid, 
                SMOK1_REG_new = SMOK1_REG, 
                SMOK2_REG_new = SMOK2_REG,
                SMOK002_numerator_new = SMOK002_numerator,
                SMOK002_denominator_new = SMOK002_denominator,
                SMOK004_numerator_new = SMOK004_numerator,
                SMOK004_denominator_new = SMOK004_denominator,
                SMOK005_numerator_new = SMOK005_numerator,
                SMOK005_denominator_new = SMOK005_denominator)

#Combine old and new 
combined_smok_QOF <- full_join(old_smok_QOF, new_smok_QOF, by = "patid")


saveRDS(combined_smok_QOF, "X:/QOF_project/Data/Smoking/cohort/8_cohort_smoking_full.RDS")

