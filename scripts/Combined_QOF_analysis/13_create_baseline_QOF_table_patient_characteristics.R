
library(dplyr)
library(data.table)
library(bit64)
library(finalfit)
library(officer)
library(flextable)
library(ggplot2)
library(stringr)


dat <- readRDS("Z:/Group_work/QOF_project/Data/cohort/50_temp_cohort_asthma_QI_comps_added.RDS")


# comparisons of interest, given that most differences are minimal:

# QA_QT vs AA_QT
# QA_QT vs QA_IT

# covars of interest




ind_vars_characteristics <- dat %>% select(gender, region, imd_2019_5, age, eth5, BMI_cat,
                                           gord:htn_qof, baseline_asthma_attacks, baseline_aecopd) %>% 
  colnames()

# patient characteristics table

# all patients

all <- dat %>%
  summary_factorlist(., dependent = NULL, 
                     explanatory = ind_vars_characteristics, fit_id = TRUE,
                     add_col_totals = TRUE) 

asthma <- dat %>%
  filter(AST_REG == 1) %>%
  summary_factorlist(., dependent = NULL, 
                     explanatory = ind_vars_characteristics, fit_id = TRUE,
                     add_col_totals = TRUE)  


COPD <- dat %>%
  filter(COPD_REG == 1) %>%
  summary_factorlist(., dependent = NULL, 
                     explanatory = ind_vars_characteristics, fit_id = TRUE,
                     add_col_totals = TRUE) 


hyp <- dat %>%
  filter(HYP_REG == 1) %>%
  summary_factorlist(., dependent = NULL, 
                     explanatory = ind_vars_characteristics, fit_id = TRUE,
                     add_col_totals = TRUE) 

smok1 <- dat %>% 
  filter(SMOK1_REG == 1) %>%
  summary_factorlist(., dependent = NULL, 
                     explanatory = ind_vars_characteristics, fit_id = TRUE,
                     add_col_totals = TRUE) 

smok2 <- dat %>% 
  filter(SMOK2_REG == 1) %>%
  summary_factorlist(., dependent = NULL, 
                     explanatory = ind_vars_characteristics, fit_id = TRUE,
                     add_col_totals = TRUE) 

