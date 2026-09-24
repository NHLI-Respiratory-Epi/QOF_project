
library(dplyr)
library(data.table)
library(bit64)
library(finalfit)
library(officer)
library(flextable)
library(ggplot2)
library(stringr)


# dat <- readRDS("Z:/Group_work/QOF_project/Data/cohort/07_finalised_cohort_with_QI_comps_added.RDS")
dat <- readRDS("X:/QOF_project/Data/cohort/07_finalised_cohort_with_QI_comps_added.RDS")

dat <- dat %>% filter(gender == "Female")
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
                     add_col_totals = TRUE) %>%
  rename(GMS = "all")

asthma <- dat %>%
  filter(AST_REG_QA_QT == 1) %>%
  summary_factorlist(., dependent = NULL, 
                     explanatory = ind_vars_characteristics, fit_id = TRUE,
                     add_col_totals = TRUE)  %>%
  rename(`Asthma register` = "all")

asthma <- asthma[ ,3:5]


COPD <- dat %>%
  filter(COPD_REG_QC == 1) %>%
  summary_factorlist(., dependent = NULL, 
                     explanatory = ind_vars_characteristics, fit_id = TRUE,
                     add_col_totals = TRUE) %>%
  rename(`COPD register` = "all")

COPD <- COPD[ ,3:5]


hyp <- dat %>%
  filter(HYP_REG_QC_QR == 1) %>%
  summary_factorlist(., dependent = NULL, 
                     explanatory = ind_vars_characteristics, fit_id = TRUE,
                     add_col_totals = TRUE) %>%
  rename(`Hypertension register` = "all")

hyp <- hyp[ ,3:5]

smok1 <- dat %>% 
  filter(SMOK1_REG_current == 1) %>%
  summary_factorlist(., dependent = NULL, 
                     explanatory = ind_vars_characteristics, fit_id = TRUE,
                     add_col_totals = TRUE) %>%
  rename(`Smoking register 1 (specific comorbidities)` = "all")  

smok1 <- smok1[ ,3:5]


smok2 <- dat %>% 
  filter(SMOK2_REG_current == 1) %>%
  summary_factorlist(., dependent = NULL, 
                     explanatory = ind_vars_characteristics, fit_id = TRUE,
                     add_col_totals = TRUE) %>%
  rename(`Smoking register 2 (age >=15)` = "all")  

smok2 <- smok2[ ,3:5]

complete_table <- all %>% left_join(asthma, by = c("fit_id", "index")) %>%
  left_join(COPD, by = c("fit_id", "index")) %>%
  left_join(hyp, by = c("fit_id", "index")) %>%
  left_join(smok1, by = c("fit_id", "index")) %>%
  left_join(smok2, by = c("fit_id", "index")) %>%
  select(-fit_id, -index)

colnames(complete_table)

complete_table[complete_table$levels == "Male",3:8]
complete_table[complete_table$levels == "Male",3:8] <- "-"
complete_table[complete_table$levels == "Indeterminate",3:8]
complete_table[complete_table$levels == "Indeterminate",3:8] <- "-"

complete_table[60:63, c(3, 4, 6, 7, 8)] <- "" # non-COPD have AECOPD removed
complete_table[56:59, c(3, 5, 6, 7, 8)] <- "" # non-asthma have asthma attack removed

# write xlsx file

# writexl::write_xlsx(complete_table, "Z:/Group_work/QOF_project/Alex/Output/combined/Patient_characteristics_by_register_male.xlsx")
writexl::write_xlsx(complete_table, "X:/QOF_project/Alex/Output/combined/Patient_characteristics_by_register_female.xlsx")

