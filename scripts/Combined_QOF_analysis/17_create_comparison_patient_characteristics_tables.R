

library(dplyr)
library(data.table)
library(bit64)
library(finalfit)
library(officer)
library(flextable)
library(ggplot2)
library(stringr)


dat <- readRDS("Z:/Group_work/QOF_project/Data/cohort/07_finalised_cohort_with_QI_comps_added.RDS")


# comparisons of interest, given that most differences are minimal:

# QA_QT vs AA_QT
# QA_QT vs QA_IT

# covars of interest




ind_vars_characteristics <- dat %>% select(gender, region, imd_2019_5, age, eth5, BMI_cat,
                                           gord:htn_qof, baseline_asthma_attacks, baseline_aecopd) %>% 
  colnames()


# asthma

# patient characteristics table
AST007_inc_exc_type_QA_QT_table <- dat %>%
  filter(!is.na(AST007_inc_exc_type_QA_QT)) %>% 
  summary_factorlist(., dependent = "AST007_inc_exc_type_QA_QT", 
                     explanatory = ind_vars_characteristics,
                     p = TRUE,total_col = TRUE, add_col_totals = TRUE)


ft_AST <- flextable(AST007_inc_exc_type_QA_QT_table)
ft_AST <- fontsize(ft_AST, size = 9, part = "all")
ft_AST <- width(ft_AST, j = 1, width = 2.2)      # first/left_AST column
ft_AST <- width(ft_AST, j = 2:ncol(AST007_inc_exc_type_QA_QT_table), width = 1.0)  # remaining columns
ft_AST <- autofit(ft_AST)


# COPD

# patient characteristics table
COPD010_inc_exc_type_table <- dat %>%
  filter(!is.na(COPD010_inc_exc_type)) %>% 
  summary_factorlist(., dependent = "COPD010_inc_exc_type", 
                     explanatory = ind_vars_characteristics,
                     p = TRUE,total_col = TRUE, add_col_totals = TRUE)


ft_COPD <- flextable(COPD010_inc_exc_type_table)
ft_COPD <- fontsize(ft_COPD, size = 9, part = "all")
ft_COPD <- width(ft_COPD, j = 1, width = 2.2)      # first/left_COPD column
ft_COPD <- width(ft_COPD, j = 2:ncol(COPD010_inc_exc_type_table), width = 1.0)  # remaining columns
ft_COPD <- autofit(ft_COPD)

# Hypertension

# patient characteristics table
HYP008_inc_exc_type_table <- dat %>%
  filter(!is.na(HYP008_inc_exc_type_QC_QR)) %>% 
  summary_factorlist(., dependent = "HYP008_inc_exc_type_QC_QR", 
                     explanatory = ind_vars_characteristics,
                     p = TRUE,total_col = TRUE, add_col_totals = TRUE)


ft_HYP <- flextable(HYP008_inc_exc_type_table)
ft_HYP <- fontsize(ft_HYP, size = 9, part = "all")
ft_HYP <- width(ft_HYP, j = 1, width = 2.2)      # first/left_HYP column
ft_HYP <- width(ft_HYP, j = 2:ncol(HYP008_inc_exc_type_table), width = 1.0)  # remaining columns
ft_HYP <- autofit(ft_HYP)

# Hypertension

# patient characteristics table
HYP009_inc_exc_type_table <- dat %>%
  filter(!is.na(HYP009_inc_exc_type_QC_QR)) %>% 
  summary_factorlist(., dependent = "HYP009_inc_exc_type_QC_QR", 
                     explanatory = ind_vars_characteristics,
                     p = TRUE,total_col = TRUE, add_col_totals = TRUE)


ft_HYP1 <- flextable(HYP009_inc_exc_type_table)
ft_HYP1 <- fontsize(ft_HYP1, size = 9, part = "all")
ft_HYP1 <- width(ft_HYP1, j = 1, width = 2.2)      # first/left_HYP1 column
ft_HYP1 <- width(ft_HYP1, j = 2:ncol(HYP009_inc_exc_type_table), width = 1.0)  # remaining columns
ft_HYP1 <- autofit(ft_HYP1)

# Smoking

# patient characteristics table
SMOK002_inc_exc_type_table <- dat %>%
  filter(!is.na(SMOK002_inc_exc_type_current)) %>% 
  summary_factorlist(., dependent = "SMOK002_inc_exc_type_current", 
                     explanatory = ind_vars_characteristics,
                     p = TRUE,total_col = TRUE, add_col_totals = TRUE)


ft_SMOK <- flextable(SMOK002_inc_exc_type_table)
ft_SMOK <- fontsize(ft_SMOK, size = 9, part = "all")
ft_SMOK <- width(ft_SMOK, j = 1, width = 2.2)      # first/left_SMOK column
ft_SMOK <- width(ft_SMOK, j = 2:ncol(SMOK002_inc_exc_type_table), width = 1.0)  # remaining columns
ft_SMOK <- autofit(ft_SMOK)

# patient characteristics table
SMOK004_inc_exc_type_table <- dat %>%
  filter(!is.na(SMOK004_inc_exc_type_current)) %>% 
  summary_factorlist(., dependent = "SMOK004_inc_exc_type_current", 
                     explanatory = ind_vars_characteristics,
                     p = TRUE,total_col = TRUE, add_col_totals = TRUE)


ft_SMOK1 <- flextable(SMOK004_inc_exc_type_table)
ft_SMOK1 <- fontsize(ft_SMOK1, size = 9, part = "all")
ft_SMOK1 <- width(ft_SMOK1, j = 1, width = 2.2)      # first/left_SMOK1 column
ft_SMOK1 <- width(ft_SMOK1, j = 2:ncol(SMOK004_inc_exc_type_table), width = 1.0)  # remaining columns
ft_SMOK1 <- autofit(ft_SMOK1)



# this is a bit of code that can make all the documents landscape
# if added

ps_land <- prop_section(
  page_size = page_size(orient = "landscape"),
  type = "continuous"
)

doc <- read_docx() %>% #  body_end_section_landscape() %>%
  body_add_par("AST007", 
               style = "Normal") %>%
  body_add_par(" ") %>%
  body_add_flextable(ft_AST) %>%
  body_add_par("COPD010", 
               style = "Normal") %>%
  body_add_par(" ") %>%
  body_add_flextable(ft_COPD) %>%
  body_add_par("SMOK002", 
               style = "Normal") %>%
  body_add_par(" ") %>%
  body_add_flextable(ft_SMOK) %>%
  body_add_par("SMOK004", 
               style = "Normal") %>%
  body_add_par(" ") %>%
  body_add_flextable(ft_SMOK1) %>%
  body_add_par("HYP008", 
               style = "Normal") %>%
  body_add_par(" ") %>%
  body_add_flextable(ft_HYP) %>%
  body_add_par("HYP009", 
               style = "Normal")  %>%
  body_add_par(" ") %>%
  body_add_flextable(ft_HYP1) %>%
  body_end_block_section(value = block_section(property = ps_land))

print(doc, target = "Z:/Group_work/QOF_project/Alex/Output/inc_exc_patient_characteristics_tables.docx")  


