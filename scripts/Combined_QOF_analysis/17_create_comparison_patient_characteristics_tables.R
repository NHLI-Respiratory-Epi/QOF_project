

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




ind_vars_characteristics_asthma <- dat %>% select(gender, region, imd_2019_5, age, eth5, BMI_cat,
                                           gord:htn_qof, baseline_asthma_attacks) %>% 
  colnames()

ind_vars_characteristics_asthma_children <- dat %>% select(gender, region, imd_2019_5, age, eth5, 
                                                           baseline_asthma_attacks) %>% 
  colnames()

ind_vars_characteristics_COPD <- dat %>% select(gender, region, imd_2019_5, age, eth5, BMI_cat,
                                           gord:htn_qof, baseline_aecopd) %>% 
  colnames()

ind_vars_characteristics_smoking <- dat %>% select(gender, region, imd_2019_5, age, eth5, BMI_cat,
                                           gord:htn_qof) %>% 
  colnames()

ind_vars_characteristics_hypertension <- dat %>% select(gender, region, imd_2019_5, age, eth5, BMI_cat,
                                           gord:frailty_qof) %>% 
  colnames()


# asthma

# patient characteristics table
AST007_inc_exc_type_QA_QT_table <- dat %>%
  filter(!is.na(AST007_inc_exc_type_QA_QT)) %>% 
  summary_factorlist_clustered(., dependent = "AST007_inc_exc_type_QA_QT", 
                     explanatory = ind_vars_characteristics_asthma,
                     p = TRUE,total_col = TRUE, add_col_totals = TRUE)


ft_AST007 <- flextable(AST007_inc_exc_type_QA_QT_table)
ft_AST007 <- fontsize(ft_AST007, size = 9, part = "all")
ft_AST007 <- width(ft_AST007, j = 1, width = 2.2)      # first/left_AST column
ft_AST007 <- width(ft_AST007, j = 2:ncol(AST007_inc_exc_type_QA_QT_table), width = 1.0)  # remaining columns
ft_AST007 <- autofit(ft_AST007)

AST008_inc_exc_type_QA_QT_table <- dat %>%
  filter(!is.na(AST008_inc_exc_type_QA_QT)) %>% 
  summary_factorlist_clustered(., dependent = "AST008_inc_exc_type_QA_QT", 
                               explanatory = ind_vars_characteristics_asthma_children,
                               p = FALSE,total_col = TRUE, add_col_totals = TRUE)


ft_AST008 <- flextable(AST008_inc_exc_type_QA_QT_table)
ft_AST008 <- fontsize(ft_AST008, size = 9, part = "all")
ft_AST008 <- width(ft_AST008, j = 1, width = 2.2)      # first/left_AST column
ft_AST008 <- width(ft_AST008, j = 2:ncol(AST008_inc_exc_type_QA_QT_table), width = 1.0)  # remaining columns
ft_AST008 <- autofit(ft_AST008)

# some issues with AST012 - region, frailty_QOF, asthma attacks

AST012_inc_exc_type_QA_QT_table <- dat %>%
  filter(!is.na(AST012_inc_exc_type_QA_QT)) %>%
  summary_factorlist_clustered(., dependent = "AST012_inc_exc_type_QA_QT", 
                     explanatory = ind_vars_characteristics_asthma,
                     p = FALSE,total_col = TRUE, add_col_totals = TRUE)

AST012_inc_exc_type_QA_QT_table$p <- dat %>%
  filter(!is.na(AST012_inc_exc_type_QA_QT)) %>%
  summary_factorlist(., dependent = "AST012_inc_exc_type_QA_QT", 
                     explanatory = ind_vars_characteristics_asthma,
                     p = TRUE,total_col = TRUE, add_col_totals = TRUE) %>%
  pull(p)

AST012_inc_exc_type_QA_QT_table_p_values <- dat %>%
  filter(!is.na(AST012_inc_exc_type_QA_QT)) %>%
  summary_factorlist_clustered(., dependent = "AST012_inc_exc_type_QA_QT", 
                               explanatory = ind_vars_characteristics_asthma[c(1, 3:15, 17)],
                               p = TRUE,total_col = TRUE, add_col_totals = TRUE)

AST012_inc_exc_type_QA_QT_table_p_values <- AST012_inc_exc_type_QA_QT_table_p_values %>%
  select(label, levels, p_clustered = p) %>% filter(levels != "1")

AST012_inc_exc_type_QA_QT_table <- left_join(AST012_inc_exc_type_QA_QT_table, AST012_inc_exc_type_QA_QT_table_p_values,
                                             by = c("label", "levels"))

AST012_inc_exc_type_QA_QT_table$p_clustered[is.na(AST012_inc_exc_type_QA_QT_table$p_clustered)] <- ""

AST012_inc_exc_type_QA_QT_table$p_clustered[AST012_inc_exc_type_QA_QT_table$p != "" & 
                                              AST012_inc_exc_type_QA_QT_table$p_clustered == ""] <- "-*"

AST012_inc_exc_type_QA_QT_table$p <- AST012_inc_exc_type_QA_QT_table$p_clustered
AST012_inc_exc_type_QA_QT_table$p_clustered <- NULL

ft_AST012 <- flextable(AST012_inc_exc_type_QA_QT_table)
ft_AST012 <- fontsize(ft_AST012, size = 9, part = "all")
ft_AST012 <- width(ft_AST012, j = 1, width = 2.2)      # first/left_AST column
ft_AST012 <- width(ft_AST012, j = 2:ncol(AST012_inc_exc_type_QA_QT_table), width = 1.0)  # remaining columns
ft_AST012 <- autofit(ft_AST012)

# COPD

# patient characteristics table
COPD010_inc_exc_type_table <- dat %>%
  filter(!is.na(COPD010_inc_exc_type)) %>% 
  summary_factorlist_clustered(., dependent = "COPD010_inc_exc_type", 
                     explanatory = ind_vars_characteristics_COPD,
                     p = TRUE,total_col = TRUE, add_col_totals = TRUE)


ft_COPD010 <- flextable(COPD010_inc_exc_type_table)
ft_COPD010 <- fontsize(ft_COPD010, size = 9, part = "all")
ft_COPD010 <- width(ft_COPD010, j = 1, width = 2.2)      # first/left_COPD010 column
ft_COPD010 <- width(ft_COPD010, j = 2:ncol(COPD010_inc_exc_type_table), width = 1.0)  # remaining columns
ft_COPD010 <- autofit(ft_COPD010)

COPD014_inc_exc_type_table <- dat %>%
  filter(!is.na(COPD014_inc_exc_type)) %>% 
  summary_factorlist_clustered(., dependent = "COPD014_inc_exc_type", 
                               explanatory = ind_vars_characteristics_COPD,
                               p = TRUE,total_col = TRUE, add_col_totals = TRUE)


ft_COPD014 <- flextable(COPD014_inc_exc_type_table)
ft_COPD014 <- fontsize(ft_COPD014, size = 9, part = "all")
ft_COPD014 <- width(ft_COPD014, j = 1, width = 2.2)      # first/left_COPD014 column
ft_COPD014 <- width(ft_COPD014, j = 2:ncol(COPD014_inc_exc_type_table), width = 1.0)  # remaining columns
ft_COPD014 <- autofit(ft_COPD014)

# Hypertension

# patient characteristics table
HYP008_inc_exc_type_table <- dat %>%
  filter(!is.na(HYP008_inc_exc_type_QC_QR)) %>% 
  summary_factorlist_clustered(., dependent = "HYP008_inc_exc_type_QC_QR", 
                     explanatory = ind_vars_characteristics_hypertension,
                     p = TRUE,total_col = TRUE, add_col_totals = TRUE)


ft_HYP008 <- flextable(HYP008_inc_exc_type_table)
ft_HYP008 <- fontsize(ft_HYP008, size = 9, part = "all")
ft_HYP008 <- width(ft_HYP008, j = 1, width = 2.2)      # first/left_HYP008 column
ft_HYP008 <- width(ft_HYP008, j = 2:ncol(HYP008_inc_exc_type_table), width = 1.0)  # remaining columns
ft_HYP008 <- autofit(ft_HYP008)

# Hypertension

# patient characteristics table
HYP009_inc_exc_type_table <- dat %>%
  filter(!is.na(HYP009_inc_exc_type_QC_QR)) %>% 
  summary_factorlist_clustered(., dependent = "HYP009_inc_exc_type_QC_QR", 
                     explanatory = ind_vars_characteristics_hypertension,
                     p = TRUE,total_col = TRUE, add_col_totals = TRUE)


ft_HYP009 <- flextable(HYP009_inc_exc_type_table)
ft_HYP009 <- fontsize(ft_HYP009, size = 9, part = "all")
ft_HYP009 <- width(ft_HYP009, j = 1, width = 2.2)      # first/left_HYP009 column
ft_HYP009 <- width(ft_HYP009, j = 2:ncol(HYP009_inc_exc_type_table), width = 1.0)  # remaining columns
ft_HYP009 <- autofit(ft_HYP009)

# Smoking

# patient characteristics table
SMOK002_inc_exc_type_table <- dat %>%
  filter(!is.na(SMOK002_inc_exc_type_current)) %>% 
  summary_factorlist_clustered(., dependent = "SMOK002_inc_exc_type_current", 
                     explanatory = ind_vars_characteristics_smoking,
                     p = TRUE,total_col = TRUE, add_col_totals = TRUE)


ft_SMOK002 <- flextable(SMOK002_inc_exc_type_table)
ft_SMOK002 <- fontsize(ft_SMOK002, size = 9, part = "all")
ft_SMOK002 <- width(ft_SMOK002, j = 1, width = 2.2)      # first/left_SMOK002 column
ft_SMOK002 <- width(ft_SMOK002, j = 2:ncol(SMOK002_inc_exc_type_table), width = 1.0)  # remaining columns
ft_SMOK002 <- autofit(ft_SMOK002)

# patient characteristics table
SMOK004_inc_exc_type_table <- dat %>%
  filter(!is.na(SMOK004_inc_exc_type_current)) %>% 
  summary_factorlist_clustered(., dependent = "SMOK004_inc_exc_type_current", 
                     explanatory = ind_vars_characteristics_smoking,
                     p = TRUE,total_col = TRUE, add_col_totals = TRUE)


ft_SMOK004 <- flextable(SMOK004_inc_exc_type_table)
ft_SMOK004 <- fontsize(ft_SMOK004, size = 9, part = "all")
ft_SMOK004 <- width(ft_SMOK004, j = 1, width = 2.2)      # first/left_SMOK004 column
ft_SMOK004 <- width(ft_SMOK004, j = 2:ncol(SMOK004_inc_exc_type_table), width = 1.0)  # remaining columns
ft_SMOK004 <- autofit(ft_SMOK004)


# patient characteristics table
SMOK005_inc_exc_type_table <- dat %>%
  filter(!is.na(SMOK005_inc_exc_type_current)) %>% 
  summary_factorlist_clustered(., dependent = "SMOK005_inc_exc_type_current", 
                               explanatory = ind_vars_characteristics_smoking,
                               p = TRUE,total_col = TRUE, add_col_totals = TRUE)


ft_SMOK005 <- flextable(SMOK005_inc_exc_type_table)
ft_SMOK005 <- fontsize(ft_SMOK005, size = 9, part = "all")
ft_SMOK005 <- width(ft_SMOK005, j = 1, width = 2.2)      # first/left_SMOK005 column
ft_SMOK005 <- width(ft_SMOK005, j = 2:ncol(SMOK005_inc_exc_type_table), width = 1.0)  # remaining columns
ft_SMOK005 <- autofit(ft_SMOK005)


# this is a bit of code that can make all the documents landscape
# if added

ps_land <- prop_section(
  page_size = page_size(orient = "landscape"),
  type = "continuous"
)

doc <- read_docx() %>% #  body_end_section_landscape() %>%
  body_add_par("Supplemental Table 27. Patient characteristics of patients rejected from the quality indicator (due to being classed as unsuitable for QI care; patient declined QI care or monitoring; patient did not respond to invites for QI care); selected into the numerator; and selected into the denominator but not numerator; for asthma QI AST007.", 
               style = "Normal") %>%
  body_add_flextable(ft_AST007) %>%
  body_add_par(" ") %>%
  body_add_par("Supplemental Table 28. Patient characteristics of patients rejected from the quality indicator (due to being classed as unsuitable for QI care; patient declined QI care or monitoring; patient did not respond to invites for QI care); selected into the numerator; and selected into the denominator but not numerator; for asthma QI AST008.", 
               style = "Normal") %>%
  body_add_flextable(ft_AST008) %>%
  body_add_par(" ") %>%
  body_add_par("Supplemental Table 29. Patient characteristics of patients rejected from the quality indicator (due to being classed as unsuitable for QI care; patient declined QI care or monitoring; patient did not respond to invites for QI care); selected into the numerator; and selected into the denominator but not numerator; for asthma QI AST012.", 
               style = "Normal") %>%
  body_add_flextable(ft_AST012) %>%
  body_add_par("*Rao-Scott adjusted Chi-squared statistical test unable to be run") %>%
  body_add_par(" ") %>%
  body_add_par("Supplemental Table 30. Patient characteristics of patients rejected from the quality indicator (due to being classed as unsuitable for QI care; patient declined QI care or monitoring; patient did not respond to invites for QI care); selected into the numerator; and selected into the denominator but not numerator; for COPD QI COPD010.", 
               style = "Normal") %>%
  body_add_flextable(ft_COPD010) %>%
  body_add_par(" ") %>%
  body_add_par("Supplemental Table 31. Patient characteristics of patients rejected from the quality indicator (due to being classed as unsuitable for QI care; patient declined QI care or monitoring; patient did not respond to invites for QI care); selected into the numerator; and selected into the denominator but not numerator; for COPD QI COPD014.", 
               style = "Normal") %>%
  body_add_flextable(ft_COPD014) %>%
  body_add_par(" ") %>%
  body_add_par("Supplemental Table 32. Patient characteristics of patients rejected from the quality indicator (due to being classed as unsuitable for QI care; patient declined QI care or monitoring; patient did not respond to invites for QI care); selected into the numerator; and selected into the denominator but not numerator; for COPD QI SMOK002.", 
               style = "Normal") %>%
  body_add_flextable(ft_SMOK002) %>%
  body_add_par(" ") %>%
  body_add_par("Supplemental Table 33. Patient characteristics of patients rejected from the quality indicator (due to being classed as unsuitable for QI care; patient declined QI care or monitoring; patient did not respond to invites for QI care); selected into the numerator; and selected into the denominator but not numerator; for COPD QI SMOK004.", 
               style = "Normal") %>%
  body_add_flextable(ft_SMOK004) %>%
  body_add_par(" ") %>%
  body_add_par("Supplemental Table 34. Patient characteristics of patients rejected from the quality indicator (due to being classed as unsuitable for QI care; patient declined QI care or monitoring; patient did not respond to invites for QI care); selected into the numerator; and selected into the denominator but not numerator; for COPD QI SMOK005.", 
               style = "Normal") %>%
  body_add_flextable(ft_SMOK005) %>%
  body_add_par(" ") %>%
  body_add_par("Supplemental Table 35. Patient characteristics of patients rejected from the quality indicator (due to being classed as unsuitable for QI care; patient declined QI care or monitoring; patient did not respond to invites for QI care); selected into the numerator; and selected into the denominator but not numerator; for COPD QI HYP008.", 
               style = "Normal") %>%
  body_add_flextable(ft_HYP008) %>%
  body_add_par(" ") %>%
  body_add_par("Supplemental Table 36. Patient characteristics of patients rejected from the quality indicator (due to being classed as unsuitable for QI care; patient declined QI care or monitoring; patient did not respond to invites for QI care); selected into the numerator; and selected into the denominator but not numerator; for COPD QI HYP009.", 
               style = "Normal") %>%
  body_add_flextable(ft_HYP009) %>%
  body_add_par(" ") %>%
  body_end_block_section(value = block_section(property = ps_land))

print(doc, target = "Z:/Group_work/QOF_project/Alex/Output/combined/inc_exc_patient_characteristics_supplement.docx")  


