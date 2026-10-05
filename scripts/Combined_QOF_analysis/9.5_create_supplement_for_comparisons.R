
library(dplyr)
library(data.table)
library(bit64)
library(finalfit)
library(officer)
library(flextable)
library(ggplot2)
library(stringr)

ps_land <- prop_section(
  page_size = page_size(orient = "landscape"),
  type = "continuous"
)

# cat(c(list.files("Z:/Group_work/QOF_project/Alex/Output/Asthma/supp_flex_tables/", full.names = TRUE),
#   list.files("Z:/Group_work/QOF_project/Alex/Output/COPD/supp_flex_tables/", full.names = TRUE),
#   list.files("Z:/Group_work/QOF_project/Alex/Output/Hypertension/", full.names = TRUE)))


AST_REG_QA_QT_vs_AA_BT <- readRDS("X:/QOF_project/Alex/Output/Asthma/supp_flex_tables/AST_REG_QA_QT_vs_AA_BT_table_patient_characteristics.RDS")
AST_REG_QA_QT_vs_AA_IT <- readRDS("X:/QOF_project/Alex/Output/Asthma/supp_flex_tables/AST_REG_QA_QT_vs_AA_IT_table_patient_characteristics.RDS")
AST_REG_QA_QT_vs_AA_QT <- readRDS("X:/QOF_project/Alex/Output/Asthma/supp_flex_tables/AST_REG_QA_QT_vs_AA_QT_table_patient_characteristics.RDS")
AST_REG_QA_QT_vs_QA_IT <- readRDS("X:/QOF_project/Alex/Output/Asthma/supp_flex_tables/AST_REG_QA_QT_vs_QA_IT_table_patient_characteristics.RDS")
COPD_REG_QC_vs_UC_RP <- readRDS("X:/QOF_project/Alex/Output/COPD/supp_flex_tables/COPD_REG_QC_vs_UC_RP_table_patient_characteristics.RDS")
COPD_REG_QC_vs_UC <- readRDS("X:/QOF_project/Alex/Output/COPD/supp_flex_tables/COPD_REG_QC_vs_UC_table_patient_characteristics.RDS")
COPD_REG_QC_vs_UC_VP <- readRDS("X:/QOF_project/Alex/Output/COPD/supp_flex_tables/COPD_REG_QC_vs_UC_VP_table_patient_characteristics.RDS")
COPD_REG_QC_vs_VC_RP <- readRDS("X:/QOF_project/Alex/Output/COPD/supp_flex_tables/COPD_REG_QC_vs_VC_RP_table_patient_characteristics.RDS")
COPD_REG_QC_vs_VC <- readRDS("X:/QOF_project/Alex/Output/COPD/supp_flex_tables/COPD_REG_QC_vs_VC_table_patient_characteristics.RDS")
COPD_REG_QC_vs_VC_VP <- readRDS("X:/QOF_project/Alex/Output/COPD/supp_flex_tables/COPD_REG_QC_vs_VC_VP_table_patient_characteristics.RDS")
HYP_REG_QC_QR_vs_UC_QR <- readRDS("X:/QOF_project/Alex/Output/Hypertension/HYP_REG_QC_QR_vs_UC_QR_table_patient_characteristics_flextable.RDS")


ps_land <- prop_section(
  page_size = page_size(orient = "landscape"),
  type = "continuous"
)


doc <- read_docx() %>% #  body_end_section_landscape() %>%
  body_add_par("Supplemental table 15. Comparing the patients characteristics of those selected into QOF 2025-26 asthma register only versus those selected into post-review scenario only versus those selected into both scenarios.") %>%
  body_add_flextable(AST_REG_QA_QT_vs_AA_BT) %>%
  body_add_par("P values for categorical data represent Chi-square test with Rao-Scott adjustment for cluster-randomised data. 
P values  for continuous variables represent Wald tests for coefficients from a linear model assessing the relationship between the continuous variable and category, using cluster-robust standard errors to account for clustering by practice.
The post-review only group have been excluded from statistical tests due to low numbers.
Cells containing values between 1-9 have been replaced with '<10' and all cells for that particular tabulation have been rounded to the nearest 10, with totals and percentages recalculated.") %>%
  body_add_par("\n\n") %>% 
  body_add_par("Supplemental table 16. Comparing the patients characteristics of those selected into QOF 2025-26 asthma register only versus those selected into alternate scenario 1 only (QOF 2025-26 asthma diagnosis codes; treatment codes restricted to just ICS) versus those selected into both scenarios.") %>%
  body_add_flextable(AST_REG_QA_QT_vs_QA_IT) %>%
  body_add_par("P values for categorical data represent Chi-square test with Rao-Scott adjustment for cluster-randomised data. 
P values  for continuous variables represent Wald tests for coefficients from a linear model assessing the relationship between the continuous variable and category, using cluster-robust standard errors to account for clustering by practice.
The post-review only group have been excluded from statistical tests due to low numbers.
Cells containing values between 1-9 have been replaced with '<10' and all cells for that particular tabulation have been rounded to the nearest 10, with totals and percentages recalculated.") %>%
  body_add_par("\n\n") %>%
  body_add_par("Supplemental table 17. Comparing the patients characteristics of those selected into QOF 2025-26 asthma register only versus those selected into alternate scenario 2 only (post-review asthma diagnosis codes; all else left constant) versus those selected into both scenarios.") %>%
  body_add_flextable(AST_REG_QA_QT_vs_AA_QT) %>%
  body_add_par("P values for categorical data represent Chi-square test with Rao-Scott adjustment for cluster-randomised data. 
P values  for continuous variables represent Wald tests for coefficients from a linear model assessing the relationship between the continuous variable and category, using cluster-robust standard errors to account for clustering by practice.
Cells containing values between 1-9 have been replaced with '<10' and all cells for that particular tabulation have been rounded to the nearest 10, with totals and percentages recalculated.") %>%
  body_add_par("\n\n") %>%
  body_add_par("Supplemental table 18. Comparing the patients characteristics of those selected into QOF 2025-26 asthma register only versus those selected into alternate scenario 1 only (post-review asthma diagnosis codes; treatment codes restricted to just ICS) versus those selected into both scenarios.") %>%
  body_add_flextable(AST_REG_QA_QT_vs_AA_IT) %>%
  body_add_par("P values for categorical data represent Chi-square test with Rao-Scott adjustment for cluster-randomised data. 
P values  for continuous variables represent Wald tests for coefficients from a linear model assessing the relationship between the continuous variable and category, using cluster-robust standard errors to account for clustering by practice.
The post-review only group have been excluded from statistical tests due to low numbers.
Cells containing values between 1-9 have been replaced with '<10' and all cells for that particular tabulation have been rounded to the nearest 10, with totals and percentages recalculated.") %>%
  body_add_par("\n\n") %>%
  body_add_par("Supplemental table 19. Comparing the patients characteristics of those selected into QOF 2025-26 COPD register only versus those selected into QOF 2026-27 only versus those selected into both scenarios.") %>%
  body_add_flextable(COPD_REG_QC_vs_VC_VP) %>%
  body_add_par("P values for categorical data represent Chi-square test with Rao-Scott adjustment for cluster-randomised data. 
P values  for continuous variables represent Wald tests for coefficients from a linear model assessing the relationship between the continuous variable and category, using cluster-robust standard errors to account for clustering by practice.
Cells containing values between 1-9 have been replaced with '<10' and all cells for that particular tabulation have been rounded to the nearest 10, with totals and percentages recalculated.") %>%
  body_add_par("\n\n") %>%
  body_add_par("Supplemental table 20. Comparing the patients characteristics of those selected into QOF 2025-26 COPD register only versus those selected into the post-review scenario only versus those selected into both scenarios.") %>%
  body_add_flextable(COPD_REG_QC_vs_UC) %>%
  body_add_par("P values for categorical data represent Chi-square test with Rao-Scott adjustment for cluster-randomised data. 
P values  for continuous variables represent Wald tests for coefficients from a linear model assessing the relationship between the continuous variable and category, using cluster-robust standard errors to account for clustering by practice.
Cells containing values between 1-9 have been replaced with '<10' and all cells for that particular tabulation have been rounded to the nearest 10, with totals and percentages recalculated.") %>%
  body_add_par("\n\n") %>%
  body_add_par("Supplemental table 21. Comparing the patients characteristics of those selected into QOF 2025-26 COPD register only versus those selected into alternate scenario 1 only (QOF 2026-27 COPD diagnosis codes, no process codes included) versus those selected into both scenarios.") %>%
  body_add_flextable(COPD_REG_QC_vs_VC) %>%
  body_add_par("P values for categorical data represent Chi-square test with Rao-Scott adjustment for cluster-randomised data. 
P values  for continuous variables represent Wald tests for coefficients from a linear model assessing the relationship between the continuous variable and category, using cluster-robust standard errors to account for clustering by practice.
Cells containing values between 1-9 have been replaced with '<10' and all cells for that particular tabulation have been rounded to the nearest 10, with totals and percentages recalculated.") %>%
  body_add_par("\n\n") %>%
  body_add_par("Supplemental table 22. Comparing the patients characteristics of those selected into QOF 2025-26 COPD register only versus those selected into alternate scenario 2 only (QOF 2026-27 COPD diagnosis codes, restricted process codes) versus those selected into both scenarios.") %>%
  body_add_flextable(COPD_REG_QC_vs_VC_RP) %>%
  body_add_par("P values for categorical data represent Chi-square test with Rao-Scott adjustment for cluster-randomised data. 
P values  for continuous variables represent Wald tests for coefficients from a linear model assessing the relationship between the continuous variable and category, using cluster-robust standard errors to account for clustering by practice.
Cells containing values between 1-9 have been replaced with '<10' and all cells for that particular tabulation have been rounded to the nearest 10, with totals and percentages recalculated.") %>%
  body_add_par("\n\n") %>%
  body_add_par("Supplemental table 23. Comparing the patients characteristics of those selected into QOF 2025-26 COPD register only versus those selected into alternate scenario 3 only (post-review COPD diagnosis codes, QOF 2026-27 process codes) versus those selected into both scenarios.") %>%
  body_add_flextable(COPD_REG_QC_vs_UC_VP) %>%
  body_add_par("P values for categorical data represent Chi-square test with Rao-Scott adjustment for cluster-randomised data. 
P values  for continuous variables represent Wald tests for coefficients from a linear model assessing the relationship between the continuous variable and category, using cluster-robust standard errors to account for clustering by practice.
Cells containing values between 1-9 have been replaced with '<10' and all cells for that particular tabulation have been rounded to the nearest 10, with totals and percentages recalculated.") %>%
  body_add_par("\n\n") %>%
  body_add_par("Supplemental table 24. Comparing the patients characteristics of those selected into QOF 2025-26 COPD register only versus those selected into alternate scenario 4 only (post-review COPD diagnosis codes, restricted process codes) versus those selected into both scenarios.") %>%
  body_add_flextable(COPD_REG_QC_vs_UC_RP) %>%
  body_add_par("P values for categorical data represent Chi-square test with Rao-Scott adjustment for cluster-randomised data. 
P values  for continuous variables represent Wald tests for coefficients from a linear model assessing the relationship between the continuous variable and category, using cluster-robust standard errors to account for clustering by practice.
Cells containing values between 1-9 have been replaced with '<10' and all cells for that particular tabulation have been rounded to the nearest 10, with totals and percentages recalculated.") %>%
  body_add_par("\n\n") %>%
  body_add_par("Supplemental table 25. Comparing the patients characteristics of those selected into QOF 2025-26 hypertension register only versus those selected into post-review register only versus those selected into both scenarios.") %>%
  body_add_flextable(HYP_REG_QC_QR_vs_UC_QR) %>%
  body_add_par("P values for categorical data represent Chi-square test with Rao-Scott adjustment for cluster-randomised data. 
P values  for continuous variables represent Wald tests for coefficients from a linear model assessing the relationship between the continuous variable and category, using cluster-robust standard errors to account for clustering by practice.
The QOF 2025-26 only group have been excluded from statistical tests due to low numbers.
Cells containing values between 1-9 have been replaced with '<10' and all cells for that particular tabulation have been rounded to the nearest 10, with totals and percentages recalculated.") %>%
  body_add_par("\n\n") %>%
  body_add_par("N.B. 2 asthma scenario comparisons not presented in this format due to low numbers outside of the 'in both' category. Smoking registers were unchanged so are not presented. Hypertension alternate scenarios 1 and 2 do not represent changes to the hypertension register, whilst alternate scenarios 3 and 4 reflect the same changes to the hypertension register as the presented post-review scenario.") %>%
  body_end_block_section(value = block_section(property = ps_land))


print(doc, target = "X:/QOF_project/Alex/Output/combined/Supplement scenario comparison section.docx")  

# AST_REG_QA_QT_vs_AA_BT <- readRDS("Z:/Group_work/QOF_project/Alex/Output/Asthma/supp_flex_tables/AST_REG_QA_QT_vs_AA_BT_table_patient_characteristics.RDS")
# AST_REG_QA_QT_vs_AA_IT <- readRDS("Z:/Group_work/QOF_project/Alex/Output/Asthma/supp_flex_tables/AST_REG_QA_QT_vs_AA_IT_table_patient_characteristics.RDS")
# AST_REG_QA_QT_vs_AA_QT <- readRDS("Z:/Group_work/QOF_project/Alex/Output/Asthma/supp_flex_tables/AST_REG_QA_QT_vs_AA_QT_table_patient_characteristics.RDS")
# AST_REG_QA_QT_vs_QA_IT <- readRDS("Z:/Group_work/QOF_project/Alex/Output/Asthma/supp_flex_tables/AST_REG_QA_QT_vs_QA_IT_table_patient_characteristics.RDS")
# COPD_REG_QC_vs_UC_RP <- readRDS("Z:/Group_work/QOF_project/Alex/Output/COPD/supp_flex_tables/COPD_REG_QC_vs_UC_RP_table_patient_characteristics.RDS")
# COPD_REG_QC_vs_UC <- readRDS("Z:/Group_work/QOF_project/Alex/Output/COPD/supp_flex_tables/COPD_REG_QC_vs_UC_table_patient_characteristics.RDS")
# COPD_REG_QC_vs_UC_VP <- readRDS("Z:/Group_work/QOF_project/Alex/Output/COPD/supp_flex_tables/COPD_REG_QC_vs_UC_VP_table_patient_characteristics.RDS")
# COPD_REG_QC_vs_VC_RP <- readRDS("Z:/Group_work/QOF_project/Alex/Output/COPD/supp_flex_tables/COPD_REG_QC_vs_VC_RP_table_patient_characteristics.RDS")
# COPD_REG_QC_vs_VC <- readRDS("Z:/Group_work/QOF_project/Alex/Output/COPD/supp_flex_tables/COPD_REG_QC_vs_VC_table_patient_characteristics.RDS")
# COPD_REG_QC_vs_VC_VP <- readRDS("Z:/Group_work/QOF_project/Alex/Output/COPD/supp_flex_tables/COPD_REG_QC_vs_VC_VP_table_patient_characteristics.RDS")
# HYP_REG_QC_QR_vs_UC_QR <- readRDS("Z:/Group_work/QOF_project/Alex/Output/Hypertension/HYP_REG_QC_QR_vs_UC_QR_table_patient_characteristics_flextable.RDS")
# 
# 
# 
# doc <- read_docx() %>% #  body_end_section_landscape() %>%
#   body_add_par("Supplemental table 15. Comparing the patients characteristics of those selected into QOF 2025-26 only versus those selected into post-review scenario only versus those selected into both scenarios.") %>%
#   body_add_flextable(AST_REG_QA_QT_vs_AA_BT) %>%
#   body_add_par("\n\n") %>% 
#   body_add_par("Supplemental table 16. Comparing the patients characteristics of those selected into QOF 2025-26 only versus those selected into alternate scenario 1 only (post-review asthma codes; treatment codes restricted to just ICS) versus those selected into both scenarios.") %>%
#   body_add_flextable(AST_REG_QA_QT_vs_AA_IT) %>%
#   body_add_par("\n\n")
#   
# 
# 
# print(doc, target = "Z:/Group_work/QOF_project/Alex/Output/combined/test.docx")  
