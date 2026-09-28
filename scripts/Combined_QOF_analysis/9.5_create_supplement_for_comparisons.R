
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


AST_REG_QA_QT_vs_AA_BT <- readRDS("Z:/Group_work/QOF_project/Alex/Output/Asthma/supp_flex_tables/AST_REG_QA_QT_vs_AA_BT_table_patient_characteristics.RDS")
AST_REG_QA_QT_vs_AA_IT <- readRDS("Z:/Group_work/QOF_project/Alex/Output/Asthma/supp_flex_tables/AST_REG_QA_QT_vs_AA_IT_table_patient_characteristics.RDS")
AST_REG_QA_QT_vs_AA_QT <- readRDS("Z:/Group_work/QOF_project/Alex/Output/Asthma/supp_flex_tables/AST_REG_QA_QT_vs_AA_QT_table_patient_characteristics.RDS")
AST_REG_QA_QT_vs_QA_IT <- readRDS("Z:/Group_work/QOF_project/Alex/Output/Asthma/supp_flex_tables/AST_REG_QA_QT_vs_QA_IT_table_patient_characteristics.RDS")
COPD_REG_QC_vs_UC_RP <- readRDS("Z:/Group_work/QOF_project/Alex/Output/COPD/supp_flex_tables/COPD_REG_QC_vs_UC_RP_table_patient_characteristics.RDS")
COPD_REG_QC_vs_UC <- readRDS("Z:/Group_work/QOF_project/Alex/Output/COPD/supp_flex_tables/COPD_REG_QC_vs_UC_table_patient_characteristics.RDS")
COPD_REG_QC_vs_UC_VP <- readRDS("Z:/Group_work/QOF_project/Alex/Output/COPD/supp_flex_tables/COPD_REG_QC_vs_UC_VP_table_patient_characteristics.RDS")
COPD_REG_QC_vs_VC_RP <- readRDS("Z:/Group_work/QOF_project/Alex/Output/COPD/supp_flex_tables/COPD_REG_QC_vs_VC_RP_table_patient_characteristics.RDS")
COPD_REG_QC_vs_VC <- readRDS("Z:/Group_work/QOF_project/Alex/Output/COPD/supp_flex_tables/COPD_REG_QC_vs_VC_table_patient_characteristics.RDS")
COPD_REG_QC_vs_VC_VP <- readRDS("Z:/Group_work/QOF_project/Alex/Output/COPD/supp_flex_tables/COPD_REG_QC_vs_VC_VP_table_patient_characteristics.RDS")
HYP_REG_QC_QR_vs_UC_QR <- readRDS("Z:/Group_work/QOF_project/Alex/Output/Hypertension/HYP_REG_QC_QR_vs_UC_QR_table_patient_characteristics_flextable.RDS")



doc <- read_docx() %>% #  body_end_section_landscape() %>%
  body_add_par("Supplemental table 15. Comparing the patients characteristics of those selected into QOF 2025-26 only versus those selected into post-review scenario only versus those selected into both scenarios.") %>%
  body_add_flextable(AST_REG_QA_QT_vs_AA_BT) %>%
  body_add_par("\n\n") %>% 
  body_add_par("Supplemental table 16. Comparing the patients characteristics of those selected into QOF 2025-26 only versus those selected into alternate scenario 1 only (post-review asthma codes; treatment codes restricted to just ICS) versus those selected into both scenarios.") %>%
  body_add_flextable(AST_REG_QA_QT_vs_AA_IT) %>%
  body_add_par("\n\n")
  


print(doc, target = "Z:/Group_work/QOF_project/Alex/Output/combined/test.docx")  
