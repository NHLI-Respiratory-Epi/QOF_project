
library(data.table)
library(dplyr)

refsets <- fread("X:/QOF_project/QOF_codes_and_rules/PCD_Refset_Content_text_files/20250912_PCD_Refset_Content_by_output.txt")

table(refsets$Output_ID)

relevant <- refsets %>%
  filter(Output_ID %in% c("AST_REG", "AST007", "AST008", "AST012", # asthma QOF codes
                          "COPD_REG", "COPD010", "COPD014", # COPD codes
                          "HYP_REG", "HYP008", "HYP009", # hypertension codes
                          "SMOK002", "SMOK004", "SMOK005", "SMOK1_REG")) %>% # smoking codes 
  rename(PCD_Refset_ID = `PCD Refset ID`) 

table(relevant$Output_ID)                                                

head(relevant)

relevant$area <- NA
relevant$area[relevant$Output_ID %in% c("AST_REG", "AST007", "AST008", "AST012")] <- "Asthma"
relevant$area[relevant$Output_ID %in% c("COPD_REG", "COPD010", "COPD014")] <- "COPD"
relevant$area[relevant$Output_ID %in% c("HYP_REG", "HYP008", "HYP009")] <- "Hypertension"
relevant$area[relevant$Output_ID %in% c("SMOK002", "SMOK004", "SMOK005", "SMOK1_REG")] <- "Smoking"

table(relevant$area, useNA = "ifany")

relevant %>% select(area, Cluster_ID, Cluster_Description, SNOMED_code) %>% 
  unique() %>% select(area, Cluster_ID, Cluster_Description) %>%
  arrange(area, Cluster_ID, Cluster_Description) %>% 
  group_by(area, Cluster_ID, Cluster_Description) %>% 
  summarise(code_n = n()) %>%
  write.csv("X:/QOF_project/Alex/Output/output/Code_clusters_checked.csv",
            row.names = FALSE)
