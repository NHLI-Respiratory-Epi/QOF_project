
library(data.table)
library(dplyr)

refsets <- fread("Z:/Group_work/QOF_project/QOF_codes_and_rules/PCD_Refset_Content_text_files/20250912_PCD_Refset_Content_by_output.txt")

table(refsets$Output_ID)
table(refsets$Cluster_ID)

relevant <- refsets %>%
  filter(Output_ID %in% c("AST_REG", "AST007", "AST008", "AST012", # asthma QOF codes
                          "COPD_REG",  "COPD009MI2",  "COPD009MI3", "COPD010", "COPD014", # COPD codes
                          "HYP_REG", "HYP008", "HYP009", # hypertension codes
                          "SMOK002", "SMOK004", "SMOK005", "SMOK1_REG") | # smoking codes 
           Cluster_ID %in% c("OSTR_COD", "HSTRK_COD", "DMTYPE2_COD", "FHYP_COD")) %>%  #LD added Cluster_IDs needed for covars
  rename(PCD_Refset_ID = `PCD Refset ID`,
         snomedctconceptid = SNOMED_code) 

table(relevant$Output_ID)   
table(relevant$Cluster_ID) 

saveRDS(relevant, "X:/QOF_project/Hannah_Leah/codelists/all/ast_copd_hyp_smok_andmore.RDS")

