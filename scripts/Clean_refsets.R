
library(data.table)
library(dplyr)
library(readstata13)

refsets <- fread("Z:/Group_work/QOF_project/QOF_codes_and_rules/PCD_Refset_Content_text_files/20250912_PCD_Refset_Content_by_output.txt")

table(refsets$Output_ID)

relevant <- refsets %>%
  filter(Output_ID %in% c("AST_REG", "AST007", "AST008", "AST012", # asthma QOF codes
                          "COPD_REG",  "COPD009MI2",  "COPD009MI3", "COPD010", "COPD014", # COPD codes
                          "HYP_REG", "HYP008", "HYP009", # hypertension codes
                          "SMOK002", "SMOK004", "SMOK005", "SMOK1_REG")) %>% # smoking codes 
  rename(PCD_Refset_ID = `PCD Refset ID`,
         snomedctconceptid = SNOMED_code) 

table(relevant$Output_ID)                                                

saveRDS(relevant, "X:/QOF_project/Alex/QOF_project/codelists/all/ast_copd_hyp_smok.RDS")

relevant_for_dta <- relevant %>% mutate(snomedctconceptid = as.character(snomedctconceptid),
                                        PCD_Refset_ID = as.character(PCD_Refset_ID)) 
  

readstata13::save.dta13(relevant_for_dta, "X:/QOF_project/Alex/QOF_project/codelists/all/ast_copd_hyp_smok.dta")

