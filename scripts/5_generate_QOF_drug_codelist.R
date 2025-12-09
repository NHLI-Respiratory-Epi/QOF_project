
library(dplyr)
library(data.table)
library(bit64)

codes <- readRDS("codelists/all/ast_copd_hyp_smok.RDS")
refs <- fread("Z:/Group_work/QOF_project/QOF_codes_and_rules/GPData_Cluster_Refset_1000230_20250904.csv")

drugcodes <- refs %>% filter(Cluster_ID %in% c("ASTTRT_COD", "PHARMDRUG_COD")) %>% filter(Active_in_Refset == 1)
drugcodes <- drugcodes %>% dplyr::select(Cluster_ID, Cluster_Description = Cluster_Desc, snomedctconceptid = ConceptId, SNOMED_code_description = ConceptId_Description) 

saveRDS(drugcodes, "codelists/all/drugscodes.RDS")


