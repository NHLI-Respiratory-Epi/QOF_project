

library(dplyr)
library(data.table)
library(bit64)
library(finalfit)
library(officer)
library(flextable)
library(ggplot2)
library(stringr)


dat <- readRDS("Z:/Group_work/QOF_project/Data/cohort/07_finalised_cohort_with_QI_comps_added.RDS")

smoking_table <- dat %>% select(starts_with("SMOK")) %>% 
  select(ends_with("comparison")) %>% # lapply(table, useNA = "ifany")
  summary_factorlist(explanatory = colnames(.))

smoking_table$levels[smoking_table$levels == "0"] <- "Not in QOF 2025-26 or post-review"
smoking_table$levels[smoking_table$levels == "1"] <- "Selected into QOF 2025-26 scenario only"
smoking_table$levels[smoking_table$levels == "2"] <- "Selected into post-review scenario only"
smoking_table$levels[smoking_table$levels == "3"] <- "Selected into QOF 2025-26 and post-review scenarios" 

write.csv(smoking_table, "Z:/Group_work/QOF_project/Alex/Output/Smoking/smoking_table_comparisons_supplement_needs_suppressing.csv",
          row.names = FALSE)
