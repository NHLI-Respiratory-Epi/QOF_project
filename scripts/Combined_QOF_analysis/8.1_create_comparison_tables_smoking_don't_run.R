

library(dplyr)
library(data.table)
library(bit64)
library(finalfit)
library(officer)
library(flextable)
library(ggplot2)
library(stringr)


dat <- readRDS("Z:/Group_work/QOF_project/Data/cohort/07_finalised_cohort_with_QI_comps_added.RDS")

dat %>% select(ends_with("current")) %>% summary()

dat %>% filter(SMOK1_REG_current == 1) %>% nrow()
dat %>% filter(SMOK1_REG_current == 1) %>% nrow()

row1 <- dat %>% select(starts_with("SMOK")) %>% 
  select(ends_with("current")) %>%
  select(-contains("inc_exc")) %>%
  summarise_all(.funs = ~sum(.))

row2 <- dat %>% select(starts_with("SMOK")) %>% 
  select(ends_with("new")) %>%
  select(-contains("inc_exc")) %>%
  summarise_all(.funs = ~sum(.))

row1
row2



smoking_table <- dat %>% select(starts_with("SMOK")) %>% 
  select(ends_with("comparison")) %>% # lapply(table, useNA = "ifany")
  summary_factorlist(explanatory = colnames(.))

smoking_table$levels[smoking_table$levels == "0"] <- "Not in QOF 2025-26 or post-review"
smoking_table$levels[smoking_table$levels == "1"] <- "Selected into QOF 2025-26 scenario only"
smoking_table$levels[smoking_table$levels == "2"] <- "Selected into post-review scenario only"
smoking_table$levels[smoking_table$levels == "3"] <- "Selected into QOF 2025-26 and post-review scenarios" 

write.csv(smoking_table, "Z:/Group_work/QOF_project/Alex/Output/Smoking/smoking_table_comparisons_supplement.csv",
          row.names = FALSE)

ft <- flextable(smoking_table)
ft <- fontsize(ft, size = 9, part = "all")
ft <- width(ft, j = 1, width = 2.2)      # first/left column
ft <- width(ft, j = 2:ncol(smoking_table), width = 1.0)  # remaining columns
ft <- autofit(ft)

saveRDS(ft, "Z:/Group_work/QOF_project/Alex/Output/Smoking/smoking_table_comparisons_supplement_flextable.RDS")

