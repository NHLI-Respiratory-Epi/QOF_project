# extract asthma rules for Sankey diagram.


library(dplyr)
library(data.table)
library(bit64)
library(readstata13)

dat <- readRDS("Z:/Group_work/QOF_project/Data/Asthma/cohort/3_cohort_asthma_with_AST_indicators.RDS")

dat <- dat %>% select(patid, pracid, starts_with("AST_REG"), starts_with("AST0"))
dat <- dat %>% mutate(patid = as.character(patid), pracid = as.character(pracid))
dat <- dat %>% mutate(across(where(is.numeric), as.integer))


readstata13::save.dta13(dat, "Z:/Group_work/QOF_project/Data/Asthma/cohort/3.5_asthma_cohort_old_QOF_rules_for_Sankey_diagram.dta",
                        compress = TRUE)
