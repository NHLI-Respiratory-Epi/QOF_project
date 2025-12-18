# Alternate Asthma iterations

library(dplyr)
library(data.table)
library(bit64)


QOF_AST_QOF_ASTTRT <- readRDS("X:/QOF_project/Data/Asthma/cohort/3_cohort_asthma_with_AST_indicators.RDS")
QOF_AST_broad_ASTTRT <- readRDS("X:/QOF_project/Data/Asthma/cohort/5_cohort_asthma_with_AST_indicators_alt_ASTTRT_broad.RDS")
QOF_AST_ics_ASTTRT <- readRDS("X:/QOF_project/Data/Asthma/cohort/6_cohort_asthma_with_AST_indicators_alt_ASTTRT_ics.RDS")

alt_AST_QOF_ASTTRT <- readRDS("X:/QOF_project/Data/Asthma/cohort/8_cohort_asthma_with_AST_indicators_alt_AST_DAT.RDS")
alt_AST_broad_ASTTRT <- readRDS("X:/QOF_project/Data/Asthma/cohort/9_cohort_asthma_with_AST_indicators_alt_AST_DAT_alt_ASTTRT_broad.RDS")
alt_AST_ics_ASTTRT <- readRDS("X:/QOF_project/Data/Asthma/cohort/10_cohort_asthma_with_AST_indicators_alt_AST_DAT_alt_ASTTRT_ics.RDS")

QOF_AST_QOF_ASTTRT$asthma_definition <- "QOF asthma codes and QOF treatment codes"
QOF_AST_broad_ASTTRT$asthma_definition <- "QOF asthma codes, broad JKQ treatment codes"
QOF_AST_ics_ASTTRT$asthma_definition <- "QOF asthma codes, ICS JKQ treatment codes"
alt_AST_QOF_ASTTRT$asthma_definition <- "Condensed asthma codes and QOF treatment codes"
alt_AST_broad_ASTTRT$asthma_definition <- "Condensed asthma codes, broad JKQ treatment codes"
alt_AST_ics_ASTTRT$asthma_definition <- "Condensed asthma codes, ICS JKQ treatment codes"


ASTsummaryFunction <- function(x) {

  x <- x %>% group_by(asthma_definition) %>%
    summarise(AST_REG_N = sum(AST_REG),
              AST007_nume_n = sum(AST007_nume),
              AST007_denom_N = sum(AST007_denom),
              AST007_perc_meeting_QI = round((AST007_nume_n/AST007_denom_N)*100, 1),
              AST008_nume_n = sum(AST008_nume),
              AST008_denom_N = sum(AST008_denom),
              AST008_perc_meeting_QI = round((AST008_nume_n/AST008_denom_N)*100, 1),
              AST012_nume_n = sum(AST012_nume),
              AST012_denom_N = sum(AST012_denom),
              AST012_perc_meeting_QI = round((AST012_nume_n/AST012_denom_N)*100, 1))
  return(x)
  
}

QOF_AST_QOF_ASTTRT_summ <- ASTsummaryFunction(QOF_AST_QOF_ASTTRT)
QOF_AST_broad_ASTTRT_summ <- ASTsummaryFunction(QOF_AST_broad_ASTTRT)
QOF_AST_ics_ASTTRT_summ <- ASTsummaryFunction(QOF_AST_ics_ASTTRT)
alt_AST_QOF_ASTTRT_summ <- ASTsummaryFunction(alt_AST_QOF_ASTTRT)
alt_AST_broad_ASTTRT_summ <- ASTsummaryFunction(alt_AST_broad_ASTTRT)
alt_AST_ics_ASTTRT_summ <- ASTsummaryFunction(alt_AST_ics_ASTTRT)

all <- bind_rows(QOF_AST_QOF_ASTTRT_summ,
                 QOF_AST_broad_ASTTRT_summ,
                 QOF_AST_ics_ASTTRT_summ,
                 alt_AST_QOF_ASTTRT_summ,
                 alt_AST_broad_ASTTRT_summ,
                 alt_AST_ics_ASTTRT_summ)
                 

write.csv(all, "X:/QOF_project/Alex/Output/asthma_definition_table_rough.csv",
          row.names = FALSE)


all_rounded <- all %>% mutate_at(vars(AST_REG_N, 
                                       AST007_nume_n, AST007_denom_N, 
                                       AST008_nume_n, AST008_denom_N, 
                                       AST012_nume_n, AST012_denom_N),
                                 ~round(., -2))
                                       
write.csv(all_rounded, "X:/QOF_project/Alex/Output/asthma_definition_table_rounded_rough.csv",
          row.names = FALSE)


