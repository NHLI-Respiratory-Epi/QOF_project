
library(dplyr)
library(data.table)
library(bit64)
library(finalfit)
library(officer)
library(flextable)
library(ggplot2)
library(stringr)


dat <- readRDS("Z:/Group_work/QOF_project/Data/cohort/07_finalised_cohort_with_QI_comps_added.RDS")

dat <- dat %>% filter(gender == "Female")

dat %>% select(starts_with("COPD_REG")) %>% colnames()

# "COPD_REG_QC"
# "COPD_REG_VC"
# "COPD_REG_VC_VP"
# "COPD_REG_VC_RP"                 
# "COPD_REG_UC"
# "COPD_REG_UC_VP"
# "COPD_REG_UC_RP"

COPD_QC <- dat %>% summarise(situation = "COPD QOF 2025-26",
                              pat_N = n(),
                              COPD_REG = sum(COPD_REG_QC),
                              COPD_REG_perc = sprintf("%.1f", round((sum(COPD_REG_QC)/nrow(dat))*100,1)),
                              COPD010_denom = sum(COPD010_denom_QC),
                              COPD010_num = sum(COPD010_num_QC),
                              COPD010_perc = sprintf("%.1f", round((sum(COPD010_num_QC)/sum(COPD010_denom_QC))*100,1)),
                              COPD014_denom = sum(COPD014_denom_QC),
                              COPD014_num = sum(COPD014_num_QC),
                              COPD014_perc = sprintf("%.1f", round((sum(COPD014_num_QC)/sum(COPD014_denom_QC))*100,1)))

COPD_VC <- dat %>% summarise(situation = "COPD VC",
                             pat_N = n(),
                             COPD_REG = sum(COPD_REG_VC),
                             COPD_REG_perc = sprintf("%.1f", round((sum(COPD_REG_VC)/nrow(dat))*100,1)),
                             COPD010_denom = sum(COPD010_denom_VC),
                             COPD010_num = sum(COPD010_num_VC),
                             COPD010_perc = sprintf("%.1f", round((sum(COPD010_num_VC)/sum(COPD010_denom_VC))*100,1)),
                             COPD014_denom = sum(COPD014_denom_VC),
                             COPD014_num = sum(COPD014_num_VC),
                             COPD014_perc = sprintf("%.1f", round((sum(COPD014_num_VC)/sum(COPD014_denom_VC))*100,1)))

# do COPD_VC_VP

COPD_VC_VP <- dat %>% summarise(situation = "COPD QOF 2026-27",
                                    pat_N = n(),
                              COPD_REG = sum(COPD_REG_VC_VP),
                              COPD_REG_perc = sprintf("%.1f", round((sum(COPD_REG_VC_VP)/nrow(dat))*100,1)),
                              COPD010_denom = sum(COPD010_denom_VC_VP),
                              COPD010_num = sum(COPD010_num_VC_VP),
                              COPD010_perc = sprintf("%.1f", round((sum(COPD010_num_VC_VP)/sum(COPD010_denom_VC_VP))*100,1)),
                              COPD014_denom = sum(COPD014_denom_VC_VP),
                              COPD014_num = sum(COPD014_num_VC_VP),
                              COPD014_perc = sprintf("%.1f", round((sum(COPD014_num_VC_VP)/sum(COPD014_denom_VC_VP))*100,1)))


COPD_VC_RP <- dat %>% summarise(situation = "COPD VC_RP",
                             pat_N = n(),
                             COPD_REG = sum(COPD_REG_VC_RP),
                             COPD_REG_perc = sprintf("%.1f", round((sum(COPD_REG_VC_RP)/nrow(dat))*100,1)),
                             COPD010_denom = sum(COPD010_denom_VC_RP),
                             COPD010_num = sum(COPD010_num_VC_RP),
                             COPD010_perc = sprintf("%.1f", round((sum(COPD010_num_VC_RP)/sum(COPD010_denom_VC_RP))*100,1)),
                             COPD014_denom = sum(COPD014_denom_VC_RP),
                             COPD014_num = sum(COPD014_num_VC_RP),
                             COPD014_perc = sprintf("%.1f", round((sum(COPD014_num_VC_RP)/sum(COPD014_denom_VC_RP))*100,1)))


# do COPD_UC

COPD_UC <- dat %>% summarise(situation = "COPD Post-review",
                                  pat_N = n(),
                              COPD_REG = sum(COPD_REG_UC),
                              COPD_REG_perc = sprintf("%.1f", round((sum(COPD_REG_UC)/nrow(dat))*100,1)),
                              COPD010_denom = sum(COPD010_denom_UC),
                              COPD010_num = sum(COPD010_num_UC),
                              COPD010_perc = sprintf("%.1f", round((sum(COPD010_num_UC)/sum(COPD010_denom_UC))*100,1)),
                              COPD014_denom = sum(COPD014_denom_UC),
                              COPD014_num = sum(COPD014_num_UC),
                              COPD014_perc = sprintf("%.1f", round((sum(COPD014_num_UC)/sum(COPD014_denom_UC))*100,1)))

COPD_UC_VP <- dat %>% summarise(situation = "COPD QOF UC_VP",
                             pat_N = n(),
                             COPD_REG = sum(COPD_REG_UC_VP),
                             COPD_REG_perc = sprintf("%.1f", round((sum(COPD_REG_UC_VP)/nrow(dat))*100,1)),
                             COPD010_denom = sum(COPD010_denom_UC_VP),
                             COPD010_num = sum(COPD010_num_UC_VP),
                             COPD010_perc = sprintf("%.1f", round((sum(COPD010_num_UC_VP)/sum(COPD010_denom_UC_VP))*100,1)),
                             COPD014_denom = sum(COPD014_denom_UC_VP),
                             COPD014_num = sum(COPD014_num_UC_VP),
                             COPD014_perc = sprintf("%.1f", round((sum(COPD014_num_UC_VP)/sum(COPD014_denom_UC_VP))*100,1)))

COPD_UC_RP <- dat %>% summarise(situation = "COPD QOF 2025-26",
                             pat_N = n(),
                             COPD_REG = sum(COPD_REG_UC_RP),
                             COPD_REG_perc = sprintf("%.1f", round((sum(COPD_REG_UC_RP)/nrow(dat))*100,1)),
                             COPD010_denom = sum(COPD010_denom_UC_RP),
                             COPD010_num = sum(COPD010_num_UC_RP),
                             COPD010_perc = sprintf("%.1f", round((sum(COPD010_num_UC_RP)/sum(COPD010_denom_UC_RP))*100,1)),
                             COPD014_denom = sum(COPD014_denom_UC_RP),
                             COPD014_num = sum(COPD014_num_UC_RP),
                             COPD014_perc = sprintf("%.1f", round((sum(COPD014_num_UC_RP)/sum(COPD014_denom_UC_RP))*100,1)))


COPD <- bind_rows(COPD_QC, COPD_VC, COPD_VC_VP, COPD_VC_RP, 
                  COPD_UC, COPD_UC_VP, COPD_UC_RP)


# and now do the same for asthma

dat %>% select(starts_with("AST_REG")) %>% colnames()

# "AST_REG_QA_QT"
# "AST_REG_QA_BT"
# "AST_REG_QA_IT"
# "AST_REG_AA_QT"                    
# "AST_REG_AA_BT"
# "AST_REG_AA_IT" 



asthma_QA_QT <- dat %>% summarise(situation = "Asthma QOF 2026-27",
                              pat_N = n(),
                              AST_REG = sum(AST_REG_QA_QT),
                              AST_REG_perc = sprintf("%.1f", round((sum(AST_REG_QA_QT)/nrow(dat))*100,1)),
                              AST007_denom = sum(AST007_denom_QA_QT),
                              AST007_num = sum(AST007_nume_QA_QT),
                              AST007_perc = sprintf("%.1f", round((sum(AST007_nume_QA_QT)/sum(AST007_denom_QA_QT))*100,1)),
                              AST008_denom = sum(AST008_denom_QA_QT),
                              AST008_num = sum(AST008_nume_QA_QT),
                              AST008_perc = sprintf("%.1f", round((sum(AST008_nume_QA_QT)/sum(AST008_denom_QA_QT))*100,1)),
                              AST008_denom_alt = sum(AST008_denom_QA_QT),
                              AST008_num_alt = sum(AST008_nume_alt_QA_QT),
                              AST008_perc_alt = sprintf("%.1f", round((sum(AST008_nume_alt_QA_QT)/sum(AST008_denom_QA_QT))*100,1)),
                              AST012_denom = sum(AST012_denom_QA_QT),
                              AST012_num = sum(AST012_nume_QA_QT),
                              AST012_perc = sprintf("%.1f", round((sum(AST012_nume_QA_QT)/sum(AST012_denom_QA_QT))*100,1)))

asthma_QA_BT <- dat %>% summarise(situation = "Asthma QA_BT",
                                  pat_N = n(),
                                  AST_REG = sum(AST_REG_QA_BT),
                                  AST_REG_perc = sprintf("%.1f", round((sum(AST_REG_QA_BT)/nrow(dat))*100,1)),
                                  AST007_denom = sum(AST007_denom_QA_BT),
                                  AST007_num = sum(AST007_nume_QA_BT),
                                  AST007_perc = sprintf("%.1f", round((sum(AST007_nume_QA_BT)/sum(AST007_denom_QA_BT))*100,1)),
                                  AST008_denom = sum(AST008_denom_QA_BT),
                                  AST008_num = sum(AST008_nume_QA_BT),
                                  AST008_perc = sprintf("%.1f", round((sum(AST008_nume_QA_BT)/sum(AST008_denom_QA_BT))*100,1)),
                                  AST008_denom_alt = sum(AST008_denom_QA_BT),
                                  AST008_num_alt = sum(AST008_nume_alt_QA_BT),
                                  AST008_perc_alt = sprintf("%.1f", round((sum(AST008_nume_alt_QA_BT)/sum(AST008_denom_QA_BT))*100,1)),
                                  AST012_denom = sum(AST012_denom_QA_BT),
                                  AST012_num = sum(AST012_nume_QA_BT),
                                  AST012_perc = sprintf("%.1f", round((sum(AST012_nume_QA_BT)/sum(AST012_denom_QA_BT))*100,1)))

asthma_QA_IT <- dat %>% summarise(situation = "Asthma QA_IT",
                                  pat_N = n(),
                                  AST_REG = sum(AST_REG_QA_IT),
                                  AST_REG_perc = sprintf("%.1f", round((sum(AST_REG_QA_IT)/nrow(dat))*100,1)),
                                  AST007_denom = sum(AST007_denom_QA_IT),
                                  AST007_num = sum(AST007_nume_QA_IT),
                                  AST007_perc = sprintf("%.1f", round((sum(AST007_nume_QA_IT)/sum(AST007_denom_QA_IT))*100,1)),
                                  AST008_denom = sum(AST008_denom_QA_IT),
                                  AST008_num = sum(AST008_nume_QA_IT),
                                  AST008_perc = sprintf("%.1f", round((sum(AST008_nume_QA_IT)/sum(AST008_denom_QA_IT))*100,1)),
                                  AST008_denom_alt = sum(AST008_denom_QA_IT),
                                  AST008_num_alt = sum(AST008_nume_alt_QA_IT),
                                  AST008_perc_alt = sprintf("%.1f", round((sum(AST008_nume_alt_QA_IT)/sum(AST008_denom_QA_IT))*100,1)),
                                  AST012_denom = sum(AST012_denom_QA_IT),
                                  AST012_num = sum(AST012_nume_QA_IT),
                                  AST012_perc = sprintf("%.1f", round((sum(AST012_nume_QA_IT)/sum(AST012_denom_QA_IT))*100,1)))

asthma_AA_QT <- dat %>% summarise(situation = "Asthma AA_QT",
                                  pat_N = n(),
                                  AST_REG = sum(AST_REG_AA_QT),
                                  AST_REG_perc = sprintf("%.1f", round((sum(AST_REG_AA_QT)/nrow(dat))*100,1)),
                                  AST007_denom = sum(AST007_denom_AA_QT),
                                  AST007_num = sum(AST007_nume_AA_QT),
                                  AST007_perc = sprintf("%.1f", round((sum(AST007_nume_AA_QT)/sum(AST007_denom_AA_QT))*100,1)),
                                  AST008_denom = sum(AST008_denom_AA_QT),
                                  AST008_num = sum(AST008_nume_AA_QT),
                                  AST008_perc = sprintf("%.1f", round((sum(AST008_nume_AA_QT)/sum(AST008_denom_AA_QT))*100,1)),
                                  AST008_denom_alt = sum(AST008_denom_AA_QT),
                                  AST008_num_alt = sum(AST008_nume_alt_AA_QT),
                                  AST008_perc_alt = sprintf("%.1f", round((sum(AST008_nume_alt_AA_QT)/sum(AST008_denom_AA_QT))*100,1)),
                                  AST012_denom = sum(AST012_denom_AA_QT),
                                  AST012_num = sum(AST012_nume_AA_QT),
                                  AST012_perc = sprintf("%.1f", round((sum(AST012_nume_AA_QT)/sum(AST012_denom_AA_QT))*100,1)))

asthma_AA_BT <- dat %>% summarise(situation = "Asthma post-review",
                                  pat_N = n(),
                                  AST_REG = sum(AST_REG_AA_BT),
                                  AST_REG_perc = sprintf("%.1f", round((sum(AST_REG_AA_BT)/nrow(dat))*100,1)),
                                  AST007_denom = sum(AST007_denom_AA_BT),
                                  AST007_num = sum(AST007_nume_AA_BT),
                                  AST007_perc = sprintf("%.1f", round((sum(AST007_nume_AA_BT)/sum(AST007_denom_AA_BT))*100,1)),
                                  AST008_denom = sum(AST008_denom_AA_BT),
                                  AST008_num = sum(AST008_nume_AA_BT),
                                  AST008_perc = sprintf("%.1f", round((sum(AST008_nume_AA_BT)/sum(AST008_denom_AA_BT))*100,1)),
                                  AST008_denom_alt = sum(AST008_denom_AA_BT),
                                  AST008_num_alt = sum(AST008_nume_alt_AA_BT),
                                  AST008_perc_alt = sprintf("%.1f", round((sum(AST008_nume_alt_AA_BT)/sum(AST008_denom_AA_BT))*100,1)),
                                  AST012_denom = sum(AST012_denom_AA_BT),
                                  AST012_num = sum(AST012_nume_AA_BT),
                                  AST012_perc = sprintf("%.1f", round((sum(AST012_nume_AA_BT)/sum(AST012_denom_AA_BT))*100,1)))

asthma_AA_IT <- dat %>% summarise(situation = "Asthma QOF 2026-27",
                                  pat_N = n(),
                                  AST_REG = sum(AST_REG_AA_IT),
                                  AST_REG_perc = sprintf("%.1f", round((sum(AST_REG_AA_IT)/nrow(dat))*100,1)),
                                  AST007_denom = sum(AST007_denom_AA_IT),
                                  AST007_num = sum(AST007_nume_AA_IT),
                                  AST007_perc = sprintf("%.1f", round((sum(AST007_nume_AA_IT)/sum(AST007_denom_AA_IT))*100,1)),
                                  AST008_denom = sum(AST008_denom_AA_IT),
                                  AST008_num = sum(AST008_nume_AA_IT),
                                  AST008_perc = sprintf("%.1f", round((sum(AST008_nume_AA_IT)/sum(AST008_denom_AA_IT))*100,1)),
                                  AST008_denom_alt = sum(AST008_denom_AA_IT),
                                  AST008_num_alt = sum(AST008_nume_alt_AA_IT),
                                  AST008_perc_alt = sprintf("%.1f", round((sum(AST008_nume_alt_AA_IT)/sum(AST008_denom_AA_IT))*100,1)),
                                  AST012_denom = sum(AST012_denom_AA_IT),
                                  AST012_num = sum(AST012_nume_AA_IT),
                                  AST012_perc = sprintf("%.1f", round((sum(AST012_nume_AA_IT)/sum(AST012_denom_AA_IT))*100,1)))


asthma <- bind_rows(asthma_QA_QT, asthma_QA_BT, asthma_QA_IT, asthma_AA_QT, asthma_AA_BT, asthma_AA_IT)


# smoking

smoking_QOF <- dat %>% summarise(situation = "Smoking QOF 2026-27",
                              pat_N = n(),
                              SMOK1_REG = sum(SMOK1_REG_current),
                              SMOK1_REG_perc = sprintf("%.1f", round((sum(SMOK1_REG_current)/nrow(dat))*100,1)),
                              SMOK2_REG = sum(SMOK2_REG_current),
                              SMOK2_REG_perc = sprintf("%.1f", round((sum(SMOK2_REG_current)/nrow(dat))*100,1)),
                              SMOK002_denom = sum(SMOK002_denominator_current),
                              SMOK002_num = sum(SMOK002_numerator_current),
                              SMOK002_perc = sprintf("%.1f", round((sum(SMOK002_numerator_current)/sum(SMOK002_denominator_current))*100,1)),
                              SMOK004_denom = sum(SMOK004_denominator_current),
                              SMOK004_num = sum(SMOK004_numerator_current),
                              SMOK004_perc = sprintf("%.1f", round((sum(SMOK004_numerator_current)/sum(SMOK004_denominator_current))*100,1)),
                              SMOK005_denom = sum(SMOK005_denominator_current),
                              SMOK005_num = sum(SMOK005_numerator_current),
                              SMOK005_perc = sprintf("%.1f", round((sum(SMOK005_numerator_current)/sum(SMOK005_denominator_current))*100,1)))

# and the updated

smoking_updated <- dat %>% summarise(situation = "Smoking post-review",
                                    pat_N = n(),
                              SMOK1_REG = sum(SMOK1_REG_new),
                              SMOK1_REG_perc = sprintf("%.1f", round((sum(SMOK1_REG_new)/nrow(dat))*100,1)),
                              SMOK2_REG = sum(SMOK2_REG_new),
                              SMOK2_REG_perc = sprintf("%.1f", round((sum(SMOK2_REG_new)/nrow(dat))*100,1)),
                              SMOK002_denom = sum(SMOK002_denominator_new),
                              SMOK002_num = sum(SMOK002_numerator_new),
                              SMOK002_perc = sprintf("%.1f", round((sum(SMOK002_numerator_new)/sum(SMOK002_denominator_new))*100,1)),
                              SMOK004_denom = sum(SMOK004_denominator_new),
                              SMOK004_num = sum(SMOK004_numerator_new),
                              SMOK004_perc = sprintf("%.1f", round((sum(SMOK004_numerator_new)/sum(SMOK004_denominator_new))*100,1)),
                              SMOK005_denom = sum(SMOK005_denominator_new),
                              SMOK005_num = sum(SMOK005_numerator_new),
                              SMOK005_perc = sprintf("%.1f", round((sum(SMOK005_numerator_new)/sum(SMOK005_denominator_new))*100,1)))

smoking <- bind_rows(smoking_QOF, smoking_updated)

dat %>% select(starts_with("HYP_REG")) %>% colnames()


# hypertension
# "HYP_REG_QC_QR"
# "HYP_REG_QC_NN"
# "HYP_REG_QC_VT"
# "HYP_REG_UC_QR"
# "HYP_REG_UC_NN"
# "HYP_REG_UC_VT"


HYP_QC_QR <- dat %>% summarise(situation = "Hypertension QOF 2026-27",
                              pat_N = n(),
                              HYP_REG = sum(HYP_REG_QC_QR),
                              HYP_REG_perc = sprintf("%.1f", round((sum(HYP_REG_QC_QR)/nrow(dat))*100,1)),
                              HYP008_denom = sum(HYP008_denom_QC_QR),
                              HYP008_num = sum(HYP008_nume_QC_QR),
                              HYP008_perc = sprintf("%.1f", round((sum(HYP008_nume_QC_QR)/sum(HYP008_denom_QC_QR))*100,1)),
                              HYP009_denom = sum(HYP009_denom_QC_QR),
                              HYP009_num = sum(HYP009_nume_QC_QR),
                              HYP009_perc = sprintf("%.1f", round((sum(HYP009_nume_QC_QR)/sum(HYP009_denom_QC_QR))*100,1)))

HYP_QC_NN <- dat %>% summarise(situation = "Hypertension QC_NN",
                               pat_N = n(),
                               HYP_REG = sum(HYP_REG_QC_NN),
                               HYP_REG_perc = sprintf("%.1f", round((sum(HYP_REG_QC_NN)/nrow(dat))*100,1)),
                               HYP008_denom = sum(HYP008_denom_QC_NN),
                               HYP008_num = sum(HYP008_nume_QC_NN),
                               HYP008_perc = sprintf("%.1f", round((sum(HYP008_nume_QC_NN)/sum(HYP008_denom_QC_NN))*100,1)),
                               HYP009_denom = sum(HYP009_denom_QC_NN),
                               HYP009_num = sum(HYP009_nume_QC_NN),
                               HYP009_perc = sprintf("%.1f", round((sum(HYP009_nume_QC_NN)/sum(HYP009_denom_QC_NN))*100,1)))

HYP_QC_VT <- dat %>% summarise(situation = "Hypertension QC_VT",
                               pat_N = n(),
                               HYP_REG = sum(HYP_REG_QC_VT),
                               HYP_REG_perc = sprintf("%.1f", round((sum(HYP_REG_QC_VT)/nrow(dat))*100,1)),
                               HYP008_denom = sum(HYP008_denom_QC_VT),
                               HYP008_num = sum(HYP008_nume_QC_VT),
                               HYP008_perc = sprintf("%.1f", round((sum(HYP008_nume_QC_VT)/sum(HYP008_denom_QC_VT))*100,1)),
                               HYP009_denom = sum(HYP009_denom_QC_VT),
                               HYP009_num = sum(HYP009_nume_QC_VT),
                               HYP009_perc = sprintf("%.1f", round((sum(HYP009_nume_QC_VT)/sum(HYP009_denom_QC_VT))*100,1)))

HYP_UC_QR <- dat %>% summarise(situation = "Hypertension UC_QR",
                               pat_N = n(),
                               HYP_REG = sum(HYP_REG_UC_QR),
                               HYP_REG_perc = sprintf("%.1f", round((sum(HYP_REG_UC_QR)/nrow(dat))*100,1)),
                               HYP008_denom = sum(HYP008_denom_UC_QR),
                               HYP008_num = sum(HYP008_nume_UC_QR),
                               HYP008_perc = sprintf("%.1f", round((sum(HYP008_nume_UC_QR)/sum(HYP008_denom_UC_QR))*100,1)),
                               HYP009_denom = sum(HYP009_denom_UC_QR),
                               HYP009_num = sum(HYP009_nume_UC_QR),
                               HYP009_perc = sprintf("%.1f", round((sum(HYP009_nume_UC_QR)/sum(HYP009_denom_UC_QR))*100,1)))

HYP_UC_NN <- dat %>% summarise(situation = "Hypertension UC_NN",
                               pat_N = n(),
                               HYP_REG = sum(HYP_REG_UC_NN),
                               HYP_REG_perc = sprintf("%.1f", round((sum(HYP_REG_UC_NN)/nrow(dat))*100,1)),
                               HYP008_denom = sum(HYP008_denom_UC_NN),
                               HYP008_num = sum(HYP008_nume_UC_NN),
                               HYP008_perc = sprintf("%.1f", round((sum(HYP008_nume_UC_NN)/sum(HYP008_denom_UC_NN))*100,1)),
                               HYP009_denom = sum(HYP009_denom_UC_NN),
                               HYP009_num = sum(HYP009_nume_UC_NN),
                               HYP009_perc = sprintf("%.1f", round((sum(HYP009_nume_UC_NN)/sum(HYP009_denom_UC_NN))*100,1)))

HYP_UC_VT <- dat %>% summarise(situation = "Hypertension post-review",
                               pat_N = n(),
                               HYP_REG = sum(HYP_REG_UC_VT),
                               HYP_REG_perc = sprintf("%.1f", round((sum(HYP_REG_UC_VT)/nrow(dat))*100,1)),
                               HYP008_denom = sum(HYP008_denom_UC_VT),
                               HYP008_num = sum(HYP008_nume_UC_VT),
                               HYP008_perc = sprintf("%.1f", round((sum(HYP008_nume_UC_VT)/sum(HYP008_denom_UC_VT))*100,1)),
                               HYP009_denom = sum(HYP009_denom_UC_VT),
                               HYP009_num = sum(HYP009_nume_UC_VT),
                               HYP009_perc = sprintf("%.1f", round((sum(HYP009_nume_UC_VT)/sum(HYP009_denom_UC_VT))*100,1)))



hypertension <- bind_rows(HYP_QC_QR, HYP_QC_NN, HYP_QC_VT,
                          HYP_UC_QR, HYP_UC_NN, HYP_UC_VT)

COPD
asthma
smoking
hypertension

# clean up tables

#hypertension 

hypertension <- hypertension %>% 
  mutate(HYP_REG_clean = paste0(HYP_REG, "/", pat_N, " (", HYP_REG_perc, "%)"),
         HYP008_clean = paste0(HYP008_num, "/", HYP008_denom, " (", HYP008_perc, "%)"),
         HYP009_clean = paste0(HYP009_num, "/", HYP009_denom, " (", HYP009_perc, "%)"))

# COPD 

COPD <- COPD %>% 
  mutate(COPD_REG_clean = paste0(COPD_REG, "/", pat_N, " (", COPD_REG_perc, "%)"),
         COPD010_clean = paste0(COPD010_num, "/", COPD010_denom, " (", COPD010_perc, "%)"),
         COPD014_clean = paste0(COPD014_num, "/", COPD014_denom, " (", COPD014_perc, "%)"))

# asthma

asthma <- asthma %>% 
  mutate(AST_REG_clean = paste0(AST_REG, "/", pat_N, " (", AST_REG_perc, "%)"),
         AST007_clean = paste0(AST007_num, "/", AST007_denom, " (", AST007_perc, "%)"),
         AST008_alt_clean = paste0(AST008_num_alt, "/", AST008_denom_alt, " (", AST008_perc_alt, "%)"),
         AST008_clean = paste0(AST008_num, "/", AST008_denom, " (", AST008_perc, "%)"),
         AST012_clean = paste0(AST012_num, "/", AST012_denom, " (", AST012_perc, "%)"))

# smoking

smoking <- smoking %>% 
  mutate(SMOK1_REG_clean = paste0(SMOK1_REG, "/", pat_N, " (", SMOK1_REG_perc, "%)"),
         SMOK2_REG_clean = paste0(SMOK2_REG, "/", pat_N, " (", SMOK2_REG_perc, "%)"),
         SMOK002_clean = paste0(SMOK002_num, "/", SMOK002_denom, " (", SMOK002_perc, "%)"),
         SMOK004_clean = paste0(SMOK004_num, "/", SMOK004_denom, " (", SMOK004_perc, "%)"),
         SMOK005_clean = paste0(SMOK005_num, "/", SMOK005_denom, " (", SMOK005_perc, "%)"))

COPD
asthma
smoking
hypertension



doc <- read_docx() %>% #  body_end_section_landscape() %>%
  body_add_flextable(flextable(COPD)) %>%
  body_add_par(" ") %>%
  body_add_flextable(flextable(asthma)) %>%
  body_add_par(" ") %>%
  body_add_flextable(flextable(smoking)) %>%
  body_add_par(" ") %>%
  body_add_flextable(flextable(hypertension)) %>%
  body_add_par(" ") %>%
  print(target = "Z:/Group_work/QOF_project/Alex/Output/combined/Supplement_all_scenario_tables_female.docx")
  
