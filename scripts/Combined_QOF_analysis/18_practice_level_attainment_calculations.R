# look at how changes in the rules occur across practices, and how this affects the indicators and outcomes at practice level.

library(dplyr)
library(data.table)
library(bit64)
library(finalfit)
library(officer)
library(flextable)
library(ggplot2)
library(stringr)
library(tidyr)

dat <- readRDS("Z:/Group_work/QOF_project/Data/cohort/07_finalised_cohort_with_QI_comps_added.RDS")

colnames(dat)

# create the formulae values to be applied

points <- data.frame(QI = c("AST007", "AST008", "AST012", "COPD010", "COPD014",
                            "SMOK002", "SMOK004", "SMOK005", "HYP008", "HYP009"))

points$LT <- c(45, 45, 45, 50, 40, 50, 40, 56, 40, 40) #lower threshold
points$UT <- c(70, 80, 80, 90, 90, 90, 90, 96, 85, 85) # upper threshold
points$pts <- c(20, 6, 15, 9, 2, 25, 12, 25, 38, 14) # points available
points$value <- 225.49
points$listsize <- 10184 # the adjustment for size of practice

# adjustments base don prevalence too

# for smoking SMOK002 and SMOK005:

# denom + PCA for SMOK002 2024-25 / (no. practices 2024-25 * average practice size 2024-25)

15573486/(6188*9964)
# 0.2525817
# for the others we have a baseline prevalence, which is easier

# COPD: 1.89 (1.894347)
# Asthma: 6.56 (6.5555)
# hypertension: 15.23 (15.22973)

points$prev_adj <- c(6.5555, 6.5555, 6.5555,
                     1.894347, 1.894347,
                     25.25817, NA, 25.25817, # SMOK004 no adjustment
                     15.22973, 15.22973)
                     
                     
  


# we want to see how the COPD indicators change across the practices

dat <- dat %>% select(patid, pracid, 
                      starts_with("AST"), starts_with("COPD"), 
                      starts_with("HYP"), starts_with("SMOK")) %>%
               select(-ends_with("comparison")) %>%
  select(-contains("inc_exc")) %>%
  select(-asthma_plan_qof, -hyperchol_qof, -smokeref_qof, -smokecess_qof, -smokecessdrugs_qof)


colnames(dat)

# summarise for each scenario (each unique ending)

dat_practice <- dat %>% group_by(pracid) %>% 
  summarise(n = n(), across(-starts_with("patid"), ~ sum(.))) %>% 
  ungroup()

# create the proportions for each scenario

dat_practice[["HYP008_nume_UC_NN"]]/dat_practice[["n"]]

calc_pipeline <- function(x, prac_n_col, 
                          reg_string,
                          QI_string,
                          nume_string,
                          denom_string,
                          scenario_string,
                          detail_lookup) {
  # create a pipeline that starts with the summed columns for dat_practice
  # first step - calculate the percentages
  
  register_n_col <- paste0(reg_string, "_", scenario_string)
  QI_nume_col <- paste0(QI_string, "_", nume_string, "_", scenario_string)
  QI_denom_col <- paste0(QI_string, "_", denom_string, "_", scenario_string)
  
  x <- x[ , c("pracid", prac_n_col, register_n_col, QI_nume_col, QI_denom_col)]
  
  x$prev <- (x[[register_n_col]]/x[[prac_n_col]])*100
  x$QI_perc <- (x[[QI_nume_col]]/x[[QI_denom_col]])*100
  x$QI_perc[x[[QI_denom_col]] == 0] <- 0
  
  # then restrict detail lookup to the indicator of choice
  detail_lookup <- detail_lookup[detail_lookup$QI == QI_string, ]
  
  
  
  # then calculate the points
  x$available_points <- detail_lookup$pts
  x$awarded_points <- case_when(x$QI_perc >= detail_lookup$UT ~ detail_lookup$pts,
                                x$QI_perc < detail_lookup$LT  ~ 0,
                                x$QI_perc >= detail_lookup$LT & x$QI_perc < detail_lookup$UT ~
  ((x$QI_perc - detail_lookup$LT)/(detail_lookup$UT - detail_lookup$LT))*detail_lookup$pts,
  TRUE ~ NA)
  x$prac_size_adj <- x[[prac_n_col]]/detail_lookup$listsize
  x$prev_adj <- case_when(detail_lookup$QI == "SMOK004" ~ 1,
                          TRUE ~ x$prev/detail_lookup$prev_adj)
  x$reimbursement <- x$awarded_points*x$prac_size_adj*x$prev_adj*detail_lookup$value
  
  # then, rename how you like
  
  # we are including a scenario now as a column
  
  x$scenario <- scenario_string
  
  colnames(x)[3] <- reg_string 
  colnames(x)[4] <- paste0(QI_string, "_", nume_string) 
  colnames(x)[5] <- paste0(QI_string, "_", denom_string) 
  
  colnames(x)[6:12] <- paste0(colnames(x)[6:12], "_", QI_string)
  
  return(x)
}


# AST007
AST007_QA_QT <- calc_pipeline(dat_practice, prac_n_col = "n", reg_string = "AST_REG",
                       QI_string = "AST007", nume_string = "nume", denom_string = "denom", 
                       scenario_string = "QA_QT", detail_lookup = points)

AST007_QA_BT <- calc_pipeline(dat_practice, prac_n_col = "n", reg_string = "AST_REG",
                              QI_string = "AST007", nume_string = "nume", denom_string = "denom", 
                              scenario_string = "QA_BT", detail_lookup = points)

AST007_QA_IT <- calc_pipeline(dat_practice, prac_n_col = "n", reg_string = "AST_REG",
                              QI_string = "AST007", nume_string = "nume", denom_string = "denom", 
                              scenario_string = "QA_IT", detail_lookup = points)

AST007_AA_QT <- calc_pipeline(dat_practice, prac_n_col = "n", reg_string = "AST_REG",
                              QI_string = "AST007", nume_string = "nume", denom_string = "denom", 
                              scenario_string = "AA_QT", detail_lookup = points)

AST007_AA_BT <- calc_pipeline(dat_practice, prac_n_col = "n", reg_string = "AST_REG",
                              QI_string = "AST007", nume_string = "nume", denom_string = "denom", 
                              scenario_string = "AA_BT", detail_lookup = points)

AST007_AA_IT <- calc_pipeline(dat_practice, prac_n_col = "n", reg_string = "AST_REG",
                              QI_string = "AST007", nume_string = "nume", denom_string = "denom", 
                              scenario_string = "AA_IT", detail_lookup = points)

# AST008
AST008_QA_QT <- calc_pipeline(dat_practice, prac_n_col = "n", reg_string = "AST_REG",
                              QI_string = "AST008", nume_string = "nume", denom_string = "denom", 
                              scenario_string = "QA_QT", detail_lookup = points)

AST008_QA_BT <- calc_pipeline(dat_practice, prac_n_col = "n", reg_string = "AST_REG",
                              QI_string = "AST008", nume_string = "nume_alt", denom_string = "denom", 
                              scenario_string = "QA_BT", detail_lookup = points)

AST008_QA_IT <- calc_pipeline(dat_practice, prac_n_col = "n", reg_string = "AST_REG",
                              QI_string = "AST008", nume_string = "nume_alt", denom_string = "denom", 
                              scenario_string = "QA_IT", detail_lookup = points)

AST008_AA_QT <- calc_pipeline(dat_practice, prac_n_col = "n", reg_string = "AST_REG",
                              QI_string = "AST008", nume_string = "nume_alt", denom_string = "denom", 
                              scenario_string = "AA_QT", detail_lookup = points)

AST008_AA_BT <- calc_pipeline(dat_practice, prac_n_col = "n", reg_string = "AST_REG",
                              QI_string = "AST008", nume_string = "nume_alt", denom_string = "denom", 
                              scenario_string = "AA_BT", detail_lookup = points)

AST008_AA_IT <- calc_pipeline(dat_practice, prac_n_col = "n", reg_string = "AST_REG",
                              QI_string = "AST008", nume_string = "nume_alt", denom_string = "denom", 
                              scenario_string = "AA_IT", detail_lookup = points)

# AST012
AST012_QA_QT <- calc_pipeline(dat_practice, prac_n_col = "n", reg_string = "AST_REG",
                              QI_string = "AST012", nume_string = "nume", denom_string = "denom", 
                              scenario_string = "QA_QT", detail_lookup = points)

AST012_QA_BT <- calc_pipeline(dat_practice, prac_n_col = "n", reg_string = "AST_REG",
                              QI_string = "AST012", nume_string = "nume", denom_string = "denom", 
                              scenario_string = "QA_BT", detail_lookup = points)

AST012_QA_IT <- calc_pipeline(dat_practice, prac_n_col = "n", reg_string = "AST_REG",
                              QI_string = "AST012", nume_string = "nume", denom_string = "denom", 
                              scenario_string = "QA_IT", detail_lookup = points)

AST012_AA_QT <- calc_pipeline(dat_practice, prac_n_col = "n", reg_string = "AST_REG",
                              QI_string = "AST012", nume_string = "nume", denom_string = "denom", 
                              scenario_string = "AA_QT", detail_lookup = points)

AST012_AA_BT <- calc_pipeline(dat_practice, prac_n_col = "n", reg_string = "AST_REG",
                              QI_string = "AST012", nume_string = "nume", denom_string = "denom", 
                              scenario_string = "AA_BT", detail_lookup = points)

AST012_AA_IT <- calc_pipeline(dat_practice, prac_n_col = "n", reg_string = "AST_REG",
                              QI_string = "AST012", nume_string = "nume", denom_string = "denom", 
                              scenario_string = "AA_IT", detail_lookup = points)


# COPD010

dat_practice %>% select(contains("COPD")) %>% colnames()

COPD010_QC <- calc_pipeline(dat_practice, prac_n_col = "n", reg_string = "COPD_REG",
                              QI_string = "COPD010", nume_string = "num", denom_string = "denom", 
                              scenario_string = "QC", detail_lookup = points)

COPD010_VC <- calc_pipeline(dat_practice, prac_n_col = "n", reg_string = "COPD_REG",
                            QI_string = "COPD010", nume_string = "num", denom_string = "denom", 
                            scenario_string = "VC", detail_lookup = points)

COPD010_VC_VP <- calc_pipeline(dat_practice, prac_n_col = "n", reg_string = "COPD_REG",
                            QI_string = "COPD010", nume_string = "num", denom_string = "denom", 
                            scenario_string = "VC_VP", detail_lookup = points)

COPD010_VC_RP <- calc_pipeline(dat_practice, prac_n_col = "n", reg_string = "COPD_REG",
                            QI_string = "COPD010", nume_string = "num", denom_string = "denom", 
                            scenario_string = "VC_RP", detail_lookup = points)


COPD010_UC <- calc_pipeline(dat_practice, prac_n_col = "n", reg_string = "COPD_REG",
                            QI_string = "COPD010", nume_string = "num", denom_string = "denom", 
                            scenario_string = "UC", detail_lookup = points)

COPD010_UC_VP <- calc_pipeline(dat_practice, prac_n_col = "n", reg_string = "COPD_REG",
                               QI_string = "COPD010", nume_string = "num", denom_string = "denom", 
                               scenario_string = "UC_VP", detail_lookup = points)

COPD010_UC_RP <- calc_pipeline(dat_practice, prac_n_col = "n", reg_string = "COPD_REG",
                               QI_string = "COPD010", nume_string = "num", denom_string = "denom", 
                               scenario_string = "UC_RP", detail_lookup = points)

# COPD014

COPD014_QC <- calc_pipeline(dat_practice, prac_n_col = "n", reg_string = "COPD_REG",
                            QI_string = "COPD014", nume_string = "num", denom_string = "denom", 
                            scenario_string = "QC", detail_lookup = points)

COPD014_VC <- calc_pipeline(dat_practice, prac_n_col = "n", reg_string = "COPD_REG",
                            QI_string = "COPD014", nume_string = "num", denom_string = "denom", 
                            scenario_string = "VC", detail_lookup = points)

COPD014_VC_VP <- calc_pipeline(dat_practice, prac_n_col = "n", reg_string = "COPD_REG",
                               QI_string = "COPD014", nume_string = "num", denom_string = "denom", 
                               scenario_string = "VC_VP", detail_lookup = points)

COPD014_VC_RP <- calc_pipeline(dat_practice, prac_n_col = "n", reg_string = "COPD_REG",
                               QI_string = "COPD014", nume_string = "num", denom_string = "denom", 
                               scenario_string = "VC_RP", detail_lookup = points)


COPD014_UC <- calc_pipeline(dat_practice, prac_n_col = "n", reg_string = "COPD_REG",
                            QI_string = "COPD014", nume_string = "num", denom_string = "denom", 
                            scenario_string = "UC", detail_lookup = points)

COPD014_UC_VP <- calc_pipeline(dat_practice, prac_n_col = "n", reg_string = "COPD_REG",
                               QI_string = "COPD014", nume_string = "num", denom_string = "denom", 
                               scenario_string = "UC_VP", detail_lookup = points)

COPD014_UC_RP <- calc_pipeline(dat_practice, prac_n_col = "n", reg_string = "COPD_REG",
                               QI_string = "COPD014", nume_string = "num", denom_string = "denom", 
                               scenario_string = "UC_RP", detail_lookup = points)

# SMOK002

SMOK002_current <- calc_pipeline(dat_practice, prac_n_col = "n", reg_string = "SMOK1_REG",
                               QI_string = "SMOK002", 
                               nume_string = "numerator", denom_string = "denominator", 
                               scenario_string = "current", detail_lookup = points)

SMOK002_new <- calc_pipeline(dat_practice, prac_n_col = "n", reg_string = "SMOK1_REG",
                                 QI_string = "SMOK002", 
                                 nume_string = "numerator", denom_string = "denominator", 
                                 scenario_string = "new", detail_lookup = points)

SMOK004_current <- calc_pipeline(dat_practice, prac_n_col = "n", reg_string = "SMOK2_REG",
                                 QI_string = "SMOK004", 
                                 nume_string = "numerator", denom_string = "denominator", 
                                 scenario_string = "current", detail_lookup = points)

SMOK004_new <- calc_pipeline(dat_practice, prac_n_col = "n", reg_string = "SMOK2_REG",
                             QI_string = "SMOK004", 
                             nume_string = "numerator", denom_string = "denominator", 
                             scenario_string = "new", detail_lookup = points)

SMOK005_current <- calc_pipeline(dat_practice, prac_n_col = "n", reg_string = "SMOK1_REG",
                                 QI_string = "SMOK005", 
                                 nume_string = "numerator", denom_string = "denominator", 
                                 scenario_string = "current", detail_lookup = points)

SMOK005_new <- calc_pipeline(dat_practice, prac_n_col = "n", reg_string = "SMOK1_REG",
                             QI_string = "SMOK005", 
                             nume_string = "numerator", denom_string = "denominator", 
                             scenario_string = "new", detail_lookup = points)

# HYP008

HYP008_QC_QR <- calc_pipeline(dat_practice, prac_n_col = "n", reg_string = "HYP_REG",
                             QI_string = "HYP008", 
                             nume_string = "nume", denom_string = "denom", 
                             scenario_string = "QC_QR", detail_lookup = points)

HYP008_QC_NN <- calc_pipeline(dat_practice, prac_n_col = "n", reg_string = "HYP_REG",
                              QI_string = "HYP008", 
                              nume_string = "nume", denom_string = "denom", 
                              scenario_string = "QC_NN", detail_lookup = points)

HYP008_QC_VT <- calc_pipeline(dat_practice, prac_n_col = "n", reg_string = "HYP_REG",
                              QI_string = "HYP008", 
                              nume_string = "nume", denom_string = "denom", 
                              scenario_string = "QC_VT", detail_lookup = points)

HYP008_UC_QR <- calc_pipeline(dat_practice, prac_n_col = "n", reg_string = "HYP_REG",
                              QI_string = "HYP008", 
                              nume_string = "nume", denom_string = "denom", 
                              scenario_string = "UC_QR", detail_lookup = points)

HYP008_UC_NN <- calc_pipeline(dat_practice, prac_n_col = "n", reg_string = "HYP_REG",
                              QI_string = "HYP008", 
                              nume_string = "nume", denom_string = "denom", 
                              scenario_string = "UC_NN", detail_lookup = points)

HYP008_UC_VT <- calc_pipeline(dat_practice, prac_n_col = "n", reg_string = "HYP_REG",
                              QI_string = "HYP008", 
                              nume_string = "nume", denom_string = "denom", 
                              scenario_string = "UC_VT", detail_lookup = points)

# HYP009

HYP009_QC_QR <- calc_pipeline(dat_practice, prac_n_col = "n", reg_string = "HYP_REG",
                              QI_string = "HYP009", 
                              nume_string = "nume", denom_string = "denom", 
                              scenario_string = "QC_QR", detail_lookup = points)

HYP009_QC_NN <- calc_pipeline(dat_practice, prac_n_col = "n", reg_string = "HYP_REG",
                              QI_string = "HYP009", 
                              nume_string = "nume", denom_string = "denom", 
                              scenario_string = "QC_NN", detail_lookup = points)

HYP009_QC_VT <- calc_pipeline(dat_practice, prac_n_col = "n", reg_string = "HYP_REG",
                              QI_string = "HYP009", 
                              nume_string = "nume", denom_string = "denom", 
                              scenario_string = "QC_VT", detail_lookup = points)

HYP009_UC_QR <- calc_pipeline(dat_practice, prac_n_col = "n", reg_string = "HYP_REG",
                              QI_string = "HYP009", 
                              nume_string = "nume", denom_string = "denom", 
                              scenario_string = "UC_QR", detail_lookup = points)

HYP009_UC_NN <- calc_pipeline(dat_practice, prac_n_col = "n", reg_string = "HYP_REG",
                              QI_string = "HYP009", 
                              nume_string = "nume", denom_string = "denom", 
                              scenario_string = "UC_NN", detail_lookup = points)

HYP009_UC_VT <- calc_pipeline(dat_practice, prac_n_col = "n", reg_string = "HYP_REG",
                              QI_string = "HYP009", 
                              nume_string = "nume", denom_string = "denom", 
                              scenario_string = "UC_VT", detail_lookup = points)


# now we create a big table - what do we want to show?

# domain
## QI 1 att (median (IQR)); QI 1 points (median (IQR)); QI 1 reimburse (median (IQR)); reimburse diff (median (IQR))

# join them up by scenario

# Asthma

colnames(AST007_QA_QT)

AST_QA_QT <- full_join(AST007_QA_QT, AST008_QA_QT, 
                       by = c("pracid", "n", "AST_REG", "scenario"))  %>%
             full_join(., AST012_QA_QT, by = c("pracid", "n", "AST_REG", "scenario"))  

AST_QA_QT_diff <- AST_QA_QT %>% select(pracid, 
                                       reimbursement_AST007_QA_QT = reimbursement_AST007,
                                       reimbursement_AST008_QA_QT = reimbursement_AST008,
                                       reimbursement_AST012_QA_QT = reimbursement_AST012)


AST_QA_BT <- full_join(AST007_QA_BT, AST008_QA_BT,
                       by = c("pracid", "n", "AST_REG", "scenario"))  %>%
  full_join(., AST012_QA_BT, by = c("pracid", "n", "AST_REG", "scenario"))  %>%
  full_join(., AST_QA_QT_diff, by = "pracid") %>%
  mutate(AST007_diff = reimbursement_AST007 - reimbursement_AST007_QA_QT,
         AST008_diff = reimbursement_AST008 - reimbursement_AST008_QA_QT,
         AST012_diff = reimbursement_AST012 - reimbursement_AST012_QA_QT)
         
AST_QA_IT <- full_join(AST007_QA_IT, AST008_QA_IT,
                       by = c("pracid", "n", "AST_REG", "scenario"))  %>%
  full_join(., AST012_QA_IT, by = c("pracid", "n", "AST_REG", "scenario"))  %>%
  full_join(., AST_QA_QT_diff, by = "pracid") %>%
  mutate(AST007_diff = reimbursement_AST007 - reimbursement_AST007_QA_QT,
         AST008_diff = reimbursement_AST008 - reimbursement_AST008_QA_QT,
         AST012_diff = reimbursement_AST012 - reimbursement_AST012_QA_QT)


AST_AA_QT <- full_join(AST007_AA_QT, AST008_AA_QT,
                       by = c("pracid", "n", "AST_REG", "scenario"))  %>%
  full_join(., AST012_AA_QT, by = c("pracid", "n", "AST_REG", "scenario"))  %>%
  full_join(., AST_QA_QT_diff, by = "pracid") %>%
  mutate(AST007_diff = reimbursement_AST007 - reimbursement_AST007_QA_QT,
         AST008_diff = reimbursement_AST008 - reimbursement_AST008_QA_QT,
         AST012_diff = reimbursement_AST012 - reimbursement_AST012_QA_QT)

AST_AA_BT <- full_join(AST007_AA_BT, AST008_AA_BT,
                       by = c("pracid", "n", "AST_REG", "scenario"))  %>%
  full_join(., AST012_AA_BT, by = c("pracid", "n", "AST_REG", "scenario"))  %>%
  full_join(., AST_QA_QT_diff, by = "pracid") %>%
  mutate(AST007_diff = reimbursement_AST007 - reimbursement_AST007_QA_QT,
         AST008_diff = reimbursement_AST008 - reimbursement_AST008_QA_QT,
         AST012_diff = reimbursement_AST012 - reimbursement_AST012_QA_QT)

AST_AA_IT <- full_join(AST007_AA_IT, AST008_AA_IT,
                       by = c("pracid", "n", "AST_REG", "scenario"))  %>%
  full_join(., AST012_AA_IT, by = c("pracid", "n", "AST_REG", "scenario"))  %>%
  full_join(., AST_QA_QT_diff, by = "pracid") %>%
  mutate(AST007_diff = reimbursement_AST007 - reimbursement_AST007_QA_QT,
         AST008_diff = reimbursement_AST008 - reimbursement_AST008_QA_QT,
         AST012_diff = reimbursement_AST012 - reimbursement_AST012_QA_QT)

AST <- bind_rows(AST_QA_QT, AST_QA_BT, AST_QA_IT,
                 AST_AA_QT, AST_AA_BT, AST_AA_IT)



colnames(AST_QA_QT)
nrow(AST_QA_QT)

# put it all together (asthma)
AST_all <- AST %>% 
  mutate(scenario = factor(scenario, levels = c("QA_QT", "QA_BT", "QA_IT",
                                                "AA_QT", "AA_BT", "AA_IT"))) %>%
           group_by(scenario) %>% 
    summarise(
      across(c(QI_perc_AST007, awarded_points_AST007, reimbursement_AST007, AST007_diff,
               QI_perc_AST008, awarded_points_AST008, reimbursement_AST008, AST008_diff,
               QI_perc_AST012, awarded_points_AST012, reimbursement_AST012, AST012_diff),
             list(
      median = ~ round(median(.x, na.rm = TRUE),1),
      lower_quartile = ~ round(quantile(.x, 0.25, na.rm = TRUE),1),
      upper_quartile = ~ round(quantile(.x, 0.75, na.rm = TRUE),1)
    ),
    .names = "{.col}_{.fn}")
    )
    
# copd


COPD_QC <- full_join(COPD010_QC, COPD014_QC, 
                       by = c("pracid", "n", "COPD_REG", "scenario"))

COPD_QC_diff <- COPD_QC %>% select(pracid, 
                                       reimbursement_COPD010_QC = reimbursement_COPD010,
                                       reimbursement_COPD014_QC = reimbursement_COPD014)


COPD_VC <- full_join(COPD010_VC, COPD014_VC,
                       by = c("pracid", "n", "COPD_REG", "scenario"))  %>%
  full_join(., COPD_QC_diff, by = "pracid") %>%
  mutate(COPD010_diff = reimbursement_COPD010 - reimbursement_COPD010_QC,
         COPD014_diff = reimbursement_COPD014 - reimbursement_COPD014_QC)

COPD_VC_VP <- full_join(COPD010_VC_VP, COPD014_VC_VP,
                     by = c("pracid", "n", "COPD_REG", "scenario"))  %>%
  full_join(., COPD_QC_diff, by = "pracid") %>%
  mutate(COPD010_diff = reimbursement_COPD010 - reimbursement_COPD010_QC,
         COPD014_diff = reimbursement_COPD014 - reimbursement_COPD014_QC)

COPD_VC_RP <- full_join(COPD010_VC_RP, COPD014_VC_RP,
                     by = c("pracid", "n", "COPD_REG", "scenario"))  %>%
  full_join(., COPD_QC_diff, by = "pracid") %>%
  mutate(COPD010_diff = reimbursement_COPD010 - reimbursement_COPD010_QC,
         COPD014_diff = reimbursement_COPD014 - reimbursement_COPD014_QC)

COPD_UC <- full_join(COPD010_UC, COPD014_UC,
                     by = c("pracid", "n", "COPD_REG", "scenario"))  %>%
  full_join(., COPD_QC_diff, by = "pracid") %>%
  mutate(COPD010_diff = reimbursement_COPD010 - reimbursement_COPD010_QC,
         COPD014_diff = reimbursement_COPD014 - reimbursement_COPD014_QC)

COPD_UC_VP <- full_join(COPD010_UC_VP, COPD014_UC_VP,
                     by = c("pracid", "n", "COPD_REG", "scenario"))  %>%
  full_join(., COPD_QC_diff, by = "pracid") %>%
  mutate(COPD010_diff = reimbursement_COPD010 - reimbursement_COPD010_QC,
         COPD014_diff = reimbursement_COPD014 - reimbursement_COPD014_QC)

COPD_UC_RP <- full_join(COPD010_UC_RP, COPD014_UC_RP,
                     by = c("pracid", "n", "COPD_REG", "scenario"))  %>%
  full_join(., COPD_QC_diff, by = "pracid") %>%
  mutate(COPD010_diff = reimbursement_COPD010 - reimbursement_COPD010_QC,
         COPD014_diff = reimbursement_COPD014 - reimbursement_COPD014_QC)

COPD <- bind_rows(COPD_QC, COPD_VC, COPD_VC_VP, COPD_VC_RP,
                  COPD_UC, COPD_UC_VP, COPD_UC_RP)
# put it all together (COPD)

COPD_all <- COPD %>%  
  mutate(scenario = factor(scenario, levels = c("QC", "VC", "VC_VP",
                                                "VC_RP", "UC", "UC_VP", "UC_RP"))) %>%
  group_by(scenario) %>% 
  summarise(
    across(c(QI_perc_COPD010, awarded_points_COPD010, reimbursement_COPD010, COPD010_diff,
             QI_perc_COPD014, awarded_points_COPD014, reimbursement_COPD014, COPD014_diff),
           list(
             median = ~ round(median(.x, na.rm = TRUE),1),
             lower_quartile = ~ round(quantile(.x, 0.25, na.rm = TRUE),1),
             upper_quartile = ~ round(quantile(.x, 0.75, na.rm = TRUE),1)
           ),
           .names = "{.col}_{.fn}")
  )

colnames(COPD_all)

# smoking

SMOK_current <- full_join(SMOK002_current, SMOK004_current, 
                       by = c("pracid", "n", "scenario"))  %>%
  full_join(., SMOK005_current, by = c("pracid", "n", "SMOK1_REG", "scenario"))  

SMOK_current_diff <- SMOK_current %>% select(pracid, 
                                       reimbursement_SMOK002_current = reimbursement_SMOK002,
                                       reimbursement_SMOK004_current = reimbursement_SMOK004,
                                       reimbursement_SMOK005_current = reimbursement_SMOK005)


SMOK_new <- full_join(SMOK002_new, SMOK004_new,
                       by = c("pracid", "n", "scenario"))  %>%
  full_join(., SMOK005_new, by = c("pracid", "n", "SMOK1_REG", "scenario"))  %>%
  full_join(., SMOK_current_diff, by = "pracid") %>%
  mutate(SMOK002_diff = reimbursement_SMOK002 - reimbursement_SMOK002_current,
         SMOK004_diff = reimbursement_SMOK004 - reimbursement_SMOK004_current,
         SMOK005_diff = reimbursement_SMOK005 - reimbursement_SMOK005_current)


SMOK <- bind_rows(SMOK_current, SMOK_new)
         
# put it all together (smoking)
SMOK_all <- SMOK %>% mutate(scenario = factor(scenario, 
                                              levels = c("current", "new"))) %>% group_by(scenario) %>% 
  summarise(
    across(c(QI_perc_SMOK002, awarded_points_SMOK002, reimbursement_SMOK002, SMOK002_diff,
             QI_perc_SMOK004, awarded_points_SMOK004, reimbursement_SMOK004, SMOK004_diff,
             QI_perc_SMOK005, awarded_points_SMOK005, reimbursement_SMOK005, SMOK005_diff),
           list(
             median = ~ round(median(.x, na.rm = TRUE),1),
             lower_quartile = ~ round(quantile(.x, 0.25, na.rm = TRUE),1),
             upper_quartile = ~ round(quantile(.x, 0.75, na.rm = TRUE),1)
           ),
           .names = "{.col}_{.fn}")
  )

# hypertension


HYP_QC_QR <- full_join(HYP008_QC_QR, HYP009_QC_QR, 
                     by = c("pracid", "n", "HYP_REG", "scenario"))

HYP_QC_QR_diff <- HYP_QC_QR %>% select(pracid, 
                                   reimbursement_HYP008_QC_QR = reimbursement_HYP008,
                                   reimbursement_HYP009_QC_QR = reimbursement_HYP009)


HYP_QC_NN <- full_join(HYP008_QC_NN, HYP009_QC_NN,
                     by = c("pracid", "n", "HYP_REG", "scenario"))  %>%
  full_join(., HYP_QC_QR_diff, by = "pracid") %>%
  mutate(HYP008_diff = reimbursement_HYP008 - reimbursement_HYP008_QC_QR,
         HYP009_diff = reimbursement_HYP009 - reimbursement_HYP009_QC_QR)

HYP_QC_VT <- full_join(HYP008_QC_VT, HYP009_QC_VT,
                       by = c("pracid", "n", "HYP_REG", "scenario"))  %>%
  full_join(., HYP_QC_QR_diff, by = "pracid") %>%
  mutate(HYP008_diff = reimbursement_HYP008 - reimbursement_HYP008_QC_QR,
         HYP009_diff = reimbursement_HYP009 - reimbursement_HYP009_QC_QR)

HYP_UC_QR <- full_join(HYP008_UC_QR, HYP009_UC_QR,
                       by = c("pracid", "n", "HYP_REG", "scenario"))  %>%
  full_join(., HYP_QC_QR_diff, by = "pracid") %>%
  mutate(HYP008_diff = reimbursement_HYP008 - reimbursement_HYP008_QC_QR,
         HYP009_diff = reimbursement_HYP009 - reimbursement_HYP009_QC_QR)

HYP_UC_NN <- full_join(HYP008_UC_NN, HYP009_UC_NN,
                       by = c("pracid", "n", "HYP_REG", "scenario"))  %>%
  full_join(., HYP_QC_QR_diff, by = "pracid") %>%
  mutate(HYP008_diff = reimbursement_HYP008 - reimbursement_HYP008_QC_QR,
         HYP009_diff = reimbursement_HYP009 - reimbursement_HYP009_QC_QR)

HYP_UC_VT <- full_join(HYP008_UC_VT, HYP009_UC_VT,
                       by = c("pracid", "n", "HYP_REG", "scenario"))  %>%
  full_join(., HYP_QC_QR_diff, by = "pracid") %>%
  mutate(HYP008_diff = reimbursement_HYP008 - reimbursement_HYP008_QC_QR,
         HYP009_diff = reimbursement_HYP009 - reimbursement_HYP009_QC_QR)

HYP <- bind_rows(HYP_QC_QR, HYP_QC_NN, HYP_QC_VT,
                 HYP_UC_QR, HYP_UC_NN, HYP_UC_VT)

HYP_all <- HYP %>% mutate(scenario = factor(scenario, 
                                            levels = c("QC_QR", "QC_NN", "QC_VT",
                                              "UC_QR", "UC_NN", "UC_VT")))  %>% 
  group_by(scenario) %>% 
  summarise(
    across(c(QI_perc_HYP008, awarded_points_HYP008, reimbursement_HYP008, HYP008_diff,
             QI_perc_HYP009, awarded_points_HYP009, reimbursement_HYP009, HYP009_diff),
           list(
             median = ~ round(median(.x, na.rm = TRUE),1),
             lower_quartile = ~ round(quantile(.x, 0.25, na.rm = TRUE),1),
             upper_quartile = ~ round(quantile(.x, 0.75, na.rm = TRUE),1)
           ),
           .names = "{.col}_{.fn}")
  )

colnames(HYP_all)


# cleaning
## QI 1 att (median (IQR)); 
# QI 1 points (median (IQR));
#QI 1 reimburse (median (IQR)); 
# reimburse diff (median (IQR))


AST_all_clean <- AST_all %>% mutate(
  QI_perc_AST007 = sprintf("%.1f (%.1f to %.1f)", 
                           QI_perc_AST007_median, 
                           QI_perc_AST007_lower_quartile,
                           QI_perc_AST007_upper_quartile),
  awarded_points_AST007 = sprintf("%.1f (%.1f to %.1f)", 
                           awarded_points_AST007_median, 
                           awarded_points_AST007_lower_quartile,
                           awarded_points_AST007_upper_quartile),
reimbursement_AST007 = sprintf("%.1f (%.1f to %.1f)", 
                         reimbursement_AST007_median, 
                         reimbursement_AST007_lower_quartile,
                         reimbursement_AST007_upper_quartile),
reimbursement_diff_AST007 = sprintf("%.1f (%.1f to %.1f)", 
                               AST007_diff_median, 
                               AST007_diff_lower_quartile,
                               AST007_diff_upper_quartile),
QI_perc_AST008 = sprintf("%.1f (%.1f to %.1f)", 
                         QI_perc_AST008_median, 
                         QI_perc_AST008_lower_quartile,
                         QI_perc_AST008_upper_quartile),
awarded_points_AST008 = sprintf("%.1f (%.1f to %.1f)", 
                                awarded_points_AST008_median, 
                                awarded_points_AST008_lower_quartile,
                                awarded_points_AST008_upper_quartile),
reimbursement_AST008 = sprintf("%.1f (%.1f to %.1f)", 
                               reimbursement_AST008_median, 
                               reimbursement_AST008_lower_quartile,
                               reimbursement_AST008_upper_quartile),
reimbursement_diff_AST008 = sprintf("%.1f (%.1f to %.1f)", 
                                    AST008_diff_median, 
                                    AST008_diff_lower_quartile,
                                    AST008_diff_upper_quartile),
QI_perc_AST012 = sprintf("%.1f (%.1f to %.1f)", 
                         QI_perc_AST012_median, 
                         QI_perc_AST012_lower_quartile,
                         QI_perc_AST012_upper_quartile),
awarded_points_AST012 = sprintf("%.1f (%.1f to %.1f)", 
                                awarded_points_AST012_median, 
                                awarded_points_AST012_lower_quartile,
                                awarded_points_AST012_upper_quartile),
reimbursement_AST012 = sprintf("%.1f (%.1f to %.1f)", 
                               reimbursement_AST012_median, 
                               reimbursement_AST012_lower_quartile,
                               reimbursement_AST012_upper_quartile),
reimbursement_diff_AST012 = sprintf("%.1f (%.1f to %.1f)", 
                                    AST012_diff_median, 
                                    AST012_diff_lower_quartile,
                                    AST012_diff_upper_quartile)) %>%
  select(scenario, QI_perc_AST007, awarded_points_AST007, 
         reimbursement_AST007, reimbursement_diff_AST007,
         QI_perc_AST008, awarded_points_AST008, 
         reimbursement_AST008, reimbursement_diff_AST008,
         QI_perc_AST012, awarded_points_AST012, 
         reimbursement_AST012, reimbursement_diff_AST012)

AST_all_clean$reimbursement_diff_AST007[AST_all_clean$reimbursement_diff_AST007 == "NA (NA to NA)"] <- "-"
AST_all_clean$reimbursement_diff_AST008[AST_all_clean$reimbursement_diff_AST008 == "NA (NA to NA)"] <- "-"
AST_all_clean$reimbursement_diff_AST012[AST_all_clean$reimbursement_diff_AST012 == "NA (NA to NA)"] <- "-"

# COPD


COPD_all_clean <- COPD_all %>% mutate(
  QI_perc_COPD010 = sprintf("%.1f (%.1f to %.1f)", 
                           QI_perc_COPD010_median, 
                           QI_perc_COPD010_lower_quartile,
                           QI_perc_COPD010_upper_quartile),
  awarded_points_COPD010 = sprintf("%.1f (%.1f to %.1f)", 
                                  awarded_points_COPD010_median, 
                                  awarded_points_COPD010_lower_quartile,
                                  awarded_points_COPD010_upper_quartile),
  reimbursement_COPD010 = sprintf("%.1f (%.1f to %.1f)", 
                                 reimbursement_COPD010_median, 
                                 reimbursement_COPD010_lower_quartile,
                                 reimbursement_COPD010_upper_quartile),
  reimbursement_diff_COPD010 = sprintf("%.1f (%.1f to %.1f)", 
                                      COPD010_diff_median, 
                                      COPD010_diff_lower_quartile,
                                      COPD010_diff_upper_quartile),
  QI_perc_COPD014 = sprintf("%.1f (%.1f to %.1f)", 
                           QI_perc_COPD014_median, 
                           QI_perc_COPD014_lower_quartile,
                           QI_perc_COPD014_upper_quartile),
  awarded_points_COPD014 = sprintf("%.1f (%.1f to %.1f)", 
                                  awarded_points_COPD014_median, 
                                  awarded_points_COPD014_lower_quartile,
                                  awarded_points_COPD014_upper_quartile),
  reimbursement_COPD014 = sprintf("%.1f (%.1f to %.1f)", 
                                 reimbursement_COPD014_median, 
                                 reimbursement_COPD014_lower_quartile,
                                 reimbursement_COPD014_upper_quartile),
  reimbursement_diff_COPD014 = sprintf("%.1f (%.1f to %.1f)", 
                                      COPD014_diff_median, 
                                      COPD014_diff_lower_quartile,
                                      COPD014_diff_upper_quartile)) %>%
  select(scenario, QI_perc_COPD010, awarded_points_COPD010, 
         reimbursement_COPD010, reimbursement_diff_COPD010,
         QI_perc_COPD014, awarded_points_COPD014, 
         reimbursement_COPD014, reimbursement_diff_COPD014)


COPD_all_clean

COPD_all_clean$reimbursement_diff_COPD010[COPD_all_clean$reimbursement_diff_COPD010 == "NA (NA to NA)"] <- "-"
COPD_all_clean$reimbursement_diff_COPD014[COPD_all_clean$reimbursement_diff_COPD014 == "NA (NA to NA)"] <- "-"



SMOK_all_clean <- SMOK_all %>% mutate(
  QI_perc_SMOK002 = sprintf("%.1f (%.1f to %.1f)", 
                           QI_perc_SMOK002_median, 
                           QI_perc_SMOK002_lower_quartile,
                           QI_perc_SMOK002_upper_quartile),
  awarded_points_SMOK002 = sprintf("%.1f (%.1f to %.1f)", 
                                  awarded_points_SMOK002_median, 
                                  awarded_points_SMOK002_lower_quartile,
                                  awarded_points_SMOK002_upper_quartile),
  reimbursement_SMOK002 = sprintf("%.1f (%.1f to %.1f)", 
                                 reimbursement_SMOK002_median, 
                                 reimbursement_SMOK002_lower_quartile,
                                 reimbursement_SMOK002_upper_quartile),
  reimbursement_diff_SMOK002 = sprintf("%.1f (%.1f to %.1f)", 
                                      SMOK002_diff_median, 
                                      SMOK002_diff_lower_quartile,
                                      SMOK002_diff_upper_quartile),
  QI_perc_SMOK004 = sprintf("%.1f (%.1f to %.1f)", 
                           QI_perc_SMOK004_median, 
                           QI_perc_SMOK004_lower_quartile,
                           QI_perc_SMOK004_upper_quartile),
  awarded_points_SMOK004 = sprintf("%.1f (%.1f to %.1f)", 
                                  awarded_points_SMOK004_median, 
                                  awarded_points_SMOK004_lower_quartile,
                                  awarded_points_SMOK004_upper_quartile),
  reimbursement_SMOK004 = sprintf("%.1f (%.1f to %.1f)", 
                                 reimbursement_SMOK004_median, 
                                 reimbursement_SMOK004_lower_quartile,
                                 reimbursement_SMOK004_upper_quartile),
  reimbursement_diff_SMOK004 = sprintf("%.1f (%.1f to %.1f)", 
                                      SMOK004_diff_median, 
                                      SMOK004_diff_lower_quartile,
                                      SMOK004_diff_upper_quartile),
  QI_perc_SMOK005 = sprintf("%.1f (%.1f to %.1f)", 
                           QI_perc_SMOK005_median, 
                           QI_perc_SMOK005_lower_quartile,
                           QI_perc_SMOK005_upper_quartile),
  awarded_points_SMOK005 = sprintf("%.1f (%.1f to %.1f)", 
                                  awarded_points_SMOK005_median, 
                                  awarded_points_SMOK005_lower_quartile,
                                  awarded_points_SMOK005_upper_quartile),
  reimbursement_SMOK005 = sprintf("%.1f (%.1f to %.1f)", 
                                 reimbursement_SMOK005_median, 
                                 reimbursement_SMOK005_lower_quartile,
                                 reimbursement_SMOK005_upper_quartile),
  reimbursement_diff_SMOK005 = sprintf("%.1f (%.1f to %.1f)", 
                                      SMOK005_diff_median, 
                                      SMOK005_diff_lower_quartile,
                                      SMOK005_diff_upper_quartile)) %>%
  select(scenario, QI_perc_SMOK002, awarded_points_SMOK002, 
         reimbursement_SMOK002, reimbursement_diff_SMOK002,
         QI_perc_SMOK004, awarded_points_SMOK004, 
         reimbursement_SMOK004, reimbursement_diff_SMOK004,
         QI_perc_SMOK005, awarded_points_SMOK005, 
         reimbursement_SMOK005, reimbursement_diff_SMOK005)

SMOK_all_clean$reimbursement_diff_SMOK002[SMOK_all_clean$reimbursement_diff_SMOK002 == "NA (NA to NA)"] <- "-"
SMOK_all_clean$reimbursement_diff_SMOK004[SMOK_all_clean$reimbursement_diff_SMOK004 == "NA (NA to NA)"] <- "-"
SMOK_all_clean$reimbursement_diff_SMOK005[SMOK_all_clean$reimbursement_diff_SMOK005 == "NA (NA to NA)"] <- "-"


HYP_all_clean <- HYP_all %>% mutate(
  QI_perc_HYP008 = sprintf("%.1f (%.1f to %.1f)", 
                            QI_perc_HYP008_median, 
                            QI_perc_HYP008_lower_quartile,
                            QI_perc_HYP008_upper_quartile),
  awarded_points_HYP008 = sprintf("%.1f (%.1f to %.1f)", 
                                   awarded_points_HYP008_median, 
                                   awarded_points_HYP008_lower_quartile,
                                   awarded_points_HYP008_upper_quartile),
  reimbursement_HYP008 = sprintf("%.1f (%.1f to %.1f)", 
                                  reimbursement_HYP008_median, 
                                  reimbursement_HYP008_lower_quartile,
                                  reimbursement_HYP008_upper_quartile),
  reimbursement_diff_HYP008 = sprintf("%.1f (%.1f to %.1f)", 
                                       HYP008_diff_median, 
                                       HYP008_diff_lower_quartile,
                                       HYP008_diff_upper_quartile),
  QI_perc_HYP009 = sprintf("%.1f (%.1f to %.1f)", 
                            QI_perc_HYP009_median, 
                            QI_perc_HYP009_lower_quartile,
                            QI_perc_HYP009_upper_quartile),
  awarded_points_HYP009 = sprintf("%.1f (%.1f to %.1f)", 
                                   awarded_points_HYP009_median, 
                                   awarded_points_HYP009_lower_quartile,
                                   awarded_points_HYP009_upper_quartile),
  reimbursement_HYP009 = sprintf("%.1f (%.1f to %.1f)", 
                                  reimbursement_HYP009_median, 
                                  reimbursement_HYP009_lower_quartile,
                                  reimbursement_HYP009_upper_quartile),
  reimbursement_diff_HYP009 = sprintf("%.1f (%.1f to %.1f)", 
                                       HYP009_diff_median, 
                                       HYP009_diff_lower_quartile,
                                       HYP009_diff_upper_quartile)) %>%
  select(scenario, QI_perc_HYP008, awarded_points_HYP008, 
         reimbursement_HYP008, reimbursement_diff_HYP008,
         QI_perc_HYP009, awarded_points_HYP009, 
         reimbursement_HYP009, reimbursement_diff_HYP009)


HYP_all_clean

HYP_all_clean$reimbursement_diff_HYP008[HYP_all_clean$reimbursement_diff_HYP008 == "NA (NA to NA)"] <- "-"
HYP_all_clean$reimbursement_diff_HYP009[HYP_all_clean$reimbursement_diff_HYP009 == "NA (NA to NA)"] <- "-"

AST_all_clean
COPD_all_clean
SMOK_all_clean
HYP_all_clean

ft_AST_all_clean <- flextable(AST_all_clean)
ft_COPD_all_clean <- flextable(COPD_all_clean)
ft_SMOK_all_clean <- flextable(SMOK_all_clean)
ft_HYP_all_clean <- flextable(HYP_all_clean)

doc_outcomes <- read_docx() %>%
  body_add_par("Asthma reimbursement", 
               style = "Normal") %>%
  body_add_par(" ") %>%
  body_add_flextable(ft_AST_all_clean) %>%
  body_add_par(" ") %>%
  body_add_par("COPD reimbursement", 
               style = "Normal") %>%
  body_add_par(" ") %>%
  body_add_flextable(ft_COPD_all_clean) %>%
  body_add_par(" ") %>%
  body_add_par("Smoking reimbursement", 
               style = "Normal") %>%
  body_add_par(" ") %>%
  body_add_flextable(ft_SMOK_all_clean) %>%
  body_add_par(" ") %>%
  body_add_par("Hypertension reimbursement", 
               style = "Normal") %>%
  body_add_par(" ") %>%
  body_add_flextable(ft_HYP_all_clean) %>%
  body_add_par(" ")
  
  
  



print(doc_outcomes, target = "Z:/Group_work/QOF_project/Alex/Output/combined/reimbursement_tables.docx")  


