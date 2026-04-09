

library(dplyr)
library(data.table)
library(bit64)
library(finalfit)

# read in cohort as it currently stands

dat <- readRDS("Z:/Group_work/QOF_project/Data/cohort/5_cohort_with_covars_and_outcomes_updated2.RDS")

colnames(dat)
nrow(dat)
str(dat)
summary(dat)

# add in age

dat <- dat %>% mutate(age = as.numeric((QSSD - dob)/365.25))

# need to do some further dataset cleaning

dat$gender <- factor(dat$gender)
dat$region <- factor(dat$region)
dat$eth5 <- factor(dat$eth5)
dat$eth19 <- factor(dat$eth19)

# BMI - lots many missing

colnames(dat)
dat$gord[is.na(dat$gord)] <- 0
dat$chd_qof[is.na(dat$chd_qof)] <- 0
dat$pad_qof[is.na(dat$pad_qof)] <- 0
dat$stroke_qof[is.na(dat$stroke_qof)] <- 0
dat$tia_qof[is.na(dat$tia_qof)] <- 0
dat$diabetes_qof[is.na(dat$diabetes_qof)] <- 0
dat$ckd_qof[is.na(dat$ckd_qof)] <- 0
dat$mhealth_qof[is.na(dat$mhealth_qof)] <- 0
dat$hyperchol_qof[is.na(dat$hyperchol_qof)] <- 0
dat$htn_qof[is.na(dat$htn_qof)] <- 0
dat$asthma_plan_qof[is.na(dat$asthma_plan_qof)] <- 0
dat$smokeref_qof[is.na(dat$smokeref_qof)] <- 0
dat$smokecess_qof[is.na(dat$smokecess_qof)] <- 0
dat$smokecessdrugs_qof[is.na(dat$smokecessdrugs_qof)] <- 0

summary(dat)

# let's put BMI into categories

dat <- dat %>% mutate(BMI_cat = case_when(
  BMI < 18.5 ~ "Underweight",
  BMI < 25 ~ "Normal weight",
  BMI < 30 ~ "Overweight",
  BMI < 100 ~ "Obese",
  TRUE ~ "Missing BMI")) %>%
  mutate(BMI_cat = factor(BMI_cat,
                          levels = c("Underweight",
                                     "Normal weight",
                                     "Overweight",
                                     "Obese",
                                     "Missing BMI")))

summary(dat$BMI_cat)
table(dat$BMI_cat)
summary(dat$BMI)

# we can label our pat characteristics variables at this point too:

dat <- dat %>% 
  mutate(gord = ff_label(gord, "Gastro-oesophageal reflux disease (GORD/GERD)"),
         chd_qof = ff_label(chd_qof, "Coronary heart disease (CHD)"),
         pad_qof = ff_label(pad_qof, "Peripheral arterial disease (PAD)"),
         stroke_qof = ff_label(stroke_qof, "Stroke"),
         tia_qof = ff_label(tia_qof, "Transient ischaemic attack (TIA)"),
         diabetes_qof = ff_label(diabetes_qof, "Diabetes"),
         ckd_qof = ff_label(ckd_qof, "Chronic Kidney Disease (CKD)"),
         mhealth_qof = ff_label(mhealth_qof, "Mental health impairment"),
         hyperchol_qof = ff_label(hyperchol_qof, "Hypercholesterolemia"),
         htn_qof = ff_label(htn_qof, "Hypertension"),
         gender = ff_label(gender, "Gender"),
         region = ff_label(region, "Region"),
         eth5 = ff_label(eth5, "Ethnicity (5 categories)"),
         eth19 = ff_label(eth19, "Ethnicity (19 categories)"),
         age = ff_label(age, "Age"),
         BMI_cat = ff_label(BMI_cat, "BMI category"))

# summary_factorlist(dat, dependent = "gender", explanatory = "gord")
# add in the comparisons - asthma first

asthma <- readRDS("Z:/Group_work/QOF_project/Data/Asthma/cohort/11_cohort_asthma_with_comparison_vars.RDS")

colnames(asthma)

dat <- full_join(dat, asthma, by = "patid")

# add in others as they come in

copd <- readRDS("Z:/Group_work/QOF_project/Data/COPD/cohort/06_cohort_COPD_with_comparison_vars.RDS")

colnames(copd)

dat <- full_join(dat, copd, by = "patid")

colnames(dat)


# we can label the current QOF register and indicator variables


# Asthma QIs
dat <- dat %>%
  mutate(AST007_denom_QA_QT = ff_label(AST007_denom_QA_QT, 
                                      "AST007 (asthma review) denominator"),
         AST007_nume_QA_QT = ff_label(AST007_nume_QA_QT,
                                     "AST007 (asthma review) numerator"),
         AST008_denom_QA_QT = ff_label(AST008_denom_QA_QT, 
                                      "AST008 (exposure to smoking) denominator"),
         AST008_nume_QA_QT = ff_label(AST008_nume_QA_QT, 
                                     "AST008 (exposure to smoking) numerator"),
         AST012_denom_QA_QT = ff_label(AST012_denom_QA_QT, 
                                      "AST012 (newly diagnosed with objective test) denominator"),
         AST012_nume_QA_QT = ff_label(AST012_nume_QA_QT, 
                                     "AST012 (newly diagnosed with objective test) numerator"),
         AST007_inc_exc_type_QA_QT = ff_label(AST007_inc_exc_type_QA_QT, 
                                       "AST007 (asthma review) selected/rejected categories"),
         AST008_inc_exc_type_QA_QT = ff_label(AST008_inc_exc_type_QA_QT, 
                                        "AST007 (exposure to smoking) selected/rejected categories"),
         AST012_inc_exc_type_QA_QT = ff_label(AST012_inc_exc_type_QA_QT, 
                                        "AST007 (newly diagnosed with objective test) selected/rejected categories"))
         
# COPD QIs

colnames(dat)

dat <- dat %>%
  mutate(COPD010_denom_QC = ff_label(COPD010_denom_QC, 
                                       "COPD010 (COPD review) denominator"),
         COPD010_num_QC = ff_label(COPD010_num_QC,
                                      "COPD010 (COPD review) numerator"),
         COPD014_denom_QC = ff_label(COPD014_denom_QC, 
                                       "COPD014 (referral to PR) denominator"),
         COPD014_num_QC = ff_label(COPD014_num_QC, 
                                      "COPD014 (referral to PR) numerator"))
        
saveRDS(dat, "Z:/Group_work/QOF_project/Data/cohort/50_temp_cohort_asthma_QI_comps_added.RDS")
