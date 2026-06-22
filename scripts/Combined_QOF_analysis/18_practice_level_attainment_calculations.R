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
points$pts <- c(15, 6, 20, 9, 2, 25, 12, 25, 38, 14) # points available
points$value <- 225.49

points

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




dat_practice <- dat_practice %>% 
  mutate(AST_REG_perc_QA_QT = (AST_REG_QA_QT / n)*100,
         AST007_perc_QA_QT = (AST007_nume_QA_QT/AST007_denom_QA_QT)*100,
         AST008_perc_QA_QT = (AST008_nume_QA_QT/AST008_denom_QA_QT)*100,
         AST008_alt_perc_QA_QT = (AST008_nume_alt_QA_QT/AST008_denom_QA_QT)*100,
         AST012_perc_QA_QT = (AST012_nume_QA_QT/AST012_denom_QA_QT)*100,
         
         AST_REG_perc_QA_BT = (AST_REG_QA_BT / n)*100,
         AST007_perc_QA_BT = (AST007_nume_QA_BT/AST007_denom_QA_BT)*100,
         AST008_perc_QA_BT = (AST008_nume_QA_BT/AST008_denom_QA_BT)*100,
         AST008_alt_perc_QA_BT = (AST008_nume_alt_QA_BT/AST008_denom_QA_BT)*100,
         AST012_perc_QA_BT = (AST012_nume_QA_BT/AST012_denom_QA_BT)*100,
         
         AST_REG_perc_QA_IT = (AST_REG_QA_IT / n)*100,
         AST007_perc_QA_IT = (AST007_nume_QA_IT/AST007_denom_QA_IT)*100,
         AST008_perc_QA_IT = (AST008_nume_QA_IT/AST008_denom_QA_IT)*100,
         AST008_alt_perc_QA_IT = (AST008_nume_alt_QA_IT/AST008_denom_QA_IT)*100,
         AST012_perc_QA_IT = (AST012_nume_QA_IT/AST012_denom_QA_IT)*100,
         
         AST_REG_perc_AA_QT = (AST_REG_AA_QT / n)*100,
         AST007_perc_AA_QT = (AST007_nume_AA_QT/AST007_denom_AA_QT)*100,
         AST008_perc_AA_QT = (AST008_nume_AA_QT/AST008_denom_AA_QT)*100,
         AST008_alt_perc_AA_QT = (AST008_nume_alt_AA_QT/AST008_denom_AA_QT)*100,
         AST012_perc_AA_QT = (AST012_nume_AA_QT/AST012_denom_AA_QT)*100,
         
         AST_REG_perc_AA_BT = (AST_REG_AA_BT / n)*100,
         AST007_perc_AA_BT = (AST007_nume_AA_BT/AST007_denom_AA_BT)*100,
         AST008_perc_AA_BT = (AST008_nume_AA_BT/AST008_denom_AA_BT)*100,
         AST008_alt_perc_AA_BT = (AST008_nume_alt_AA_BT/AST008_denom_AA_BT)*100,
         AST012_perc_AA_BT = (AST012_nume_AA_BT/AST012_denom_AA_BT)*100,
         
         AST_REG_perc_AA_IT = (AST_REG_AA_IT / n)*100,
         AST007_perc_AA_IT = (AST007_nume_AA_IT/AST007_denom_AA_IT)*100,
         AST008_perc_AA_IT = (AST008_nume_AA_IT/AST008_denom_AA_IT)*100,
         AST008_alt_perc_AA_IT = (AST008_nume_alt_AA_IT/AST008_denom_AA_IT)*100,
         AST012_perc_AA_IT = (AST012_nume_AA_IT/AST012_denom_AA_IT)*100,
         
         COPD_REG_perc_QC = (COPD_REG_QC / n)*100,
         COPD010_perc_QC = (COPD010_num_QC / COPD010_denom_QC)*100,
         COPD014_perc_QC = (COPD014_num_QC / COPD014_denom_QC)*100,
         
         COPD_REG_perc_VC = (COPD_REG_VC / n)*100,
         COPD010_perc_VC = (COPD010_num_VC / COPD010_denom_VC)*100,
         COPD014_perc_VC = (COPD014_num_VC / COPD014_denom_VC)*100,
         
         COPD_REG_perc_VC_VP = (COPD_REG_VC_VP / n)*100,
         COPD010_perc_VC_VP = (COPD010_num_VC_VP / COPD010_denom_VC_VP)*100,
         COPD014_perc_VC_VP = (COPD014_num_VC_VP / COPD014_denom_VC_VP)*100,
         
         
         COPD_REG_perc_VC_RP = (COPD_REG_VC_RP / n)*100,
         COPD010_perc_VC_RP = (COPD010_num_VC_RP / COPD010_denom_VC_RP)*100,
         COPD014_perc_VC_RP = (COPD014_num_VC_RP / COPD014_denom_VC_RP)*100,
         
         COPD_REG_perc_UC = (COPD_REG_UC / n)*100,
         COPD010_perc_UC = (COPD010_num_UC / COPD010_denom_UC)*100,
         COPD014_perc_UC = (COPD014_num_UC / COPD014_denom_UC)*100,
         
         
         COPD_REG_perc_UC_VP = (COPD_REG_UC_VP / n)*100,
         COPD010_perc_UC_VP = (COPD010_num_UC_VP / COPD010_denom_UC_VP)*100,
         COPD014_perc_UC_VP = (COPD014_num_UC_VP / COPD014_denom_UC_VP)*100,
         
         COPD_REG_perc_UC_RP = (COPD_REG_UC_RP / n)*100,
         COPD010_perc_UC_RP = (COPD010_num_UC_RP / COPD010_denom_UC_RP)*100,
         COPD014_perc_UC_RP = (COPD014_num_UC_RP / COPD014_denom_UC_RP)*100, 
         
         SMOK1_REG_perc_current = (SMOK1_REG_current / n)*100,
         SMOK2_REG_perc_current = (SMOK2_REG_current/ n)*100,
         SMOK002_perc_current = (SMOK002_numerator_current/SMOK002_denominator_current)*100,
         SMOK004_perc_current = (SMOK004_numerator_current/SMOK004_denominator_current)*100,
         SMOK005_perc_current = (SMOK005_numerator_current/SMOK005_denominator_current)*100,
         
         SMOK1_REG_perc_new = (SMOK1_REG_new / n)*100,
         SMOK2_REG_perc_new = (SMOK2_REG_new/ n)*100,
         SMOK002_perc_new = (SMOK002_numerator_new/SMOK002_denominator_new)*100,
         SMOK004_perc_new = (SMOK004_numerator_new/SMOK004_denominator_new)*100,
         SMOK005_perc_new = (SMOK005_numerator_new/SMOK005_denominator_new)*100,
         
         HYP_REG_perc_QC_QR = (HYP_REG_QC_QR / n)*100,
         HYP008_perc_QC_QR = (HYP008_nume_QC_QR/HYP008_denom_QC_QR)*100,
         HYP009_perc_QC_QR = (HYP009_nume_QC_QR/HYP009_denom_QC_QR)*100,
         
         HYP_REG_perc_QC_NN = (HYP_REG_QC_NN / n)*100,
         HYP008_perc_QC_NN = (HYP008_nume_QC_NN/HYP008_denom_QC_NN)*100,
         HYP009_perc_QC_NN = (HYP009_nume_QC_NN/HYP009_denom_QC_NN)*100,
         
         HYP_REG_perc_QC_VT = (HYP_REG_QC_VT / n)*100,
         HYP008_perc_QC_VT = (HYP008_nume_QC_VT/HYP008_denom_QC_VT)*100,
         HYP009_perc_QC_VT = (HYP009_nume_QC_VT/HYP009_denom_QC_VT)*100,
         
         HYP_REG_perc_UC_QR = (HYP_REG_UC_QR / n)*100,
         HYP008_perc_UC_QR = (HYP008_nume_UC_QR/HYP008_denom_UC_QR)*100,
         HYP009_perc_UC_QR = (HYP009_nume_UC_QR/HYP009_denom_UC_QR)*100,
         
         HYP_REG_perc_UC_NN = (HYP_REG_UC_NN / n)*100,
         HYP008_perc_UC_NN = (HYP008_nume_UC_NN/HYP008_denom_UC_NN)*100,
         HYP009_perc_UC_NN = (HYP009_nume_UC_NN/HYP009_denom_UC_NN)*100,
         
         HYP_REG_perc_UC_VT = (HYP_REG_UC_VT / n)*100,
         HYP008_perc_UC_VT = (HYP008_nume_UC_VT/HYP008_denom_UC_VT)*100,
         HYP009_perc_UC_VT = (HYP009_nume_UC_VT/HYP009_denom_UC_VT)*100)
         


  summary(dat_practice)       
  
  
  
  
  
dat_all <- dat_practice %>% ungroup() %>% 
    summarise(
      across(!starts_with("pracid"),
             list(
      mean = ~ round(mean(.x, na.rm = TRUE),1),
      median = ~ round(median(.x, na.rm = TRUE),1),
      lower_quartile = ~ round(quantile(.x, 0.25, na.rm = TRUE),1),
      upper_quartile = ~ round(quantile(.x, 0.75, na.rm = TRUE),1),
      min = ~ round(min(.x, na.rm = TRUE),1),
      max = ~ round(max(.x, na.rm = TRUE),1)
    ),
    .names = "{.col}_{.fn}")
    )
    
      
  dat_all %>% select(contains("perc")) %>% as.data.frame() %>% print()

  

  dat_all
    
  
  #        %>% # order columns by scenario
  # select(pracid, n, 
  #        COPD_REG_QC, COPD_REG_prop_QC, COPD010_num_QC, COPD010_denom_QC, COPD010_prop_QC,
  #        COPD014_num_QC, COPD014_denom_QC, COPD014_prop_QC,
  #        COPD_REG_VC, COPD_REG_prop_VC, COPD010_num_VC, COPD010_denom_VC, COPD010_prop_VC,
  #        COPD014_num_VC, COPD014_denom_VC, COPD014_prop_VC,
  #        COPD_REG_VC_VP, COPD_REG_prop_VC_VP, COPD010_num_VC_VP, COPD010_denom_VC_VP, COPD010_prop_VC_VP,
  #        COPD014_num_VC_VP, COPD014_denom_VC_VP, COPD014_prop_VC_VP,
  #        COPD_REG_VC_RP, COPD_REG_prop_VC_RP, COPD010_num_VC_RP, COPD010_denom_VC_RP, COPD010_prop_VC_RP,
  #        COPD014_num_VC_RP, COPD014_denom_VC_RP, COPD014_prop_VC_RP,
  #        COPD_REG_UC, COPD_REG_prop_UC, COPD010_num_UC, COPD010_denom_UC, COPD010_prop_UC,
  #        COPD014_num_UC, COPD014_denom_UC, COPD014_prop_UC,
  #        COPD_REG_UC_VP, COPD_REG_prop_UC_VP, COPD010_num_UC_VP, COPD010_denom_UC_VP, 
  #        COPD010_prop_UC_VP, COPD014_num_UC_VP, COPD014_denom_UC_VP, COPD014_prop_UC_VP,
  #        COPD_REG_UC_RP, COPD_REG_prop_UC_RP, COPD010_num_UC_RP, COPD010_denom_UC_RP, COPD010_prop_UC_RP,
  #        COPD014_num_UC_RP, COPD014_denom_UC_RP, COPD014_prop_UC_RP)

# rename columns that have an underscore within their scenarios so that the underscore is removed

colnames(dat_practice) <- colnames(dat_practice) %>% 
  str_replace_all("VC_VP", "VCVP") %>%
  str_replace_all("VC_RP", "VCRP") %>%
  str_replace_all("UC_VP", "UCVP") %>%
  str_replace_all("UC_RP", "UCRP")


dat_practice_for_hist <- dat_practice

dat_practice %>% select(starts_with("COPD_REG")) %>% colnames()

dat_practice_for_hist$COPD_REG_QC_vs_VC <- dat_practice_for_hist$COPD_REG_QC/dat_practice_for_hist$COPD_REG_VC
dat_practice_for_hist$COPD_REG_QC_vs_UC <- dat_practice_for_hist$COPD_REG_QC/dat_practice_for_hist$COPD_REG_UC
dat_practice_for_hist$COPD_REG_QC_vs_VCVP <- dat_practice_for_hist$COPD_REG_QC/dat_practice_for_hist$COPD_REG_VCVP
dat_practice_for_hist$COPD_REG_QC_vs_VCRP <- dat_practice_for_hist$COPD_REG_QC/dat_practice_for_hist$COPD_REG_VCRP
dat_practice_for_hist$COPD_REG_QC_vs_UCVP <- dat_practice_for_hist$COPD_REG_QC/dat_practice_for_hist$COPD_REG_UCVP
dat_practice_for_hist$COPD_REG_QC_vs_UCRP <- dat_practice_for_hist$COPD_REG_QC/dat_practice_for_hist$COPD_REG_UCRP

dat_practice_for_hist <- dat_practice_for_hist %>% filter(COPD_REG_QC > 20)

dat_practice_for_hist$COPD_REG_VCVP_vs_QC <- dat_practice_for_hist$COPD_REG_VCVP/dat_practice_for_hist$COPD_REG_QC

summary(dat_practice_for_hist$COPD_REG_QC_vs_VCVP)
summary(dat_practice_for_hist$COPD_REG_VCVP_vs_QC)

dat_practice_for_hist$COPD_REG_VCVP_vs_QC_cat <- cut(dat_practice_for_hist$COPD_REG_VCVP_vs_QC, 
                                                     breaks = c(0.85, 0.95, 0.98, 1.02, 
                                                                1.05, 1.5),
                                                     labels = c("0.85-0.95", "0.95-0.98", "0.98-1.02", "1.02-1.05", "+1.05"),
                                                     right = FALSE)
# right = false means that the intervals are left-closed and right-open, so for example 0.85-0.9 includes 0.85 but not 0.9
summary(dat_practice_for_hist$COPD_REG_VCVP_vs_QC_cat)
table(dat_practice_for_hist$COPD_REG_VCVP_vs_QC_cat)
table(dat_practice_for_hist$COPD_REG_VCVP_vs_QC_cat) %>% prop.table() %>% round(3)*100

dat_practice_for_hist %>% select(contains("_vs_")) %>% summary()

ggplot(dat_practice_for_hist, aes(x = COPD_REG_QC_vs_VC)) +
  geom_histogram(binwidth = 0.01, boundary = 0, closed = "left",
                 fill = NA, colour = "black") +
  scale_x_continuous(breaks = seq(0.65, 1.35, 0.05), limits = c(0.65, 1.35)) +
  labs(title = "COPD register proportions (current QOF codes vs validated codes)",
       x = "Ratio of COPD register Ns",
       y = "Number of practices") +
  theme_bw()  
  ggsave("X:/QOF_project/JENNI_TO_CHECK/COPD_register_proportions_QC_vs_VC_histogram.png", width = 8, height = 6)

ggplot(dat_practice_for_hist, aes(x = COPD_REG_QC_vs_VCVP)) +
  geom_histogram(binwidth = 0.01, boundary = 0, closed = "left",
                 fill = NA, colour = "black") +
  scale_x_continuous(breaks = seq(0.65, 1.35, 0.05), limits = c(0.65, 1.35)) +
  labs(title = "COPD register proportions (current QOF codes vs validated codes\n with validated process codes <2 years)",
       x = "Ratio of COPD register Ns",
       y = "Number of practices") +
  theme_bw()
ggsave("X:/QOF_project/JENNI_TO_CHECK/COPD_register_proportions_QC_vs_VCVP_histogram.png", width = 8, height = 6)

ggplot(dat_practice_for_hist, aes(x = COPD_REG_QC_vs_VCRP)) +
  geom_histogram(binwidth = 0.01, boundary = 0, closed = "left",
                 fill = NA, colour = "black") +
  scale_x_continuous(breaks = seq(0.65, 1.35, 0.05), limits = c(0.65, 1.35)) +
  labs(title = "COPD register proportions (current QOF codes vs validated codes\n with restricted process codes <2 years)",
       x = "Ratio of COPD register Ns",
       y = "Number of practices") +
  theme_bw()
ggsave("X:/QOF_project/JENNI_TO_CHECK/COPD_register_proportions_QC_vs_VCRP_histogram.png", width = 8, height = 6)

ggplot(dat_practice_for_hist, aes(x = COPD_REG_QC_vs_UC)) +
  geom_histogram(binwidth = 0.01, boundary = 0, closed = "left",
                 fill = NA, colour = "black") +
  scale_x_continuous(breaks = seq(0.65, 1.35, 0.05), limits = c(0.65, 1.35)) +
  labs(title = "COPD register proportions (current QOF codes vs updated codes)",
       x = "Ratio of COPD register Ns",
       y = "Number of practices") +
  theme_bw()
ggsave("X:/QOF_project/JENNI_TO_CHECK/COPD_register_proportions_QC_vs_UC_histogram.png", width = 8, height = 6)

ggplot(dat_practice_for_hist, aes(x = COPD_REG_QC_vs_UCVP)) +
  geom_histogram(binwidth = 0.01, boundary = 0, closed = "left",
                 fill = NA, colour = "black") +
  scale_x_continuous(breaks = seq(0.65, 1.35, 0.05), limits = c(0.65, 1.35)) +
  labs(title = "COPD register proportions (current QOF codes vs updated codes\n with validated process codes <2 years)",
       x = "Ratio of COPD register Ns",
       y = "Number of practices") +
  theme_bw()
ggsave("X:/QOF_project/JENNI_TO_CHECK/COPD_register_proportions_QC_vs_UCVP_histogram.png", width = 8, height = 6)

ggplot(dat_practice_for_hist, aes(x = COPD_REG_QC_vs_UCRP)) +
  geom_histogram(binwidth = 0.01,  boundary = 0, closed = "left",
                 fill = NA, colour = "black") +
  scale_x_continuous(breaks = seq(0.65, 1.35, 0.05), limits = c(0.65, 1.35)) +
  labs(title = "COPD register proportions (current QOF codes vs updated codes\n with restricted process codes <2 years)",
       x = "Ratio of COPD register Ns",
       y = "Number of practices") +
  theme_bw()
ggsave("X:/QOF_project/JENNI_TO_CHECK/COPD_register_proportions_QC_vs_UCRP_histogram.png", width = 8, height = 6)



# hist(dat_practice_for_hist$COPD_REG_QC_vs_VC, breaks = 100, main =
#        "COPD register proportions (current QOF codes vs validated codes) across practices",
#      xlab = "Ratio of COPD register Ns",
#      ylab = "Number of practices",
#      xlim = c(0.65, 1.75))
# 
# hist(dat_practice_for_hist$COPD_REG_QC_vs_VCVP, breaks = 100, main =
#        "COPD register proportions (current QOF codes vs validated codes\n with validated process codes <2 years) across practices",
#      xlab = "Ratio of COPD register Ns",
#      ylab = "Number of practices",
#      xlim = c(0.65, 1.75))
# 
# hist(dat_practice_for_hist$COPD_REG_QC_vs_VCRP, breaks = 50, main =
#        "COPD register proportions (current QOF codes vs validated codes\n with restricted process codes <2 years) across practices",
#      xlab = "Ratio of COPD register Ns",
#      ylab = "Number of practices",
#      xlim = c(0.65, 1.75),
#      ylim = c(0, 80))
# 
# hist(dat_practice_for_hist$COPD_REG_QC_vs_UCRP, breaks = 50, main =
#        "COPD register proportions (current QOF codes vs updated codes\n with restricted process codes <2 years) across practices",
#      xlab = "Ratio of COPD register Ns",
#      ylab = "Number of practices",
#      xlim = c(0.65, 1.75),
#      ylim = c(0, 80))



# reshape so that we have 6 rows for each practice to represent the scenarios

dat_practice <- dat_practice %>% pivot_longer(
  cols = -c(pracid, n),
  names_to = c(".value", "scenario"),
  names_pattern = "(.*)_(QC|VC|VCVP|VCRP|UC|UCVP|UCRP)$")


head(dat_practice)


head(dat_practice)

summary(dat_practice)

# clean up for export
dat_practice <- dat_practice %>% select(pracid:COPD010_prop)

dat_practice$COPD010_denom[dat_practice$COPD010_denom < 10] <- NA
dat_practice$COPD010_num[dat_practice$COPD010_num < 10] <- NA
dat_practice$COPD_REG[dat_practice$COPD_REG < 10] <- NA

# replace pracid with a unique number for each practice to anonymise the data

dat_practice <- dat_practice %>% mutate(pracid = as.integer(factor(pracid)))

dat_practice$n <- NULL

dat_practice$scenario <- recode(dat_practice$scenario,
                                 "QC" = "Current QOF",
                                 "VC" = "Validated COPD codelist, no process codes",
                                 "VCVP" = "Validated COPD codelist with process codes from validated codelist <2 years",
                                 "VCRP" = "Validated COPD codelist with restricted process codes (i.e. just monitoring and annual review codes) <2 years",
                                 "UC" = "Updated COPD codelist, no process codes",
                                 "UCVP" = "Updated COPD codelist with validated process codes <2 years",
                                 "UCRP" = "Updated COPD codelist with restricted process codes <2 years (i.e. just monitoring and annual review codes) <2 years")


nrow(dat_practice)
summary(dat_practice)

colnames(dat_practice)[1] <- "anonymised_pracid"

dat_practice %>% write.csv("X:/QOF_project/JENNI_TO_CHECK/COPD_practice_level_analysis.csv",
                           row.names = FALSE)

dat %>% filter(COPD_REG_QC == 0 & COPD_REG_UC == 1) %>% nrow() # 3 patients out of 3million patients are added in - negligible

# we can therefore assume that anyone who is QC but not UC is because of the differing COPD codelist
# this means that we can use QC, UC, and UCRP for the ones we've been doing.
dat %>% group_by(COPD_REG_QC, COPD_REG_VC, COPD_REG_VC_VP) %>% summarise(n = n()) %>% ungroup()
  

dat %>% group_by(COPD_REG_QC, COPD_REG_UC, COPD_REG_UC_RP) %>% summarise(n = n()) %>% ungroup() %>%
  filter(!(COPD_REG_QC == 0 & COPD_REG_UC == 0 & COPD_REG_UC_RP == 0)) %>% 
  filter(!(COPD_REG_QC == 1 & COPD_REG_UC == 1 & COPD_REG_UC_RP == 0)) %>%
  filter(!(COPD_REG_QC == 0 & COPD_REG_UC == 1 & COPD_REG_UC_RP == 1)) %>% write.csv("clipboard")

head(dat_practice)
  