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

dat <- readRDS("X:/QOF_project/Data/cohort/50_temp_cohort_asthma_QI_comps_added.RDS")

colnames(dat)

# we want to see how the COPD indicators change across the practices

dat <- dat %>% select(patid, pracid, starts_with("COPD_REG"),
               starts_with("COPD010_num"),
               starts_with("COPD010_denom"),
               starts_with("COPD014_num"),
               starts_with("COPD014_denom"))  %>% select(-ends_with("comparison"))


colnames(dat)

nrow(dat)
sum(dat$COPD_REG_QC)
(sum(dat$COPD_REG_QC)/nrow(dat))*100 # 1.69%

sum(dat$COPD_REG_VC_VP)
(sum(dat$COPD_REG_VC_VP)/nrow(dat))*100 # 1.73%

47597/46575

sum(dat$COPD010_denom_QC)
sum(dat$COPD010_denom_VC_VP)

sum(dat$COPD010_denom_VC_VP)/sum(dat$COPD010_denom_QC) # 3% increase

# [1] "patid"               "pracid"              "COPD_REG_QC"         "COPD_REG_VC"        
# [5] "COPD_REG_VC_VP"      "COPD_REG_VC_RP"      "COPD_REG_UC"         "COPD_REG_UC_VP"     
# [9] "COPD_REG_UC_RP"      "COPD010_num_QC"      "COPD010_num_VC"      "COPD010_num_VC_VP"  
# [13] "COPD010_num_VC_RP"   "COPD010_num_UC"      "COPD010_num_UC_VP"   "COPD010_num_UC_RP"  
# [17] "COPD010_denom_QC"    "COPD010_denom_VC"    "COPD010_denom_VC_VP" "COPD010_denom_VC_RP"
# [21] "COPD010_denom_UC"    "COPD010_denom_UC_VP" "COPD010_denom_UC_RP" "COPD014_num_QC"     
# [25] "COPD014_num_VC"      "COPD014_num_VC_VP"   "COPD014_num_VC_RP"   "COPD014_num_UC"     
# [29] "COPD014_num_UC_VP"   "COPD014_num_UC_RP"   "COPD014_denom_QC"    "COPD014_denom_VC"   
# [33] "COPD014_denom_VC_VP" "COPD014_denom_VC_RP" "COPD014_denom_UC"    "COPD014_denom_UC_VP"
# [37] "COPD014_denom_UC_RP"

# summarise for each scenario (each unique ending)

dat_practice <- dat %>% group_by(pracid) %>% 
  summarise(n = n(), across(-starts_with("patid"), ~ sum(.))) %>% 
  ungroup()

# create the proportions for each scenario

dat_practice <- dat_practice %>% 
  mutate(COPD_REG_prop_QC = round((COPD_REG_QC / n)*100, 1),
         COPD_REG_prop_VC = round((COPD_REG_VC / n)*100, 1),
         COPD_REG_prop_VC_VP = round((COPD_REG_VC_VP / n)*100, 1),
         COPD_REG_prop_VC_RP = round((COPD_REG_VC_RP / n)*100, 1),
         COPD_REG_prop_UC = round((COPD_REG_UC / n)*100, 1),
         COPD_REG_prop_UC_VP = round((COPD_REG_UC_VP / n)*100, 1),
         COPD_REG_prop_UC_RP = round((COPD_REG_UC_RP / n)*100, 1),
         COPD010_prop_QC = round((COPD010_num_QC / COPD010_denom_QC)*100, 1),
         COPD010_prop_VC = round((COPD010_num_VC / COPD010_denom_VC)*100, 1),
         COPD010_prop_VC_VP = round((COPD010_num_VC_VP / COPD010_denom_VC_VP)*100, 1),
         COPD010_prop_VC_RP = round((COPD010_num_VC_RP / COPD010_denom_VC_RP)*100, 1),
         COPD010_prop_UC = round((COPD010_num_UC / COPD010_denom_UC)*100, 1),
         COPD010_prop_UC_VP = round((COPD010_num_UC_VP / COPD010_denom_UC_VP)*100, 1),
         COPD010_prop_UC_RP = round((COPD010_num_UC_RP / COPD010_denom_UC_RP)*100, 1),
         COPD014_prop_QC = round((COPD014_num_QC / COPD014_denom_QC)*100, 1),
         COPD014_prop_VC = round((COPD014_num_VC / COPD014_denom_VC)*100, 1),
         COPD014_prop_VC_VP = round((COPD014_num_VC_VP / COPD014_denom_VC_VP)*100, 1),
         COPD014_prop_VC_RP = round((COPD014_num_VC_RP / COPD014_denom_VC_RP)*100, 1),
         COPD014_prop_UC = round((COPD014_num_UC / COPD014_denom_UC)* 100, 1),
         COPD014_prop_UC_VP = round((COPD014_num_UC_VP / COPD014_denom_UC_VP)*100, 1),
         COPD014_prop_UC_RP = round((COPD014_num_UC_RP / COPD014_denom_UC_RP)*100, 1)) %>% # order columns by scenario
  select(pracid, n, 
         COPD_REG_QC, COPD_REG_prop_QC, COPD010_num_QC, COPD010_denom_QC, COPD010_prop_QC,
         COPD014_num_QC, COPD014_denom_QC, COPD014_prop_QC,
         COPD_REG_VC, COPD_REG_prop_VC, COPD010_num_VC, COPD010_denom_VC, COPD010_prop_VC,
         COPD014_num_VC, COPD014_denom_VC, COPD014_prop_VC,
         COPD_REG_VC_VP, COPD_REG_prop_VC_VP, COPD010_num_VC_VP, COPD010_denom_VC_VP, COPD010_prop_VC_VP,
         COPD014_num_VC_VP, COPD014_denom_VC_VP, COPD014_prop_VC_VP,
         COPD_REG_VC_RP, COPD_REG_prop_VC_RP, COPD010_num_VC_RP, COPD010_denom_VC_RP, COPD010_prop_VC_RP,
         COPD014_num_VC_RP, COPD014_denom_VC_RP, COPD014_prop_VC_RP,
         COPD_REG_UC, COPD_REG_prop_UC, COPD010_num_UC, COPD010_denom_UC, COPD010_prop_UC,
         COPD014_num_UC, COPD014_denom_UC, COPD014_prop_UC,
         COPD_REG_UC_VP, COPD_REG_prop_UC_VP, COPD010_num_UC_VP, COPD010_denom_UC_VP, 
         COPD010_prop_UC_VP, COPD014_num_UC_VP, COPD014_denom_UC_VP, COPD014_prop_UC_VP,
         COPD_REG_UC_RP, COPD_REG_prop_UC_RP, COPD010_num_UC_RP, COPD010_denom_UC_RP, COPD010_prop_UC_RP,
         COPD014_num_UC_RP, COPD014_denom_UC_RP, COPD014_prop_UC_RP)

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
  