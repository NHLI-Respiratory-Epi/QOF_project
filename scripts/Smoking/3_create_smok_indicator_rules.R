
library(dplyr)
library(data.table)
library(bit64)


dat <- readRDS("Z:/Group_work/QOF_project/Data/Smoking/cohort/2_cohort_smoking_with_correct_DAT_cols.RDS")


# create the rules

# R = reject
# S = select
# N = next rule

# Ensure all patients have GMS registration status
dat <- dat %>%
  filter((!is.na(REG_DAT) & is.na(regenddate)) | (!is.na(REG_DAT) & regenddate > ACHV))

# SMOK1_REG

dat <- dat %>% mutate(SMOK1_REG_RN1 = if_else(!is.na(CHD_DAT)|
                                              !is.na(PAD_DAT)|
                                              !is.na(STRK_DAT)|
                                              !is.na(TIA_DAT)|
                                              !is.na(HYPLAT_DAT) & is.na(HYPRES_DAT)|
                                              !is.na(DMLAT_DAT) & is.na (DMRES_DAT) & PAT_AGE >= 17|
                                              !is.na(COPDLAT_DAT) & is.na(COPDRES_DAT)|
                                              !is.na(ASTLAT_DAT) & is.na(ASTRES_DAT) & !is.na(ASTTRT_DAT)|
                                              !is.na(CKD_DAT) & is.na(CKDRES_DAT) & is.na(CKD1AND2_DAT) & PAT_AGE >= 18|
                                              !is.na(MH_DAT), 1, 0)) # 1 = S, 0 = R
                 
dat <- dat %>% mutate(SMOK1_REG = ifelse(SMOK1_REG_RN1 == 1, 1, 0))



# SMOK2_REG

dat <- dat %>% mutate(SMOK2_REG_RN1 = if_else(PAT_AGE >= 15, 1, 0)) # 1 = S, 0 = R

dat <- dat %>% mutate(SMOK2_REG = ifelse(SMOK2_REG_RN1 == 1, 1, 0))

### INDICATOR 002 ###

dat <- dat %>% 
  mutate(SMOK002_denom_RN1 = if_else(!is.na(CHD_DAT)|
                                       !is.na(PAD_DAT)|
                                       !is.na(STRK_DAT)|
                                       !is.na(TIA_DAT)|
                                       !is.na(HYPLAT_DAT) & is.na(HYPRES_DAT)|
                                       !is.na(DMLAT_DAT) & is.na (DMRES_DAT) & PAT_AGE >= 17|
                                       !is.na(COPDLAT_DAT) & is.na(COPDRES_DAT)|
                                       !is.na(ASTLAT_DAT) & is.na(ASTRES_DAT) & !is.na(ASTTRT_DAT)|
                                       !is.na(CKD_DAT) & is.na(CKDRES_DAT) & is.na(CKD1AND2_DAT) & PAT_AGE >= 18|
                                       !is.na(MH_DAT), 1, 0, missing = 0), # 1 = N, 0 = R
         SMOK002_denom_RN2 = if_else(CSMOK_DAT >= PPED - 365.25, 1, 0, missing = 0), # 1 = S, 0 = N
         SMOK002_denom_RN3 = if_else(PAT_AGE > 25 & 
                                       !is.na(NSMOK_DAT) & 
                                       NSMOK_DAT >= DIAG_DAT & 
                                       NSMOK_DAT > PAT_DOB + 365.25*25, 1, 0, missing = 0), # 1 = S, 0 = N
         SMOK002_denom_RN4 = if_else(PAT_AGE <= 25 &
                                       NSMOK_DAT > PPED - 365.25, 1, 0, missing = 0), # 1 = S, 0 = N
         SMOK002_denom_RN5 = if_else(!is.na(LEXSMOK_DAT) &
                                       LEXSMOK_DAT > PPED - 365.25, 1, 0, missing = 0), # 1 = S, 0 = N
         SMOK002_denom_RN6 = if_else(!is.na(E3YREXSMOK2_DAT), 1, 0, missing = 0), # 1 = S, 0 = N
         SMOK002_denom_RN7 = if_else(SMOKPCAPU_DAT > PPED - 365.25, 1, 0, missing = 0), # 1 = R, 0 = N
         SMOK002_denom_RN8 = if_else(SMOKSTATDEC_DAT > PPED - 365.25, 1, 0, missing = 0), # 1 = R, 0 = N
         SMOK002_denom_RN9 = if_else(SMOKPCADEC_DAT > PPED - 365.25, 1, 0, missing = 0), # 1 = R, 0 = N
         SMOK002_denom_RN10 = if_else(!is.na(SMOKINVITE1_DAT) &
                                        !is.na(SMOKINVITE2_DAT), 1, 0, missing = 0), # 1 = R, 0 = N
         SMOK002_denom_RN11 = if_else(DIAG_DAT > PPED - 93, 1, 0, missing = 0), # 1 = R, 0 = N
         SMOK002_denom_RN12 = if_else(REG_DAT > PPED - 93, 1, 0, missing = 0)) # 1 = R, 0 = S


dat %>% select(starts_with("SMOK002_denom_RN")) %>% summary()

# create the final indicator denominator

dat <- dat %>% mutate(SMOK002_denominator = if_else(
  SMOK1_REG == 1 &                    
    (SMOK002_denom_RN1 != 0 &         
    (SMOK002_denom_RN2 == 1) | (SMOK002_denom_RN2 == 0) &      
    (SMOK002_denom_RN3 == 1) | (SMOK002_denom_RN2 == 0) & 
    (SMOK002_denom_RN4 == 1) | (SMOK002_denom_RN2 == 0) & 
    (SMOK002_denom_RN5 == 1) | (SMOK002_denom_RN2 == 0) & 
    (SMOK002_denom_RN6 == 1) | (SMOK002_denom_RN2 == 0) & 
     SMOK002_denom_RN7 == 0 & 
     SMOK002_denom_RN8 == 0 & 
     SMOK002_denom_RN9 == 0 & 
     SMOK002_denom_RN10 == 0 & 
     SMOK002_denom_RN11 == 0 & 
     SMOK002_denom_RN12 == 0 
       ), 
  1, 0, missing = 0))



# The numerator is applied to patients selected into the denominator

dat <- dat %>%
  mutate(SMOK002_numerator = case_when(
    SMOK002_denominator == 0 ~ 0L,
    SMOK002_denom_RN2 == 1 ~ 1L,
    SMOK002_denom_RN3 == 1 ~ 1L,
    SMOK002_denom_RN4 == 1 ~ 1L,
    SMOK002_denom_RN5 == 1 ~ 1L,
    SMOK002_denom_RN6 == 1 ~ 1L,
    TRUE ~ 0L
  ))

# quality indicator prevalence........

dat %>%
  filter(SMOK1_REG == 1) %>%
  group_by(SMOK002_denominator,
           SMOK002_numerator) %>%
  summarise(freq = n())

sum(dat$SMOK002_numerator, na.rm = TRUE)/sum(dat$SMOK002_denominator) 


# Explore where the numbers go
smok1_reg_dat <- dat %>%
  filter(SMOK1_REG == 1)

denom_rule_summary <- smok1_reg_dat %>%
  group_by(SMOK002_denom_RN2,
           SMOK002_denom_RN3,
           SMOK002_denom_RN4,
           SMOK002_denom_RN5,
           SMOK002_denom_RN6,
           SMOK002_denom_RN7,
           SMOK002_denom_RN8,
           SMOK002_denom_RN9,
           SMOK002_denom_RN10,
           SMOK002_denom_RN11,
           SMOK002_denom_RN12
  ) %>%
  summarise(freq = n())



# add the process for HYP008 to smoking
# dat <- dat %>% mutate(HYP008_inc_exc_type = case_when(
#   HYP_REG == 1 & HYP008_denom_RN1 == 0 & (HYP008_denom_RN2 == 1 | HYP008_denom_RN3 == 1)  ~ "Included in numerator",
#   HYP_REG == 0 | HYP008_denom_RN1 == 1 | HYP008_denom_RN9 == 1 | 
#     HYP008_denom_RN10 == 1  ~ NA_character_, # not included in either because not on register, or not correct age, 
#   # or not registered or newly registered or new diagnosis
#   HYP008_denom_RN4 == 1 ~ "Rejected as on maximal therapy",
#   HYP008_denom_RN5 == 1 ~ "Rejected as unsuitable",
#   HYP008_denom_RN6 == 1 | HYP008_denom_RN7 == 1 ~ "Rejected as declined care",
#   HYP008_denom_RN8 == 1 ~ "Rejected as invited",
#   TRUE ~ "Included in denominator"
# ))

dat %>% select(contains("RN")) %>% summary()

dat <- dat %>% mutate(SMOK002_inc_exc_type = case_when(
  SMOK1_REG == 0 | SMOK002_denom_RN1 == 0 ~ NA_character_, # not included in either 
                                                           # because not on register, or disease groups
  SMOK002_numerator == 1 ~ "Included in numerator",
  SMOK002_denom_RN11 == 1 | SMOK002_denom_RN12 == 1 ~ NA_character_, # Rejected as not registered or new diagnosis
  SMOK002_denom_RN7 == 1 ~ "Rejected as unsuitable",
  SMOK002_denom_RN8 == 1 | SMOK002_denom_RN9 == 1 ~ "Rejected as declined care",
  SMOK002_denom_RN10 == 1 ~ "Rejected as invited",
  TRUE ~ "Included in denominator" 
  ))

# make factor
dat$SMOK002_inc_exc_type <- factor(dat$SMOK002_inc_exc_type,
                                  levels = c("Included in numerator",
                                             "Included in denominator",
                                             "Rejected as unsuitable",
                                             "Rejected as declined care",
                                             "Rejected as invited"))
summary(dat$SMOK002_inc_exc_type)



### INDICATOR 004 ###

dat <- dat %>% 
  mutate(SMOK004_denom_RN1 = if_else(is.na(CSMOK_DAT), 1, 0, missing = 0), # 1 = R, 0 = N
         SMOK004_denom_RN2 = if_else((!is.na(REFERSSSA_DAT) & (REFERSSSA_DAT >= ACHV - 365.25*2))|
                                     (!is.na(PHARM_DAT) & (PHARM_DAT >= ACHV - 365.25*2))|
                                     (!is.na(PHARMDRUG_DAT) & (PHARMDRUG_DAT >= ACHV - 365.25*2)),
                                     1, 0, missing = NA_real_), # 1 = S, 0 = N
         SMOK004_denom_RN3 = if_else(SMOKPCAPU_DAT >= QSSD, 1, 0, missing = 0), # 1 = R, 0 = N
         SMOK004_denom_RN4 = if_else(SMOKPCADEC_DAT >= QSSD, 1, 0, missing = 0), # 1 = R, 0 = N
         SMOK004_denom_RN5 = if_else(!is.na(SMOKINVITE1_DAT) & !is.na(SMOKINVITE2_DAT), 1, 0, missing = NA_real_), # 1 = R, 0 = N
         SMOK004_denom_RN6 = if_else(REG_DAT >= ACHV - 93, 1, 0, missing = NA_real_)) # 1 = R, 0 = S

dat %>% select(starts_with("SMOK004_denom_RN")) %>% summary()
# There are two ways a patient can be selected into the denominator:
# In both cases, the patient must not be rejected at rule no. 1 (i.e. they must
# be a current smoker either way). Then they can either...
# A) Be selected in rule 2 (through having received the care standard) OR
# B) Be passed through rules 2, 3, 4, and 5, and then meet the criteria for
# selection in rule 6.

dat <- dat %>% mutate(SMOK004_denominator = if_else(
  (SMOK2_REG == 1) &                    # In population register AND
    ((SMOK004_denom_RN1 == 0) &         # Have status "current smoker" AND
       ((SMOK004_denom_RN2 == 1) |      # (A) Meet RN2 selection criterion OR...
          ((SMOK004_denom_RN2 == 0) &   # ...
             (SMOK004_denom_RN3 == 0) & # ...
             (SMOK004_denom_RN4 == 0) & # ...
             (SMOK004_denom_RN5 == 0) & # ...
             (SMOK004_denom_RN6 == 0))  # ... (B) make it through RN3-6
       )), 
  1, 0, missing = 0))

# The numerator is applied to patients selected into the denominator

dat <- dat %>%
  mutate(SMOK004_numerator = case_when(
    SMOK004_denominator == 0 ~ 0L,
    SMOK004_denom_RN2 == 1 ~ 1L,
    SMOK004_denom_RN2 == 0 ~ 0L,
    TRUE ~ NA_integer_
  ))

# quality indicator prevalence........

dat %>%
  filter(SMOK2_REG == 1) %>%
  group_by(SMOK004_denominator,
           SMOK004_numerator) %>%
  summarise(freq = n())

sum(dat$SMOK004_numerator, na.rm = TRUE)/sum(dat$SMOK004_denominator) 


# Explore where the numbers go
smok2_reg_dat <- dat %>%
  filter(SMOK2_REG == 1)

smok2_reg_cs_dat <- smok2_reg_dat %>%
  filter(SMOK004_denom_RN1 == 0)

denom_rule_summary <- smok2_reg_cs_dat %>%
  group_by(SMOK004_denom_RN2,
           SMOK004_denom_RN3,
           SMOK004_denom_RN4,
           SMOK004_denom_RN5,
           SMOK004_denom_RN6
  ) %>%
  summarise(freq = n())



dat <- dat %>% mutate(SMOK004_inc_exc_type = case_when(
  SMOK2_REG == 0 | SMOK004_denom_RN1 == 1 ~ NA_character_, # not included in either 
  # because not on register, or disease groups
  SMOK004_numerator == 1 ~ "Included in numerator",
  SMOK004_denom_RN6 == 1 ~ NA_character_, # Rejected as new registration
  SMOK004_denom_RN3 == 1 ~ "Rejected as unsuitable",
  SMOK004_denom_RN4 == 1 ~ "Rejected as declined care",
  SMOK004_denom_RN5 == 1 ~ "Rejected as invited",
  TRUE ~ "Included in denominator" 
))

# make factor
dat$SMOK004_inc_exc_type <- factor(dat$SMOK004_inc_exc_type,
                                   levels = c("Included in numerator",
                                              "Included in denominator",
                                              "Rejected as unsuitable",
                                              "Rejected as declined care",
                                              "Rejected as invited"))
summary(dat$SMOK004_inc_exc_type)




### INDICATOR 005 ###

dat <- dat %>% 
  mutate(SMOK005_denom_RN1 = if_else(is.na(CSMOK_DAT), 1, 0, missing = 0), # 1 = R, 0 = N
         SMOK005_denom_RN2 = if_else((!is.na(REFERSSSA_DAT) & REFERSSSA_DAT >= (QSSD))|
                                       (!is.na(PHARM_DAT) & PHARM_DAT >= (QSSD))|
                                       (!is.na(PHARMDRUG_DAT) & PHARMDRUG_DAT >= (QSSD)),
                                     1, 0, missing = NA_real_), # 1 = R, 0 = N
         SMOK005_denom_RN3 = if_else(SMOKPCAPU_DAT >= (QSSD), 1, 0, missing = 0), # 1 = R, 0 = N
         SMOK005_denom_RN4 = if_else(SMOKPCADEC_DAT >= (QSSD), 1, 0, missing = 0), # 1 = R, 0 = N
         SMOK005_denom_RN5 = if_else(!is.na(SMOKINVITE1_DAT) & !is.na(SMOKINVITE2_DAT), 1, 0, missing = 0), # 1 = R, 0 = N
         SMOK005_denom_RN6 = if_else(DIAG_DAT >= PPED - 93, 1, 0, missing = 0),# 1 = R, 0 = S
         SMOK005_denom_RN7 = if_else(REG_DAT >= PPED - 93, 1, 0, missing = 0)) # 1 = R, 0 = S

# create the final indicator denominator

dat <- dat %>% mutate(SMOK005_denominator = if_else(
  SMOK1_REG == 1 &                    # In population register AND
    (SMOK005_denom_RN1 == 0 &         # Have status "current smoker" AND
       (SMOK005_denom_RN2 == 1 |      # (A) Meet RN2 selection criterion OR...
          (SMOK005_denom_RN2 == 0 &   # ...
             SMOK005_denom_RN3 == 0 & # ...
             SMOK005_denom_RN4 == 0 & # ...
             SMOK005_denom_RN5 == 0 & # ...
             SMOK005_denom_RN6 == 0)  # ... (B) make it through RN3-6
       )), 
  1, 0, missing = 0))

# The numerator is applied to patients selected into the denominator

dat <- dat %>%
  mutate(SMOK005_numerator = case_when(
    SMOK005_denominator == 0 ~ 0,
    SMOK005_denom_RN2 == 1 ~ 1L,
    SMOK005_denom_RN2 == 0 ~ 0L,
    TRUE ~ NA_integer_
  ))

# quality indicator prevalence........

sum(dat$SMOK005_numerator, na.rm = TRUE)/sum(dat$SMOK005_denominator) 




dat <- dat %>% mutate(SMOK005_inc_exc_type = case_when(
  SMOK1_REG == 0 | SMOK005_denom_RN1 == 1 ~ NA_character_, # not included in either 
  # because not on register, or disease groups
  SMOK005_numerator == 1 ~ "Included in numerator",
  SMOK005_denom_RN6 == 1 | SMOK005_denom_RN7 == 1 ~ NA_character_, # Rejected as new registration or disease
  SMOK005_denom_RN3 == 1 ~ "Rejected as unsuitable",
  SMOK005_denom_RN4 == 1 ~ "Rejected as declined care",
  SMOK005_denom_RN5 == 1 ~ "Rejected as invited",
  TRUE ~ "Included in denominator" 
))

# make factor
dat$SMOK005_inc_exc_type <- factor(dat$SMOK005_inc_exc_type,
                                   levels = c("Included in numerator",
                                              "Included in denominator",
                                              "Rejected as unsuitable",
                                              "Rejected as declined care",
                                              "Rejected as invited"))
summary(dat$SMOK005_inc_exc_type)



# Save the cohort!!!!

dat <- saveRDS(dat, "Z:/Group_work/QOF_project/Data/Smoking/cohort/3_cohort_smoking_with_rules.RDS")


