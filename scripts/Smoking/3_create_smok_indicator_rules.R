
library(dplyr)
library(data.table)
library(bit64)


dat <- readRDS("Z:/Group_work/QOF_project/Data/Smoking/cohort/2_cohort_smoking_with_correct_DAT_cols.RDS")


# create the rules

# R = reject
# S = select
# N = next rule



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




### INDICATOR 004 ###

dat <- dat %>% 
  mutate(SMOK004_denom_RN1 = if_else(is.na(CSMOK_DAT), 1, 0, missing = 0), # 1 = R, 0 = N
         SMOK004_denom_RN2 = if_else(REFERSSSA_DAT >= ACHV - 365.25*2|
                                     PHARM_DAT >= ACHV - 365.25*2|
                                     PHARMDRUG_DAT >= ACHV - 365.25*2, 1, 0, missing = 0), # 1 = S, 0 = N
         SMOK004_denom_RN3 = if_else(SMOKPCAPU_DAT >= QSSD, 1, 0, missing = 0), # 1 = R, 0 = N
         SMOK004_denom_RN4 = if_else(SMOKPCADEC_DAT >= QSSD, 1, 0, missing = 0), # 1 = R, 0 = N
         SMOK004_denom_RN5 = if_else(!is.na(SMOKINVITE1_DAT) & !is.na(SMOKINVITE2_DAT), 1, 0, missing = 0), # 1 = R, 0 = N
         SMOK004_denom_RN6 = if_else(REG_DAT >= ACHV - 93, 1, 0, missing = 0)) # 1 = R, 0 = S

# create the final indicator denominator

dat <- dat %>% mutate(SMOK004_denom = if_else(SMOK2_REG == 1 & 
                                               (SMOK004_denom_RN1 == 1 | (
                                                 SMOK004_denom_RN2 == 0 &
                                                   SMOK004_denom_RN3 == 1 &
                                                   SMOK004_denom_RN4 == 1 &
                                                   SMOK004_denom_RN5 == 1 &
                                                   SMOK004_denom_RN6 == 1)), 
                                             1, 0, missing = 0))

# There are two ways a patient can be selected into the denominator:
# In both cases, the patient must not be rejected at rule no. 1 (i.e. they must
# be a current smoker either way). Then they can either...
# A) Be selected in rule 2 (through having received the care standard) OR
# B) Be passed through rules 2, 3, 4, and 5, and then meet the criteria for
# selection in rule 6.

dat <- dat %>% mutate(SMOK004_denominator = if_else(
  SMOK2_REG == 1 &                    # In population register AND
    (SMOK004_denom_RN1 == 0 &         # Have status "current smoker" AND
       (SMOK004_denom_RN2 == 1 |      # (A) Meet RN2 selection criterion OR...
          (SMOK004_denom_RN2 == 0 &   # ...
             SMOK004_denom_RN3 == 0 & # ...
             SMOK004_denom_RN4 == 0 & # ...
             SMOK004_denom_RN5 == 0 & # ...
             SMOK004_denom_RN6 == 0)  # ... (B) make it through RN3-6
       )), 
  1, 0, missing = 0))




# numerator is the 2nd rule of the denominator! 

dat <- dat %>% mutate(SMOK004_nume = SMOK004_denom_RN2)

# The numerator is applied to patients selected into the denominator

dat <- dat %>%
  mutate(SMOK004_numerator = case_when(
    SMOK004_denominator == 0 ~ NA_integer_,
    SMOK004_denom_RN2 == 1 ~ 1L,
    SMOK004_denom_RN2 == 0 ~ 0L,
    TRUE ~ NA_integer_
  ))

# quality indicator prevalence........

sum(dat$SMOK004_nume)/sum(dat$SMOK004_denom) 

sum(dat$SMOK004_numerator, na.rm = TRUE)/sum(dat$SMOK004_denominator) 


### INDICATOR 005 ###

dat <- dat %>% 
  mutate(SMOK005_denom_RN1 = if_else(is.na(CSMOK_DAT), 1, 0, missing = 0), # 1 = R, 0 = N
         SMOK005_denom_RN2 = if_else(REFERSSSA_DAT >= (QSSD)|
                                       PHARM_DAT >= (QSSD)|
                                       PHARMDRUG_DAT >= (QSSD), 1, 0, missing = 0), # 1 = R, 0 = N
         SMOK005_denom_RN3 = if_else(SMOKPCAPU_DAT >= (QSSD), 1, 0, missing = 0), # 1 = R, 0 = N
         SMOK005_denom_RN4 = if_else(SMOKPCADEC_DAT >= (QSSD), 1, 0, missing = 0), # 1 = R, 0 = N
         SMOK005_denom_RN5 = if_else(!is.na(SMOKINVITE1_DAT) & !is.na(SMOKINVITE2_DAT), 1, 0, missing = 0), # 1 = R, 0 = N
         SMOK005_denom_RN6 = if_else(DIAG_DAT >= PPED - 93, 1, 0, missing = 0),# 1 = R, 0 = S
         SMOK005_denom_RN7 = if_else(REG_DAT >= PPED - 93, 1, 0, missing = 0)) # 1 = R, 0 = S

# create the final indicator denominator

dat <- dat %>% mutate(SMOK005_denom = if_else(SMOK1_REG == 1 & 
                                                (SMOK005_denom_RN1 == 1 | (
                                                  SMOK005_denom_RN2 == 0 &
                                                    SMOK005_denom_RN3 == 1 &
                                                    SMOK005_denom_RN4 == 1 &
                                                    SMOK005_denom_RN5 == 1 &
                                                    SMOK005_denom_RN6 == 1 &
                                                    SMOK005_denom_RN7 == 1)), 
                                              1, 0, missing = 0))

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


# numerator is the 2nd rule of the denominator! 

dat <- dat %>% mutate(SMOK005_nume = SMOK005_denom_RN2)

# The numerator is applied to patients selected into the denominator

dat <- dat %>%
  mutate(SMOK005_numerator = case_when(
    SMOK005_denominator == 0 ~ NA_integer_,
    SMOK005_denom_RN2 == 1 ~ 1L,
    SMOK005_denom_RN2 == 0 ~ 0L,
    TRUE ~ NA_integer_
  ))

# quality indicator prevalence........

sum(dat$SMOK005_nume)/sum(dat$SMOK005_denom)

sum(dat$SMOK005_numerator, na.rm = TRUE)/sum(dat$SMOK005_denominator) 
