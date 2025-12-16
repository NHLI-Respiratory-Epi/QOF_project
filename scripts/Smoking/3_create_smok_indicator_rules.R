
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
         SMOK004_denom_RN2 = if_else(REFERSSSA_DAT >= PPED - 365.25*2|
                                     PHARM_DAT >= PPED - 365.25*2|
                                     PHARMDRUG_DAT >= PPED - 365.25*2, 1, 0, missing = 0), # 1 = R, 0 = N
         SMOK004_denom_RN3 = if_else(SMOKPCAPU_DAT >= QSSD, 1, 0, missing = 0), # 1 = R, 0 = N
         SMOK004_denom_RN4 = if_else(SMOKPCADEC_DAT >= QSSD, 1, 0, missing = 0), # 1 = R, 0 = N
         SMOK004_denom_RN5 = if_else(!is.na(SMOKINVITE1_DAT) & !is.na(SMOKINVITE2_DAT), 1, 0, missing = 0), # 1 = R, 0 = N
         SMOK004_denom_RN6 = if_else(REG_DAT >= PPED - 93, 1, 0, missing = 0)) # 1 = R, 0 = S

# create the final indicator denominator

dat <- dat %>% mutate(SMOK004_denom = if_else(SMOK2_REG == 1 & 
                                               (SMOK004_denom_RN1 == 1 | (
                                                 SMOK004_denom_RN2 == 0 &
                                                   SMOK004_denom_RN3 == 1 &
                                                   SMOK004_denom_RN4 == 1 &
                                                   SMOK004_denom_RN5 == 1 &
                                                   SMOK004_denom_RN6 == 1)), 
                                             1, 0, missing = 0))




# numerator is the first rule of the denominator! 

dat <- dat %>% mutate(SMOK004_nume = SMOK004_denom_RN2)

# and the big reveal for quality indicator prevalence........

sum(dat$SMOK004_nume)/sum(dat$SMOK004_denom) 



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


# numerator is the first rule of the denominator! 

dat <- dat %>% mutate(SMOK005_nume = SMOK005_denom_RN2)

# and the big reveal for quality indicator prevalence........

sum(dat$SMOK005_nume)/sum(dat$SMOK005_denom)


### 3 months is 93 days
# PPED = ACHV
# PPED - 1 year = QSSD


