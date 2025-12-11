
library(dplyr)
library(data.table)
library(bit64)


dat <- readRDS("Z:/Group_work/QOF_project/Data/Smoking/cohort/2_cohort_asthma_with_correct_DAT_cols.RDS")


# create the rules

# R = reject
# S = select
# N = next rule

# SMOK1_REG

dat <- dat %>% mutate(SMOK1_REG_RN1 = if_else(!is.na(ASTLAT_DAT) & !is.na(ASTTRT_DAT),
                                            1, 0), # 1 = N, 0 = R
                      SMOK1_REG_RN2 = if_else(is.na(ASTRES_DAT), 1, 0), # 1 = N, 0 = R
                      SMOK1_REG_RN3 = if_else(PAT_AGE >= 6, 1, 0)) # 1 = S, 0 = R

dat <- dat %>% mutate(SMOK1_REG = ifelse(SMOK1_REG_RN1 == 1 &
                                         SMOK1_REG_RN2 == 1 &
                                         SMOK1_REG_RN3 == 1, 1, 0))



# SMOK2_REG

dat <- dat %>% mutate(SMOK2_REG_RN1 = if_else(PAT_AGE >= 15, 1, 0)) # 1 = S, 0 = R

dat <- dat %>% mutate(SMOK2_REG = ifelse(SMOK2_REG_RN1 == 1, 1, 0))




###simpler smoking rules

dat <- dat %>% 
  mutate(SMOK004_denom_RN1 = if_else(!is.na(CSMOK_DAT), 1, 0, missing = 0), # 1 = R, 0 = N
         SMOK004_denom_RN2 = if_else(REFERSSSA_DAT >= (PPED - 720) >= QSSD|
                                     ASTPCAPU_DAT >= QSSD|
                                     ASTPCAPU_DAT >= QSSD, 1, 0, missing = 0), # 1 = R, 0 = N
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

dat <- dat %>% mutate(SMOK004_nume = SMOK004_denom_RN1)

# and the big reveal for quality indicator prevalence........

sum(dat$SMOK004_nume)/sum(dat$SMOK004_denom) # Not bad!!!
