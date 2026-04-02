
library(dplyr)
library(data.table)
library(bit64)


dat <- readRDS("Z:/Group_work/QOF_project/Data/Hypertension/cohort/3_cohort_hypertension_with_DAT_cols.RDS")


# create the rules

# R = reject
# S = select
# N = next rule

#------------------- HYP_REG

dat <- dat %>% mutate(HYP_REG_RN1 = if_else(!is.na(HYPLAT_DAT) & is.na(HYPRES_DAT),
                                            1, 0, missing = 0), # 1 = S, 0 = R
                      HYP_REG = HYP_REG_RN1) # only one rule for the hypertension register

table(dat$HYP_REG)                      

#------------------- HYP008


dat <- dat %>% 
  mutate(HYP008_denom_RN1 = if_else(PAT_AGE >= 80, 1, 0), # 1 = R, 0 = N
         HYP008_denom_RN2 = if_else(HOMEAMBBPSYSLAT_VAL <= 135 &
                                      HOMEAMBBPDIALAT_VAL <= 85 &
                                      HOMEAMBBPLAT_DAT >= QSSD &
                                      CLHMAMBBPLAT_DAT == HOMEAMBBPLAT_DAT, 1, 0, missing = 0), # 1 = S, 0 = N
         HYP008_denom_RN3 = if_else(CLINBPSYSLAT_VAL <= 140 &
                                      CLINBPDIALAT_VAL <= 90 &
                                      CLINBPLAT_DAT >= QSSD &
                                      CLHMAMBBPLAT_DAT == CLINBPLAT_DAT, 1, 0, missing = 0), # 1 = S, 0 = N
         HYP008_denom_RN4 = if_else(HTMAX_DAT >= QSSD, 1, 0, missing = 0), # 1 = R, 0 = N
         HYP008_denom_RN5 = if_else(HYPPCAPU_DAT >= QSSD, 1, 0, missing = 0), # 1 = R, 0 = N
         HYP008_denom_RN6 = if_else(BPDEC_DAT >= QSSD | HOMEBPDEC_DAT >= QSSD | 
                                      ABPMDEC_DAT >= QSSD, 1, 0, missing = 0), # 1 = R, 0 = N.
         HYP008_denom_RN7 = if_else(HYPPCADEC_DAT >= QSSD, 1, 0, missing = 0), # 1 = R, 0 = N
         HYP008_denom_RN8_SS1 = if_else(CLINBPLAT_DAT >= QSSD &   # subsection 1 for rule 8
                                          (CLINBPSYSLAT_VAL > 140 | CLINBPDIALAT_VAL > 90) &
                                          CLHMAMBBPLAT_DAT == CLINBPLAT_DAT &
                                          HYPINVITE1_DAT > CLINBPLAT_DAT & 
                                          !is.na(HYPINVITE2_DAT), 1, 0, missing = 0), # 1/0 = meets/doesn't meet SS1 criteria
         HYP008_denom_RN8_SS2 = if_else(HOMEAMBBPLAT_DAT >= QSSD &   # subsection 2 for rule 8
                                          (HOMEAMBBPSYSLAT_VAL > 135 | HOMEAMBBPDIALAT_VAL > 85) &
                                          CLHMAMBBPLAT_DAT == HOMEAMBBPLAT_DAT &
                                          HYPINVITE1_DAT > HOMEAMBBPLAT_DAT & 
                                          !is.na(HYPINVITE2_DAT), 1, 0, missing = 0), # 1/0 = meets/doesn't meet SS2 criteria
         HYP008_denom_RN8_SS3 = if_else(!is.na(HYPINVITE2_DAT) &
                                          (is.na(CLHMAMBBPLAT_DAT) | CLHMAMBBPLAT_DAT <= QSSD),
                                        1, 0, missing = 0), # 1/0 = meets/doesn't meet SS3 criteria
         HYP008_denom_RN8 = if_else(HYP008_denom_RN8_SS1 == 1 |  # any of the subsections need to be met to pass
                                      HYP008_denom_RN8_SS2 == 1 |
                                      HYP008_denom_RN8_SS3 == 1, 1, 0, missing = 0), # 1 = R, 0 = N
         HYP008_denom_RN9 = if_else(HYP_DAT >= as.Date("2023-07-01"), 1, 0, missing = 0), # 1 = R, 0 = N # PPED (/ACHV) - 9 months
         HYP008_denom_RN10 = if_else(regstartdate >= as.Date("2023-07-01"), 1, 0, missing = 0)) # 1 = R, 0 = S
    
# put the rules together

dat <- dat %>% mutate(HYP008_denom = if_else(HYP_REG == 1 & # registered
                                               HYP008_denom_RN1 == 0 & ( # appropriate age
                                               HYP008_denom_RN2 == 1 | # suitably controlled bp clinical measurement
                                               HYP008_denom_RN3 == 1 | # suitably controlled bp home measurement
                                                 (HYP008_denom_RN4 == 0 & # not kicked out for any other reason
                                                    HYP008_denom_RN5 == 0 & 
                                                    HYP008_denom_RN6 == 0 &
                                                    HYP008_denom_RN7 == 0 &
                                                    HYP008_denom_RN8 == 0 & 
                                                    HYP008_denom_RN9 == 0 & 
                                                    HYP008_denom_RN10 == 0)), 1, 0, missing = 0))

# numerator

dat <- dat %>% mutate(HYP008_nume_RN1 = HYP008_denom_RN2, # numerator rules already created for the denominator
                      HYP008_nume_RN2 = HYP008_denom_RN3)

dat <- dat %>% mutate(HYP008_nume = if_else(HYP008_denom == 1 & 
                                              (HYP008_nume_RN1 == 1 | 
                                              HYP008_nume_RN2 == 1), 1, 0, missing = 0)) 

dat %>% select(starts_with("HYP008")) %>% summary()



# the big reveal... 
sum(dat$HYP008_nume)/sum(dat$HYP008_denom)



# We can try HYP009 while we are at it...




dat <- dat %>% 
  mutate(HYP009_denom_RN1 = if_else(PAT_AGE <80, 1, 0), # 1 = R, 0 = N
         HYP009_denom_RN2 = if_else(HOMEAMBBPSYSLAT_VAL <= 145 &
                                      HOMEAMBBPDIALAT_VAL <= 85 &
                                      HOMEAMBBPLAT_DAT >= QSSD &
                                      CLHMAMBBPLAT_DAT == HOMEAMBBPLAT_DAT, 1, 0, missing = 0), # 1 = S, 0 = N
         HYP009_denom_RN3 = if_else(CLINBPSYSLAT_VAL <= 150 &
                                      CLINBPDIALAT_VAL <= 90 &
                                      CLINBPLAT_DAT >= QSSD &
                                      CLHMAMBBPLAT_DAT == CLINBPLAT_DAT, 1, 0, missing = 0), # 1 = S, 0 = N
         HYP009_denom_RN4 = if_else(HTMAX_DAT >= QSSD, 1, 0, missing = 0), # 1 = R, 0 = N
         HYP009_denom_RN5 = if_else(HYPPCAPU_DAT >= QSSD, 1, 0, missing = 0), # 1 = R, 0 = N
         HYP009_denom_RN6 = if_else(BPDEC_DAT >= QSSD | HOMEBPDEC_DAT >= QSSD | 
                                      ABPMDEC_DAT >= QSSD, 1, 0, missing = 0), # 1 = R, 0 = N.
         HYP009_denom_RN7 = if_else(HYPPCADEC_DAT >= QSSD, 1, 0, missing = 0), # 1 = R, 0 = N
         HYP009_denom_RN8_SS1 = if_else(CLINBPLAT_DAT >= QSSD &   # subsection 1 for rule 8
                                          (CLINBPSYSLAT_VAL > 150 | CLINBPDIALAT_VAL > 90) &
                                          CLHMAMBBPLAT_DAT == CLINBPLAT_DAT &
                                          HYPINVITE1_DAT > CLINBPLAT_DAT & 
                                          !is.na(HYPINVITE2_DAT), 1, 0, missing = 0), # 1/0 = meets/doesn't meet SS1 criteria
         HYP009_denom_RN8_SS2 = if_else(HOMEAMBBPLAT_DAT >= QSSD &   # subsection 2 for rule 8
                                          (HOMEAMBBPSYSLAT_VAL > 145 | HOMEAMBBPDIALAT_VAL > 85) &
                                          CLHMAMBBPLAT_DAT == HOMEAMBBPLAT_DAT &
                                          HYPINVITE1_DAT > HOMEAMBBPLAT_DAT & 
                                          !is.na(HYPINVITE2_DAT), 1, 0, missing = 0), # 1/0 = meets/doesn't meet SS2 criteria
         HYP009_denom_RN8_SS3 = if_else(!is.na(HYPINVITE2_DAT) &
                                          (is.na(CLHMAMBBPLAT_DAT) | CLHMAMBBPLAT_DAT <= QSSD),
                                        1, 0, missing = 0), # 1/0 = meets/doesn't meet SS3 criteria
         HYP009_denom_RN8 = if_else(HYP009_denom_RN8_SS1 == 1 |  # any of the subsections need to be met to pass
                                      HYP009_denom_RN8_SS2 == 1 |
                                      HYP009_denom_RN8_SS3 == 1, 1, 0, missing = 0), # 1 = R, 0 = N
         HYP009_denom_RN9 = if_else(HYP_DAT >= as.Date("2023-07-01"), 1, 0, missing = 0), # 1 = R, 0 = N # PPED (/ACHV) - 9 months
         HYP009_denom_RN10 = if_else(regstartdate >= as.Date("2023-07-01"), 1, 0, missing = 0)) # 1 = R, 0 = S

# put the rules together

dat <- dat %>% mutate(HYP009_denom = if_else(HYP_REG == 1 & # registered
                                               HYP009_denom_RN1 == 0 & ( # appropriate age
                                                 HYP009_denom_RN2 == 1 | # suitably controlled bp clinical measurement
                                                   HYP009_denom_RN3 == 1 | # suitably controlled bp home measurement
                                                   (HYP009_denom_RN4 == 0 & # not kicked out for any other reason
                                                      HYP009_denom_RN5 == 0 & 
                                                      HYP009_denom_RN6 == 0 &
                                                      HYP009_denom_RN7 == 0 &
                                                      HYP009_denom_RN8 == 0 & 
                                                      HYP009_denom_RN9 == 0 & 
                                                      HYP009_denom_RN10 == 0)), 1, 0, missing = 0))

# numerator

dat <- dat %>% mutate(HYP009_nume_RN1 = HYP009_denom_RN2, # numerator rules already created for the denominator
                      HYP009_nume_RN2 = HYP009_denom_RN3)

dat <- dat %>% mutate(HYP009_nume = if_else(HYP009_denom == 1 & 
                                              (HYP009_nume_RN1 == 1 | 
                                                 HYP009_nume_RN2 == 1), 1, 0, missing = 0)) 


# the big reveal... 
sum(dat$HYP009_nume)/sum(dat$HYP009_denom)


# all seems good to me!

saveRDS(dat, "Z:/Group_work/QOF_project/Data/Hypertension/cohort/4_cohort_hypertension_with_HYP_indicators.RDS")


