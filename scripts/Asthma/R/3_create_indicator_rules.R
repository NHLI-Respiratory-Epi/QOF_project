
library(dplyr)
library(data.table)
library(bit64)


dat <- readRDS("Z:/Group_work/QOF_project/Data/Asthma/cohort/2_cohort_asthma_with_correct_DAT_cols.RDS")


# create the rules

# R = reject
# S = select
# N = next rule

#---------------------------------# AST_REG

dat <- dat %>% mutate(AST_REG_RN1 = if_else(!is.na(ASTLAT_DAT) & !is.na(ASTTRT_DAT),
                                         1, 0), # 1 = N, 0 = R
                      AST_REG_RN2 = if_else(is.na(ASTRES_DAT), 1, 0), # 1 = N, 0 = R
                      AST_REG_RN3 = if_else(PAT_AGE >= 6, 1, 0)) # 1 = S, 0 = R

dat <- dat %>% mutate(AST_REG = ifelse(AST_REG_RN1 == 1 &
                                         AST_REG_RN2 == 1 &
                                         AST_REG_RN3 == 1, 1, 0))



#------------------------------------# AST007

# surprisingly difficult first rule for this one!
# uses this function to match a particular rev dat against any writpatp date or astexacb date.

REV_match_function <- function(cohort, column_name, include_temp_cols = FALSE) {
  cohort <- cohort %>%
    mutate(
      WRITPASTP_match = as.integer(
        if_any(
          starts_with("WRITPASTP_DAT"),
          ~ .x == .data[[column_name]]
        )
      ),
      ASTEXACB_match = as.integer(
        if_any(
          starts_with("ASTEXACB_DAT"),
          ~ .x > (.data[[column_name]] - 31) & .x <= .data[[column_name]]
        )
      ),
      pass = if_else(
        WRITPASTP_match == 1L & ASTEXACB_match == 1L,
        1L, 0L, missing = 0L
      )
    ) %>%
    rename_with(~ paste0(column_name, "_pass"), pass)
  
  if (include_temp_cols == FALSE) {
    cohort <- cohort %>%
      select(-WRITPASTP_match, -ASTEXACB_match)
  }
  
  cohort
}

# then, apply the function to each REV_DAT column
dat <- REV_match_function(dat, "REV_DAT_1")
dat <- REV_match_function(dat, "REV_DAT_2")
dat <- REV_match_function(dat, "REV_DAT_3")
dat <- REV_match_function(dat, "REV_DAT_4")
dat <- REV_match_function(dat, "REV_DAT_5")
dat <- REV_match_function(dat, "REV_DAT_6")
dat <- REV_match_function(dat, "REV_DAT_7")
dat <- REV_match_function(dat, "REV_DAT_8")
dat <- REV_match_function(dat, "REV_DAT_9")

# Then, anyone who passes for a REV_DAT_pass, passes for the rule.

dat <- dat %>% mutate(AST007_denom_RN1 = as.integer(
    if_any(matches("^REV_DAT_.*_pass$"), ~ .x == 1))) # 1 = S, 0 = N

# then onto the more simple rules:

dat <- dat %>% 
  mutate(AST007_denom_RN2 = if_else(ASTPCAPU_DAT >= QSSD, 1, 0, missing = 0), # 1 = R, 0 = N
         AST007_denom_RN3 = if_else(ASTMONDEC_DAT >= QSSD, 1, 0, missing = 0), # 1 = R, 0 = N
         AST007_denom_RN4 = if_else(ASTPCADEC_DAT >= QSSD, 1, 0, missing = 0), # 1 = R, 0 = N
         AST007_denom_RN5 = if_else(!is.na(ASTINVITE1_DAT) & !is.na(ASTINVITE2_DAT), 1, 0, missing = 0), # 1 = R, 0 = N
         AST007_denom_RN6 = if_else(AST_DAT >= ACHV - 93, 1, 0, missing = 0), # 1 = R, 0 = N
         AST007_denom_RN7 = if_else(regstartdate >= ACHV - 93, 1, 0, missing = 0)) # 1 = R, 0 = S

# create the final indicator denominator

dat <- dat %>% mutate(AST007_denom = if_else(AST_REG == 1 & 
                                               (AST007_denom_RN1 == 1 | (
                                                 AST007_denom_RN2 == 0 &
                                                   AST007_denom_RN3 == 0 &
                                                   AST007_denom_RN4 == 0 &
                                                   AST007_denom_RN5 == 0 &
                                                   AST007_denom_RN6 == 0 &
                                                   AST007_denom_RN7 == 0)), 
                                             1, 0, missing = 0))

# only run this bit below if you have mice installed - you can check that your rules are
# being applied correctly by converting '0's to missing and using Mice's
# md.pattern function (there's probably a better way to do this tbh)
# 
# dat %>% filter(AST_REG == 1) %>% select(AST_REG, starts_with("AST007")) %>% 
#   mutate_all(~if_else(. == 1, 1, NA)) %>% mice::md.pattern()


# numerator is the first rule of the denominator (but also needs to remember that
# the denominator is applied to AST_REG 

dat <- dat %>% mutate(AST007_nume = ifelse(
  AST007_denom == 1, AST007_denom_RN1, 0)) # if they are in the denominator then 
                                           # it is rule 1 of the denominator, otherwise 0

# and the big reveal for quality indicator prevalence........

sum(dat$AST007_nume)/sum(dat$AST007_denom) # Not bad!!!


# now AST008: # smoking habit / secondhand smoking

# denominator:

dat <- dat %>% 
  mutate(AST008_denom_RN1 = if_else(PAT_AGE > 19, 1, 0, missing = 0), # 1 = R, 0 = N,
         AST008_denom_RN2 = if_else(SMOK_DAT >= QSSD, 1, 0, missing = 0), # 1 = S, 0 = N,
         AST008_denom_RN3 = if_else(SMOKEXPOS_DAT >= QSSD, 1, 0, missing = 0), # 1 = S, 0 = N,
         AST008_denom_RN4 = if_else(NOSMOKEXPOS_DAT >= QSSD, 1, 0, missing = 0), # 1 = S, 0 = N,
         AST008_denom_RN5 = if_else(ASTPCAPU_DAT >= QSSD, 1, 0, missing = 0), # 1 = R, 0 = N,
         AST008_denom_RN6 = if_else(SMOKSTATDEC_DAT >= QSSD, 1, 0, missing = 0), # 1 = R, 0 = N,
         AST008_denom_RN7 = if_else(ASTMONDEC_DAT >= QSSD, 1, 0, missing = 0), # 1 = R, 0 = N,
         AST008_denom_RN8 = if_else(ASTPCADEC_DAT >= QSSD, 1, 0, missing = 0), # 1 = R, 0 = N,
         AST008_denom_RN9 = if_else(!is.na(ASTINVITE1_DAT) & !is.na(ASTINVITE2_DAT), 1, 0, missing = 0), # 1 = R, 0 = N
         AST008_denom_RN10 = if_else(AST_DAT >= ACHV - 93, 1, 0, missing = 0), # 1 = R, 0 = N
         AST008_denom_RN11 = if_else(regstartdate >= ACHV - 93, 1, 0, missing = 0)) # 1 = R, 0 = S

dat <- dat %>% 
  mutate(AST008_denom = 
           if_else(AST_REG == 1 & AST008_denom_RN1 == 0 & # registered and under 19 and...
                     ((AST008_denom_RN2 == 1 | AST008_denom_RN3 == 1 |  
                         AST008_denom_RN4 == 1) | # ...meets one of first 3 rules, or
                        (AST008_denom_RN5 == 0 & # doesn't get ejected by this process (i.e. all = 0)
                           AST008_denom_RN6 == 0 &
                           AST008_denom_RN7 == 0 &
                           AST008_denom_RN8 == 0 &
                           AST008_denom_RN9 == 0 &
                           AST008_denom_RN10 == 0 &
                           AST008_denom_RN11 == 0)),
                   1, 0, missing = 0))
                       
# and create the numerator
# rules already specified for denominator so can just use the same ones
# (but note the rules numbers are different for denominator vs numerator)
# Also, need to be in the denominator to make it into the numerator

dat <- dat %>% mutate(AST008_nume_RN1 = AST008_denom_RN2,
                      AST008_nume_RN2 = AST008_denom_RN3,
                      AST008_nume_RN3 = AST008_denom_RN4)
                      
dat <- dat %>% 
  mutate(AST008_nume = 
           if_else(AST008_denom == 1 & (AST008_nume_RN1 == 1 | AST008_nume_RN2 == 1 |
                     AST008_nume_RN3 == 1), 1, 0, missing = 0)) 


# and the big reveal for quality indicator prevalence........

sum(dat$AST008_nume)/sum(dat$AST008_denom) 


# how do these people pass?

dat %>% select(starts_with("AST008_nume")) %>% summary()

dat %>% filter(AST008_denom ==1 & AST008_nume == 1) %>%
  select(PAT_AGE, starts_with("AST008_nume")) %>% summary()

dat %>% filter(floor(PAT_AGE) == 6) %>%   filter(AST008_denom ==1 & AST008_nume == 1) %>%
  select(PAT_AGE, starts_with("AST008_nume")) %>% summary()

dat %>% filter(floor(PAT_AGE) %in% 6:12) %>%   filter(AST008_nume == 1) %>%
  filter(AST008_nume_RN2 == 0 & AST008_nume_RN3 == 0) %>% nrow()

dat %>% filter(floor(PAT_AGE) %in% 6:12) %>%   filter(AST008_nume == 1) %>%
  filter(AST008_nume_RN2 == 1 | AST008_nume_RN3 == 1) %>% nrow()

# who passed the quality indicator for children's asthma
# but did not record a secondhand smoking status

# SMOK_DAT is just the latest smoking status so we can see which status they had
# by matching up the date with non/current/ex COD

colnames(dat)

smoking <- dat %>% filter(AST008_nume == 1) %>%
  filter(AST008_nume_RN2 == 0 & AST008_nume_RN3 == 0) %>%
  select(PAT_AGE, SMOK_DAT, NSMOK_COD_latest, EXSMOK_COD_latest, LSMOK_COD_latest)

smoking <- smoking %>% mutate(
  smoking_matches_never = if_else(SMOK_DAT == NSMOK_COD_latest,
                                  1, 0, missing = 0),
  smoking_matches_ex = if_else(SMOK_DAT == EXSMOK_COD_latest,
                               1, 0, missing = 0),
  smoking_matches_current = if_else(SMOK_DAT == LSMOK_COD_latest,
                                    1, 0, missing = 0))

summary(smoking)

