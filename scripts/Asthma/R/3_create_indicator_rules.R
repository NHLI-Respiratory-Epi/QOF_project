
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

# # RN1: Select patients from the specified population who meet all of the criteria below:
#   • Patient has an asthma review on the same day as a written personalised asthma plan.
# • Patient has a recording of the number of exacerbations within the one month leading up to and including date of the asthma review. 
# • The review above was in the 12 month period up to and including the payment period end date.
# Pass all remaining patients to the next rule.

# then onto the more simple rules:

dat <- dat %>% 
  mutate(AST007_denom_RN2 = if_else(ASTPCAPU_DAT >= QSSD, 1, 0, missing = 0), # 1 = R, 0 = N # unsuitable
         AST007_denom_RN3 = if_else(ASTMONDEC_DAT >= QSSD, 1, 0, missing = 0), # 1 = R, 0 = N # declined 1
         AST007_denom_RN4 = if_else(ASTPCADEC_DAT >= QSSD, 1, 0, missing = 0), # 1 = R, 0 = N # declined 2
         AST007_denom_RN5 = if_else(!is.na(ASTINVITE1_DAT) & !is.na(ASTINVITE2_DAT), 1, 0, missing = 0), # 1 = R, 0 = N # invited
         AST007_denom_RN6 = if_else(AST_DAT >= ACHV - 93, 1, 0, missing = 0), # 1 = R, 0 = N # new diagnosis
         AST007_denom_RN7 = if_else(regstartdate >= ACHV - 93, 1, 0, missing = 0)) # 1 = R, 0 = S # newly registered

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

summary(dat$PAT_AGE)

dat <- dat %>% 
  mutate(AST008_denom_RN1 = if_else(floor(PAT_AGE) > 19, 1, 0, missing = 0), # 1 = R, 0 = N,
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


# AST012*  (*but diagnosis reflects 1st April 2023, not 2025 as stated)


# denominator

dat <- dat %>% 
  mutate(AST012_denom_RN1 = if_else(EUNRESAST_DAT >= "2023-04-01", 1, 0, missing = 0), # 1 = N, 0 = R
         AST012_denom_RN2 = if_else(floor(PAT_AGE) > 16 &                # 1 = S, 0 = N
                                      (FBC_DAT <= EUNRESAST_DAT + 93 |
                                         FENODIAG_DAT <= EUNRESAST_DAT + 93 | 
                                         ASTSPIRDIAG_DAT <= EUNRESAST_DAT + 93 | 
                                         (!is.na(SPIRPCASU_DAT) & PEFRVAR_DAT <= EUNRESAST_DAT + 93) |
                                         BRONCCHALENG_DAT <= EUNRESAST_DAT + 93), 1, 0, missing = 0),
         AST012_denom_RN3 = if_else(floor(PAT_AGE) >= 5 & floor(PAT_AGE) <= 16 & # 1 = S, 0 = N
                                      (FENODIAG_DAT <= EUNRESAST_DAT + 93 |
                                      ASTSPIRDIAG_DAT <= EUNRESAST_DAT + 93 |
                                      (!is.na(SPIRPCASU_DAT) & PEFRVAR_DAT <= EUNRESAST_DAT + 93) |
                                      SKINTEST_DAT <= EUNRESAST_DAT + 93 |
                                      (IGE_DAT <= EUNRESAST_DAT + 93 & FBC_DAT <= EUNRESAST_DAT + 93) |
                                      BRONCCHALENG_DAT <= EUNRESAST_DAT + 93), 1, 0, missing = 0),
         AST012_denom_RN4 = if_else(ASTPCAPU_DAT >= QSSD, 1, 0, missing = 0), # 1 = R, 0 = N,
         AST012_denom_RN5 = if_else(ASTMONDEC_DAT >= QSSD, 1, 0, missing = 0), # 1 = R, 0 = N,
         AST012_denom_RN6 = if_else(ASTPCADEC_DAT >= QSSD, 1, 0, missing = 0), # 1 = R, 0 = N,
         AST012_denom_RN7 = if_else(!is.na(ASTINVITE1_DAT) & !is.na(ASTINVITE2_DAT), 1, 0, missing = 0), # 1 = R, 0 = N
         AST012_denom_RN8 = if_else(AST_DAT >= ACHV - 93, 1, 0, missing = 0), # 1 = R, 0 = N
         AST012_denom_RN9 = if_else(regstartdate >= ACHV - 93, 1, 0, missing = 0)) # 1 = R, 0 = S

dat <- dat %>% mutate(AST012_denom = if_else(AST_REG == 1 & AST012_denom_RN1 == 1 & # registered and meets age criteria and
                                               (AST012_denom_RN2 == 1 |   # meets QI criteria for >16s or
                                                  AST012_denom_RN3 == 1 | # meets QI criteria for 5-16s or
                                                  (AST012_denom_RN4 == 0 & # or is not ejected by this process
                                                     AST012_denom_RN5 == 0 &
                                                     AST012_denom_RN6 == 0 &
                                                     AST012_denom_RN7 == 0 &
                                                     AST012_denom_RN8 == 0 &
                                                     AST012_denom_RN9 == 0)), 1, 0, missing = 0))
# numerator is just rule 2 and 3 of denominator

dat <- dat %>% mutate(AST012_nume_RN1 = AST012_denom_RN2,
                      AST012_nume_RN2 = AST012_denom_RN3)

dat <- dat %>% mutate(AST012_nume = if_else(AST_REG == 1 & AST012_denom == 1 &
                                              (AST012_nume_RN1 == 1 | AST012_nume_RN2), 1, 0, missing = 0))

# the big reveal...
sum(dat$AST012_nume)/sum(dat$AST012_denom)

# dat %>% filter(AST_REG == 1) %>% select(AST_REG, AST007_nume, AST007_denom, AST008_nume, AST008_denom, AST012_nume, AST012_denom) %>% summary()

# separate out the rules according to the alternate order so that we can see why
# patients are removed at each step and compare their characteristics

# as long as we don't change the nume and denom columns we can still save this in the same 
# cohort file and not mess anything up downstream 

# AST007
dat <- dat %>% mutate(AST007_inc_exc_type = case_when(
  AST007_denom_RN1 == 1 & AST_REG == 1 ~ "Included in numerator",
  AST_REG == 0 | AST007_denom_RN6 == 1 | AST007_denom_RN7 == 1 ~ NA_character_, # not included in either because not registered or newly registered or new diagnosis
  AST007_denom_RN2 == 1 ~ "Rejected as unsuitable",
  AST007_denom_RN3 == 1 | AST007_denom_RN4 == 1 ~ "Rejected as declined care",
  AST007_denom_RN5 == 1 ~ "Rejected as invited",
  TRUE ~ "Included in denominator"
))

#
table(dat$AST007_inc_exc_type) # check that these roughly match Sankey diagram
# (won't be exact as order is changed)

# make factor
dat$AST007_inc_exc_type <- factor(dat$AST007_inc_exc_type, 
                                  levels = c("Included in numerator",
                                             "Included in denominator",
                                             "Rejected as unsuitable",
                                             "Rejected as declined care",
                                             "Rejected as invited"))



# AST008
dat <- dat %>% mutate(AST008_inc_exc_type = case_when(
  AST008_denom_RN1 == 1 ~ NA_character_, # too old
  AST_REG == 1 & (AST008_denom_RN2 == 1 | AST008_denom_RN3 == 1 | AST008_denom_RN4 == 1) ~ "Included in numerator",
  AST_REG == 0 | AST008_denom_RN10 == 1 | AST008_denom_RN11 == 1 ~ NA_character_, # not included in either because not registered or newly registered or new diagnosis
  AST008_denom_RN5 == 1 ~ "Rejected as unsuitable",
  AST008_denom_RN6 == 1 | AST008_denom_RN7 == 1 | AST008_denom_RN8 == 1 ~ "Rejected as declined care",
  AST008_denom_RN9 == 1 ~ "Rejected as invited",
  TRUE ~ "Included in denominator"
))

table(dat$AST008_inc_exc_type) # check that these roughly match Sankey diagram

# make factor
dat$AST008_inc_exc_type <- factor(dat$AST008_inc_exc_type, 
                                  levels = c("Included in numerator",
                                             "Included in denominator",
                                             "Rejected as unsuitable",
                                             "Rejected as declined care",
                                             "Rejected as invited"))

# AST012
dat <- dat %>% mutate(AST012_inc_exc_type = case_when(
  AST012_denom_RN1 == 0 ~ NA_character_, # not newly diagnosed
  AST_REG == 1 & (AST012_denom_RN2 == 1 | AST012_denom_RN3 == 1) ~ "Included in numerator",
  AST_REG == 0 | AST012_denom_RN8 == 1 | AST012_denom_RN9 == 1 ~ NA_character_, # not included in either because not registered or newly registered or new diagnosis
  AST012_denom_RN4 == 1 ~ "Rejected as unsuitable",
  AST012_denom_RN5 == 1 | AST012_denom_RN6 == 1 ~ "Rejected as declined care",
  AST012_denom_RN7 == 1 ~ "Rejected as invited",
  TRUE ~ "Included in denominator"
))



table(dat$AST012_inc_exc_type) # check that these roughly match Sankey diagram

# make factor
dat$AST012_inc_exc_type <- factor(dat$AST012_inc_exc_type, 
                                  levels = c("Included in numerator",
                                             "Included in denominator",
                                             "Rejected as unsuitable",
                                             "Rejected as declined care",
                                             "Rejected as invited"))





saveRDS(dat, "Z:/Group_work/QOF_project/Data/Asthma/cohort/3_cohort_asthma_with_AST_indicators.RDS")
