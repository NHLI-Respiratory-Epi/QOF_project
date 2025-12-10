
library(dplyr)
library(data.table)
library(bit64)


dat <- readRDS("Z:/Group_work/QOF_project/Data/Asthma/cohort/2_cohort_asthma_with_correct_DAT_cols.RDS")


# create the rules

# R = reject
# S = select
# N = next rule

# AST_REG

dat <- dat %>% mutate(AST_REG_RN1 = if_else(!is.na(ASTLAT_DAT) & !is.na(ASTTRT_DAT),
                                         1, 0), # 1 = NR, 0 = R
                      AST_REG_RN2 = if_else(is.na(ASTRES_DAT), 1, 0), # 1 = NR, 0 = R
                      AST_REG_RN3 = if_else(PAT_AGE >= 6, 1, 0)) # 1 = S, 0 = R

dat <- dat %>% mutate(AST_REG = ifelse(AST_REG_RN1 == 1 &
                                         AST_REG_RN2 == 1 &
                                         AST_REG_RN3 == 1, 1, 0))



# AST007

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


# numerator is the first rule of the denominator! 

dat <- dat %>% mutate(AST007_nume = AST007_denom_RN1)

# and the big reveal for quality indicator prevalence........

sum(dat$AST007_nume)/sum(dat$AST007_denom) # Not bad!!!



