# create categorical variable for each indicator with the following levels:
#     -numerator
#     -included in the denom only (baseline)
#     -rejected - unsuitable
#     -rejected - declined
#     -rejected - didn't respond to invites


library(dplyr)
library(data.table)
library(bit64)
library(tableone)

#--------------- read in data with rules columns
dat <- fread("X:/QOF_project/Data/COPD/working_data/1_cohort_COPD_with_indicators_Sankey.csv")

# ensure they are in the integer format
dat <- dat %>% mutate(across(COPD_REG_RN1:COPD014_num, as.integer))

str(dat)


#------ COPD010 QI - Classify patients according to excluded for being unsuitable, excluded for declining,
#                   excluded for not responding to invites, included in the denominator, included in the numerator
#    Select the numerator first as originally done in QOF rule order
#    Then apply rules that aren't of interest first (e.g. out of age bracket) and then apply rest of rules in order above
#
# Current COPD010 rules and order
# "Selected rule 1: COPD review with record of exacerbations and MRC score" ******* numerator *******
# "Rejected rule 2: Unsuitable for QI care"
# "Rejected rule 3: Declined QI care"
# "Rejected rule 4: Did not respond to COPD care review invitations"
# "Rejected rule 5: Newly diagnosed COPD <3 months"
# "Rejected rule 6: Registered at practice <3 months"
# "Selected rule 6: Remainder in denominator")


# COPD010
# create categorical var: 1 = numerator, 2 = denominator only, 3 = unsuitable, 4 = declined, 5 = invites
dat <- dat %>% mutate(COPD010_inc_exc_type = case_when(
  COPD_REG == 1 & COPD010_RN1 == 1 ~ "Included in numerator",
  COPD_REG == 0 | COPD010_RN5 == 1 | COPD010_RN6 == 1 ~ NA_character_, # not included in either because not registered or newly registered or new diagnosis
  COPD010_RN2 == 1 ~ "Rejected as unsuitable",
  COPD010_RN3 == 1 ~ "Rejected as declined care",
  COPD010_RN4 == 1 ~ "Rejected as invited",
  TRUE ~ "Included in denominator"
))

table(dat$COPD010_inc_exc_type) # check that these roughly match Sankey diagram
# (won't be exact as order is changed)

# make factor
dat$COPD010_inc_exc_type <- factor(dat$COPD010_inc_exc_type, 
                                  levels = c("Included in numerator",
                                             "Included in denominator",
                                             "Rejected as unsuitable",
                                             "Rejected as declined care",
                                             "Rejected as invited"))



#------ COPD014 QI - Classify patients according to excluded for being unsuitable, excluded for declining,
#                   excluded for not responding to invites, included in the denominator, included in the numerator
#    Select the numerator first as originally done in QOF rule order
#    Then apply rules that aren't of interest first (e.g. out of age bracket) and then apply rest of rules in order above
#    Going to combine some of the unsuitable and declined rules for this QI
#
# Current COPD010 rules and order
# "Rejected rule 1: No MRC Score >= 3 in preceding 12 months"
# "Rejected rule 2: Attended PR between diagnosis date and MRC Score >= 3 date"
# "Selected rule 3: Referred to PR on/after MRC Score >= 3" ******* numerator *******
# "Rejected rule 4: COPD service or PR service was unavailable"
# "Rejected rule 5: Unsuitable for PR"
# "Rejected rule 6: Unsuitable for QI care"
# "Rejected rule 7: Declined PR service referral"
# "Rejected rule 8: Declined QI care"
# "Rejected rule 9: Did not respond to COPD care review invitations"
# "Rejected rule 10: Newly diagnosed COPD <3 months"
# "Rejected rule 11: Registered at practice <3 months"
# "Selected rule 11: Remainder in denominator")


# COPD014
#create categorical var: 1 = numerator, 2 = denominator only, 
#                        3 = service unavailable (ADDED CATEGORY FOR THIS QI), 
#                        4 = unsuitable, 5 = declined, 6 = invited
dat <- dat %>% mutate(COPD014_inc_exc_type = case_when(
  COPD014_RN1 == 0 | COPD014_RN2 == 1 ~ NA_character_, # no MRC score >= 3 or already attended PR; 0 = rejected for Rule 1
  COPD_REG == 1 & COPD014_RN3 == 1 ~ "Included in numerator",
  COPD_REG == 0 | COPD014_RN10 == 1 | COPD014_RN11 == 1 ~ NA_character_, # not included in either because not registered or newly registered or new diagnosis
  COPD014_RN4 == 1 ~ "Rejected as service unavailable",
  COPD014_RN5 == 1 | COPD014_RN6 == 1 ~ "Rejected as unsuitable",
  COPD014_RN7 == 1 | COPD014_RN8 == 1 ~ "Rejected as declined care",
  COPD014_RN9 == 1 ~ "Rejected as invited",
  TRUE ~ "Included in denominator"
))

table(dat$COPD014_inc_exc_type) # check that these roughly match Sankey diagram

# make factor
dat$COPD014_inc_exc_type <- factor(dat$COPD014_inc_exc_type, 
                                  levels = c("Included in numerator",
                                             "Included in denominator",
                                             "Rejected as service unavailable",
                                             "Rejected as unsuitable",
                                             "Rejected as declined care",
                                             "Rejected as invited"))


#add onto cohort
cohort <- readRDS("X:/QOF_project/Data/COPD/cohort/06_cohort_COPD_with_comparison_vars.RDS")
cohort <- left_join(cohort, dplyr::select(dat, patid, COPD010_inc_exc_type, COPD014_inc_exc_type), by = "patid")
saveRDS(cohort, "X:/QOF_project/Data/COPD/cohort/06_cohort_COPD_with_comparison_vars2.RDS")






  













