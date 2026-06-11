

library(dplyr)
library(data.table)
library(bit64)
library(finalfit)
library(officer)
library(flextable)
library(ggplot2)
library(stringr)
library(DHARMa)
library(glmmTMB)
library(broom)


dat <- readRDS("Z:/Group_work/QOF_project/Data/cohort/07_finalised_cohort_with_QI_comps_added.RDS")
colnames(dat)
# # # update table titles here!!!
# comparison_OI = comparison of interest
comparison_OI <- "Reason for inclusion/exclusion from AST007 (asthma review) indicator under current QOF"
# scenario_code <- "QA_IT"

# comparisons of interest, given that most differences are minimal:

# QA_QT vs AA_QT
# QA_QT vs QA_IT

# covars of interest



ind_vars_characteristics_for_table <- dat %>% select(gender, region, imd_2019_5, age, eth5, BMI_cat,
                                           gord:htn_qof, baseline_asthma_attacks) %>% 
  colnames()

ind_vars_characteristics <- dat %>% select(gender, region, imd_2019_5, age, eth5, BMI_cat,
                                           gord:htn_qof, baseline_asthma_attacks) %>% 
  colnames()

ind_vars_asthma_outcomes <- dat %>% select(outcome_asthma_attacks_bin, 
                                           followup_time_pys) %>% colnames()


dat %>% select(contains("inc_exc")) %>% head()
dat %>% select(contains("inc_exc")) %>% str()

# patient characteristics table
AST007_inc_exc_type_QA_QT_table <- dat %>%
  filter(!is.na(AST007_inc_exc_type_QA_QT)) %>% 
  summary_factorlist(., dependent = "AST007_inc_exc_type_QA_QT", 
                     explanatory = ind_vars_characteristics_for_table,
                     p = TRUE,total_col = TRUE, add_col_totals = TRUE)

writexl::write_xlsx(AST007_inc_exc_type_QA_QT_table, "Z:/Group_work/QOF_project/Alex/Output/Asthma/AST007_inc_exc_type_QA_QT_table_patient_characteristics.xlsx")


# patient characteristics requires everyone.
# patient outcomes is just a subsection.

# filter out the zeros and restrict to just the dataset we want.
dat <- dat %>% filter(in_follow_up_flag == 1 & !is.na(AST007_inc_exc_type_QA_QT))



# outcomes table
AST007_inc_exc_type_QA_QT_table_outcomes <- dat %>%
       summary_factorlist(., dependent = "AST007_inc_exc_type_QA_QT", 
                          explanatory = ind_vars_asthma_outcomes,
                          p = TRUE,total_col = TRUE, add_col_totals = TRUE)

# manually create the rate variables for the outcomes table, as finalfit doesn't do this for us

rate_results <- dat %>% group_by(AST007_inc_exc_type_QA_QT) %>%
  summarise(
    total_person_years = sum(followup_time_pys, na.rm = TRUE),
    total_attacks = sum(outcome_asthma_attacks, na.rm = TRUE)
  ) %>%
  mutate(rate_per_1000_pys = (total_attacks / total_person_years) * 1000) %>% # create CIs too
  # explain the formula for the confidence intervals - we are using a Poisson distribution to model the number of attacks,
  # so the variance is equal to the mean (total_attacks). The standard error of the rate is then sqrt(total_attacks / total_person_years^2) * 1000, 
  # and we multiply by 1.96 for the 95% confidence interval.
  mutate(CI_lower = (total_attacks / total_person_years) * 1000 - 1.96 * sqrt((total_attacks / total_person_years^2) * 1000^2),
         CI_upper = (total_attacks / total_person_years) * 1000 + 1.96 * sqrt((total_attacks / total_person_years^2) * 1000^2)) %>% # then 
  mutate(rate_CI = sprintf("%.1f (%.1f - %.1f)", round(rate_per_1000_pys, 1), round(CI_lower, 1), round(CI_upper, 1)))

# # statistically test difference in rates for 5 groups for reason for inclusion or exclusion
# # need a test that will compare rates across multiple groups - we can use a Poisson regression model for this, with the number of attacks as the outcome and the log of person-years as an offset
# model <- glm(outcome_asthma_attacks ~ AST007_inc_exc_type_QA_QT, offset = log(followup_time_pys), data = dat, family = poisson)
# # get the p-value for the overall difference in rates across the groups
# summary(model)
# p_value_temp <- summary(model)$coefficients[2, 4] # this gives the p-value for the difference in rates across the groups
# 
# 
# p_value <- if_else(p_value_temp < 0.001, "<0.001", sprintf("%.3f", round(p_value_temp, 3)))

# do the same but for the 'total' column
rate_results_total <- dat %>% filter(AST007_inc_exc_type_QA_QT != 0) %>% 
  filter(in_follow_up_flag == 1) %>%
  summarise(
    total_person_years = sum(followup_time_pys, na.rm = TRUE),
    total_attacks = sum(outcome_asthma_attacks, na.rm = TRUE)
  ) %>%
  mutate(rate_per_1000_pys = (total_attacks / total_person_years) * 1000) %>%
  mutate(CI_lower = (total_attacks / total_person_years) * 1000 - 1.96 * sqrt((total_attacks / total_person_years^2) * 1000^2),
         CI_upper = (total_attacks / total_person_years) * 1000 + 1.96 * sqrt((total_attacks / total_person_years^2) * 1000^2)) %>%
  mutate(rate_CI = sprintf("%.1f (%.1f-%.1f)", round(rate_per_1000_pys, 1), round(CI_lower, 1), round(CI_upper, 1)))

# manually replace the follow-up time row in the table with the rates we've recalculated, and add the p-value for the difference in rates

AST007_inc_exc_type_QA_QT_table_outcomes[4, 1] <- "Asthma attacks per 1000 person-years (95% CI)"
AST007_inc_exc_type_QA_QT_table_outcomes[4, 2] <- "Overall rate (95% CI)"
AST007_inc_exc_type_QA_QT_table_outcomes[4, 3] <- rate_results$rate_CI[1]
AST007_inc_exc_type_QA_QT_table_outcomes[4, 4] <- rate_results$rate_CI[2]
AST007_inc_exc_type_QA_QT_table_outcomes[4, 5] <- rate_results$rate_CI[3]
AST007_inc_exc_type_QA_QT_table_outcomes[4, 6] <- rate_results$rate_CI[4]
AST007_inc_exc_type_QA_QT_table_outcomes[4, 7] <- rate_results$rate_CI[5]
AST007_inc_exc_type_QA_QT_table_outcomes[4, 8] <- rate_results_total$rate_CI[1]

writexl::write_xlsx(AST007_inc_exc_type_QA_QT_table_outcomes, "Z:/Group_work/QOF_project/Alex/Output/Asthma/AST007_inc_exc_type_QA_QT_table_outcomes.xlsx")







# now assess the impact of inclusion/exclusion on asthma attack rate using a negative binomial model

# check there are no zeros in follow up time

summary(dat$followup_time_pys)


dat <- dat %>% select(outcome_asthma_attacks, followup_time_pys, 
                      AST007_inc_exc_type_QA_QT, pracid, 
                      all_of(ind_vars_characteristics))

summary(dat)

# make sure the reference categories are set correctly for the categorical variables
dat$eth5 <- relevel(dat$eth5, ref = "White")
dat$BMI_cat <- relevel(dat$BMI_cat, ref = "Normal weight")

# group the age variables, as more easy to interpret

summary(dat$age)
hist(dat$age)

# categorise age into 5 year bands from age 5 to 85, with a final category for 85 and above
# this will have to change according to the analysis

dat <- dat %>% mutate(age_cat = case_when(
  age >= 5 & age < 10 ~ "5-9",
  age >= 10 & age < 15 ~ "10-14",
  age >= 15 & age < 20 ~ "15-19",
  age >= 20 & age < 25 ~ "20-24",
  age >= 25 & age < 30 ~ "25-29",
  age >= 30 & age < 35 ~ "30-34",
  age >= 35 & age < 40 ~ "35-39",
  age >= 40 & age < 45 ~ "40-44",
  age >= 45 & age < 50 ~ "45-49",
  age >= 50 & age < 55 ~ "50-54",
  age >= 55 & age < 60 ~ "55-59",
  age >= 60 & age < 65 ~ "60-64",
  age >= 65 & age < 70 ~ "65-69",
  age >= 70 & age < 75 ~ "70-74",
  age >= 75 & age < 80 ~ "75-79",
  age >=80 & age <85 ~ "80-84",
   TRUE ~ "85+"
))

dat$age_cat <- factor(dat$age_cat, levels = c("5-9", "10-14", "15-19", "20-24",
                                                            "25-29", "30-34", "35-39", "40-44",
                                                            "45-49", "50-54", "55-59", 
                                                            "60-64", "65-69", "70-74", 
                                                            "75-79", "80-84", "85+"))
# largest group is 55-59 so set reference category to 55-59
dat$age_cat <- relevel(dat$age_cat, ref = "55-59")

ind_vars_characteristics[ind_vars_characteristics == "age"] <- "age_cat"

ind_vars_characteristics

library(glmmTMB)

# glmer.nb too long so try creating the model in glmmTMB

# create clean coefficient table skeleton using finalfit
coef_table_skeleton <- dat %>% 
  finalfit::finalfit(dependent = "outcome_asthma_attacks", 
                     explanatory = c("AST007_inc_exc_type_QA_QT", ind_vars_characteristics), 
                     total_col = TRUE, add_col_totals = TRUE, keep_fit_id = TRUE) 
coef_table_skeleton <- coef_table_skeleton[ ,1:3]

colnames(coef_table_skeleton)[1] <- "term"

# rename gord1 and the variables that end qof1 so that the '1' is removed
# use a regular expression to remove the '1' from the end of the variable names

coef_table_skeleton <- coef_table_skeleton %>% 
  mutate(term = str_replace(term, "gord1", "gord"),
         term = str_replace(term, "chd_qof1", "chd_qof"),
         term = str_replace(term, "pad_qof1", "pad_qof"),
         term = str_replace(term, "htn_qof1", "htn_qof"),
         term = str_replace(term, "diabetes_qof1", "diabetes_qof"),
         term = str_replace(term, "stroke_qof1", "stroke_qof"),
         term = str_replace(term, "tia_qof1", "tia_qof"),
         term = str_replace(term, "ckd_qof1", "ckd_qof"),
         term = str_replace(term, "mhealth_qof1", "mhealth_qof"),
         term = str_replace(term, "hyperchol_qof1", "hyperchol_qof"))


coef_table_skeleton_unadj <- coef_table_skeleton %>% 
  filter(str_detect(term, "AST007_inc_exc_type_QA_QT"))

#---------------------------------------------#
# mixed effects adjusted model ---------------#
#---------------------------------------------#

Sys.time()
nb_model_adj_me <- glmmTMB::glmmTMB(as.formula(paste0("outcome_asthma_attacks ~ AST007_inc_exc_type_QA_QT + ", 
                                           paste(ind_vars_characteristics, collapse = " + "),
                                           " + offset(log(followup_time_pys)) + (1|pracid)")), 
                         data = dat,
                         family = glmmTMB::nbinom2(link = "log"),
                         control = glmmTMBControl(parallel = 48))
Sys.time()
saveRDS(nb_model_adj_me, "Z:/Group_work/QOF_project/Alex/Output/Asthma/nb_model_adj_me_AST007_inc_exc_type_QA_QT.RDS")

# extract icc
icc <- performance::icc(nb_model_adj_me)
icc

# very low: 0.02 (unadjusted)

# assess assumptions
# simulateoutput <- DHARMa::simulateResiduals(nb_model_adj_me)
# simulateoutput

# create coef table for nb model. broom doesn't work on glmmtmb so have to create it manually. 

coef_table_adj_me <- summary(nb_model_adj_me)$coefficients$cond %>%
  as.data.frame() %>%
  tibble::rownames_to_column(var = "term") %>%
  dplyr::rename(estimate = Estimate, std_error = `Std. Error`, z_value = `z value`, p_value = `Pr(>|z|)`) %>%
  mutate(IRR = exp(estimate),
         IRR_CI_lower = exp(estimate - 1.96 * std_error),
         IRR_CI_upper = exp(estimate + 1.96 * std_error)) %>%
  select(term, estimate, std_error, z_value, p_value, IRR, IRR_CI_lower, IRR_CI_upper) %>% # round
  mutate(across(c(estimate, std_error, z_value, IRR, IRR_CI_lower, IRR_CI_upper), ~ round(., 2)),
         p_value = if_else(p_value < 0.001, "<0.001", sprintf("%.3f", round(p_value, 3)))) %>% # combine confidence intervals and estimate into one clean column
  mutate(IRR_CI = sprintf("%.2f (%.2f - %.2f)", IRR, IRR_CI_lower, IRR_CI_upper)) %>%
  filter(term != "(Intercept)")

# combine with the skeleton table
coef_table_adj_me_clean <- left_join(coef_table_skeleton, coef_table_adj_me, by = "term")

coef_table_adj_me_clean <- coef_table_adj_me_clean[ ,c(1, 2, 3, 11)]
coef_table_adj_me_clean$IRR_CI[is.na(coef_table_adj_me_clean$IRR_CI)] <- "-"

# paste (reference) next to the term in the term column for the reference categories
coef_table_adj_me <- coef_table_adj_me %>%
  mutate(term = case_when(
    IRR_CI == "-" ~ paste0(term, " (reference)"),
    TRUE ~ term
  ))

table(dat$age_cat)
coef_table_adj_me_clean$IRR_CI[coef_table_adj_me_clean$` ` == "Indeterminate"] <- "-"


# # write this table
# coef_table_adj_me_clean %>%  
#   writexl::write_xlsx("Z:/Group_work/QOF_project/Alex/Output/Asthma/nb_model_adj_me_AST007_inc_exc_type_QA_QT_coef_table.xlsx")


#---------------------------------------------#
# mixed effects unadjusted model ---------------#
#---------------------------------------------#

Sys.time()
nb_model_unadj_me <- glmmTMB::glmmTMB(as.formula(paste0("outcome_asthma_attacks ~ AST007_inc_exc_type_QA_QT ", 
                                                      " + offset(log(followup_time_pys)) + (1|pracid)")), 
                                    data = dat,
                                    family = glmmTMB::nbinom2(link = "log"),
                                    control = glmmTMBControl(parallel = 48))
Sys.time()
saveRDS(nb_model_unadj_me, "Z:/Group_work/QOF_project/Alex/Output/Asthma/nb_model_unadj_me_AST007_inc_exc_type_QA_QT.RDS")

# extract icc
icc <- performance::icc(nb_model_unadj_me)
icc

# very low: 0.02 (ununadjusted)

# assess assumptions
# simulateoutput <- DHARMa::simulateResiduals(nb_model_unadj_me)
# simulateoutput

# create coef table for nb model. broom doesn't work on glmmtmb so have to create it manually. 

coef_table_unadj_me <- summary(nb_model_unadj_me)$coefficients$cond %>%
  as.data.frame() %>%
  tibble::rownames_to_column(var = "term") %>%
  dplyr::rename(estimate = Estimate, std_error = `Std. Error`, z_value = `z value`, p_value = `Pr(>|z|)`) %>%
  mutate(IRR = exp(estimate),
         IRR_CI_lower = exp(estimate - 1.96 * std_error),
         IRR_CI_upper = exp(estimate + 1.96 * std_error)) %>%
  select(term, estimate, std_error, z_value, p_value, IRR, IRR_CI_lower, IRR_CI_upper) %>% # round
  mutate(across(c(estimate, std_error, z_value, IRR, IRR_CI_lower, IRR_CI_upper), ~ round(., 2)),
         p_value = if_else(p_value < 0.001, "<0.001", sprintf("%.3f", round(p_value, 3)))) %>% # combine confidence intervals and estimate into one clean column
  mutate(IRR_CI = sprintf("%.2f (%.2f - %.2f)", IRR, IRR_CI_lower, IRR_CI_upper)) %>%
  filter(term != "(Intercept)")

# combine with the skeleton table
coef_table_unadj_me_clean <- left_join(coef_table_skeleton_unadj, coef_table_unadj_me, by = "term")

coef_table_unadj_me_clean <- coef_table_unadj_me_clean[ ,c(1, 2, 3, 11)]
coef_table_unadj_me_clean$IRR_CI[coef_table_unadj_me_clean$` ` == "Included in numerator"] <- "-"


# paste (reference) next to the term in the term column for the reference categories
coef_table_unadj_me <- coef_table_unadj_me %>%
  mutate(term = case_when(
    IRR_CI == "-" ~ paste0(term, " (reference)"),
    TRUE ~ term
  ))

table(dat$age_cat)


# # write this table
# coef_table_unadj_me_clean %>% 
#   writexl::write_xlsx("Z:/Group_work/QOF_project/Alex/Output/Asthma/nb_model_unadj_me_AST007_inc_exc_type_QA_QT_coef_table.xlsx")

coef_table_me <- left_join(rename(coef_table_adj_me_clean, `Adjusted IRR` = "IRR_CI"), coef_table_unadj_me_clean[ ,c(1,4)], by = "term")

coef_table_me <- coef_table_me %>% select(-term) %>% rename(`Unadjusted IRR` = "IRR_CI")

writexl::write_xlsx(coef_table_me, "Z:/Group_work/QOF_project/Alex/Output/Asthma/AST007_inc_exc_type_coef_table_adj_unadj.xlsx")




# #---------------------------------------------#
# # fixed effects adjusted model ---------------#
# #---------------------------------------------#
# 
# Sys.time()
# nb_model_adj_fe <- glmmTMB::glmmTMB(as.formula(paste0("outcome_asthma_attacks ~ AST007_inc_exc_type_QA_QT + ", 
#                                                       paste(ind_vars_characteristics, collapse = " + "),
#                                                       " + offset(log(followup_time_pys))")), 
#                                     data = dat,
#                                     family = glmmTMB::nbinom2(link = "log"),
#                                     control = glmmTMBControl(parallel = 48))
# Sys.time()
# saveRDS(nb_model_adj_fe, "Z:/Group_work/QOF_project/Alex/Output/Asthma/nb_model_adj_fe_AST007_inc_exc_type_QA_QT.RDS")
# 
# # extract icc
# icc <- performance::icc(nb_model_adj_fe)
# icc
# 
# # very low: 0.02 (unadjusted)
# 
# # assess assumptions
# # simulateoutput <- DHARMa::simulateResiduals(nb_model_adj_fe)
# # simulateoutput
# 
# coef_table_adj_fe <- summary(nb_model_adj_fe)$coefficients$cond %>%
#   as.data.frame() %>%
#   tibble::rownames_to_column(var = "term") %>%
#   dplyr::rename(estimate = Estimate, std_error = `Std. Error`, z_value = `z value`, p_value = `Pr(>|z|)`) %>%
#   mutate(IRR = exp(estimate),
#          IRR_CI_lower = exp(estimate - 1.96 * std_error),
#          IRR_CI_upper = exp(estimate + 1.96 * std_error)) %>%
#   select(term, estimate, std_error, z_value, p_value, IRR, IRR_CI_lower, IRR_CI_upper) %>% # round
#   mutate(across(c(estimate, std_error, z_value, IRR, IRR_CI_lower, IRR_CI_upper), ~ round(., 2)),
#          p_value = if_else(p_value < 0.001, "<0.001", sprintf("%.3f", round(p_value, 3)))) %>% # combine confidence intervals and estimate into one clean column
#   mutate(IRR_CI = sprintf("%.2f (%.2f - %.2f)", IRR, IRR_CI_lower, IRR_CI_upper)) %>%
#   filter(term != "(Intercept)")
# 
# # combine with the skeleton table
# coef_table_adj_fe_clean <- left_join(coef_table_skeleton, coef_table_adj_fe, by = "term")
# 
# coef_table_adj_fe_clean <- coef_table_adj_fe_clean[ ,c(2, 3, 11)]
# coef_table_adj_fe_clean$IRR_CI[is.na(coef_table_adj_fe_clean$IRR_CI)] <- "-"
# 
# # paste (reference) next to the term in the term column for the reference categories
# coef_table_adj_fe <- coef_table_adj_fe %>%
#   mutate(term = case_when(
#     IRR_CI == "-" ~ paste0(term, " (reference)"),
#     TRUE ~ term
#   ))
# 
# table(dat$age_cat)
# coef_table_adj_fe_clean$IRR_CI[coef_table_adj_fe_clean$` ` == "Indeterminate"] <- "-"
# 
# 
# # write this table
# coef_table_adj_fe_clean %>% 
#   writexl::write_xlsx("Z:/Group_work/QOF_project/Alex/Output/Asthma/nb_model_adj_fe_AST007_inc_exc_type_QA_QT_coef_table.xlsx")
# 
# 
# 
# #---------------------------------------------#
# # fixed effects unadjusted model ---------------#
# #---------------------------------------------#
# 
# Sys.time()
# nb_model_unadj_fe <- glmmTMB::glmmTMB(as.formula(paste0("outcome_asthma_attacks ~ AST007_inc_exc_type_QA_QT",
#                                                       " + offset(log(followup_time_pys))")), 
#                                     data = dat,
#                                     family = glmmTMB::nbinom2(link = "log"),
#                                     control = glmmTMBControl(parallel = 48))
# Sys.time()
# saveRDS(nb_model_unadj_fe, "Z:/Group_work/QOF_project/Alex/Output/Asthma/nb_model_unadj_fe_AST007_inc_exc_type_QA_QT.RDS")
# 
# # extract icc
# icc <- performance::icc(nb_model_unadj_fe)
# icc
# 
# # very low: 0.02 (ununadjusted)
# 
# # assess assumptions
# # simulateoutput <- DHARMa::simulateResiduals(nb_model_unadj_fe)
# # simulateoutput
# 
# coef_table_unadj_fe <- summary(nb_model_unadj_fe)$coefficients$cond %>%
#   as.data.frame() %>%
#   tibble::rownames_to_column(var = "term") %>%
#   dplyr::rename(estimate = Estimate, std_error = `Std. Error`, z_value = `z value`, p_value = `Pr(>|z|)`) %>%
#   mutate(IRR = exp(estimate),
#          IRR_CI_lower = exp(estimate - 1.96 * std_error),
#          IRR_CI_upper = exp(estimate + 1.96 * std_error)) %>%
#   select(term, estimate, std_error, z_value, p_value, IRR, IRR_CI_lower, IRR_CI_upper) %>% # round
#   mutate(across(c(estimate, std_error, z_value, IRR, IRR_CI_lower, IRR_CI_upper), ~ round(., 2)),
#          p_value = if_else(p_value < 0.001, "<0.001", sprintf("%.3f", round(p_value, 3)))) %>% # combine confidence intervals and estimate into one clean column
#   mutate(IRR_CI = sprintf("%.2f (%.2f - %.2f)", IRR, IRR_CI_lower, IRR_CI_upper)) %>%
#   filter(term != "(Intercept)")
# 
# # combine with the skeleton table
# coef_table_unadj_fe_clean <- left_join(coef_table_skeleton_unadj, coef_table_unadj_fe, by = "term")
# 
# coef_table_unadj_fe_clean <- coef_table_unadj_fe_clean[ ,c(2, 3, 11)]
# coef_table_unadj_fe_clean$IRR_CI[is.na(coef_table_unadj_fe_clean$IRR_CI)] <- "-"
# 
# # paste (reference) next to the term in the term column for the reference categories
# coef_table_unadj_fe <- coef_table_unadj_fe %>%
#   mutate(term = case_when(
#     IRR_CI == "-" ~ paste0(term, " (reference)"),
#     TRUE ~ term
#   ))
# 
# table(dat$age_cat)
# coef_table_unadj_fe_clean$IRR_CI[coef_table_unadj_fe_clean$` ` == "Indeterminate"] <- "-"
# 
# 
# # write this table
# coef_table_unadj_fe_clean %>% 
#   writexl::write_xlsx("Z:/Group_work/QOF_project/Alex/Output/Asthma/nb_model_unadj_fe_AST007_inc_exc_type_QA_QT_coef_table.xlsx")

