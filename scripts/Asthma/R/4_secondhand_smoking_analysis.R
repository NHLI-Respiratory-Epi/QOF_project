
# how do these people pass?

library(data.table)
library(dplyr)
library(bit64)



dat <- readRDS("Z:/Group_work/QOF_project/Data/Asthma/cohort/3_cohort_asthma_with_AST_indicators.RDS")


# table: 

# rows: all patients aged 6-19, and then each age as a row

# cols: Denom N, Numerator N, Numerator N with smoking habit code, Numerator N with never smoker, 
# Numerator N with passive smoking code, QI pass under current rules, QI pass under proposed changes 
# (smoking habit AND passive smoking record)



smoking <- dat %>% filter(AST008_denom == 1) %>%
  mutate(AST008_passive = if_else(AST008_nume_RN2 == 1 | AST008_nume_RN3 == 1, 1, 0, missing = 0),
         never_smoker_recording = if_else(AST008_nume_RN1 == 1 & SMOK_DAT == NSMOK_COD_latest,
                                         1, 0, missing = 0),
         age = floor(PAT_AGE),
         ever_record_of_passive_smoking = if_else(!is.na(SMOKEXPOS_DAT) | !is.na(NOSMOKEXPOS_DAT), 1, 0, missing = 0),
         ever_record_of_passive_smoking_restrict_to_nume = if_else(AST008_nume == 1 & (!is.na(SMOKEXPOS_DAT) | !is.na(NOSMOKEXPOS_DAT)), 1, 0, missing = 0),
         AST008_nume_alternative = if_else(AST008_nume_RN1 == 1 & (AST008_nume_RN2 == 1 | AST008_nume_RN3 == 1), 
                                           1, 0, missing = 0),
         age_group = cut(PAT_AGE, breaks = c(5.999, 10.999, 14.999, 16.999, 20), 
                         labels = c("Age 6-10", "Age 11-14", "Age 15-16", "Age 17-19")))

agegroup <- smoking %>% 
  select(AST008_denom, age_group, AST008_nume, AST008_smoking_habit = AST008_nume_RN1, AST008_passive, never_smoker_recording,
         ever_record_of_passive_smoking, ever_record_of_passive_smoking_restrict_to_nume, AST008_nume_alternative)  %>%
  group_by(age_group) %>% summarise(AST008_denom_N = sum(AST008_denom), 
                              AST008_nume_N = sum(AST008_nume), 
                              AST008_smoking_habit_N = sum(AST008_smoking_habit),
                              never_smoker_recording_N = sum(never_smoker_recording),
                              AST008_passive_N = sum(AST008_passive),
                              ever_record_of_passive_smoking_N = sum(ever_record_of_passive_smoking),
                              ever_record_of_passive_smoking_restrict_to_nume_N = sum(ever_record_of_passive_smoking_restrict_to_nume),
                              AST008_nume_alternative_N = sum(AST008_nume_alternative)) %>%
  mutate(perc_meeting_QI = round((AST008_nume_N/AST008_denom_N)*100, 1),
         perc_meeting_QI_with_smoking_habit_recorded = round((AST008_smoking_habit_N/AST008_nume_N)*100, 1),
         perc_meeting_QI_with_passive_smoking_exposure_recorded = round((AST008_passive_N/AST008_nume_N)*100, 1),
         perc_meeting_QI_with_never_smoker_recorded = round((never_smoker_recording_N/AST008_nume_N)*100, 1),
         perc_meeting_QI_with_alternate_numerator = round((AST008_nume_alternative_N/AST008_denom_N)*100, 1),
         perc_meeting_QI_with_recording_of_passive_smoking_ever = round((ever_record_of_passive_smoking_restrict_to_nume_N/AST008_nume_N)*100, 1))
         
allgroup <- smoking %>% 
  select(AST008_denom, age_group, AST008_nume, AST008_smoking_habit = AST008_nume_RN1, AST008_passive, never_smoker_recording,
         ever_record_of_passive_smoking, ever_record_of_passive_smoking_restrict_to_nume, AST008_nume_alternative)  %>%
 summarise(AST008_denom_N = sum(AST008_denom), 
                                    AST008_nume_N = sum(AST008_nume), 
                                    AST008_smoking_habit_N = sum(AST008_smoking_habit),
                                    never_smoker_recording_N = sum(never_smoker_recording),
                                    AST008_passive_N = sum(AST008_passive),
                                    ever_record_of_passive_smoking_N = sum(ever_record_of_passive_smoking),
                                    ever_record_of_passive_smoking_restrict_to_nume_N = sum(ever_record_of_passive_smoking_restrict_to_nume),
                                    AST008_nume_alternative_N = sum(AST008_nume_alternative)) %>%
  mutate(perc_meeting_QI = round((AST008_nume_N/AST008_denom_N)*100, 1),
         perc_meeting_QI_with_smoking_habit_recorded = round((AST008_smoking_habit_N/AST008_nume_N)*100, 1),
         perc_meeting_QI_with_passive_smoking_exposure_recorded = round((AST008_passive_N/AST008_nume_N)*100, 1),
         perc_meeting_QI_with_never_smoker_recorded = round((never_smoker_recording_N/AST008_nume_N)*100, 1),
         perc_meeting_QI_with_alternate_numerator = round((AST008_nume_alternative_N/AST008_denom_N)*100, 1),
         perc_meeting_QI_with_recording_of_passive_smoking_ever = round((ever_record_of_passive_smoking_restrict_to_nume_N/AST008_nume_N)*100, 1))

togeth <- bind_rows(agegroup, allgroup)

togeth$age_group <- as.character(togeth$age_group)
togeth <- togeth[c(nrow(togeth), 1:(nrow(togeth)-1)), ] # put aggregate ages in first row

togeth$age_group[1] <- "All"

togeth <- togeth %>% select(age_group, AST008_denom_N, AST008_nume_N, perc_meeting_QI,
                  AST008_smoking_habit_N, perc_meeting_QI_with_smoking_habit_recorded,
                  never_smoker_recording_N, perc_meeting_QI_with_never_smoker_recorded,
                  AST008_passive_N, perc_meeting_QI_with_passive_smoking_exposure_recorded,
                  ever_record_of_passive_smoking_N, 
                  ever_record_of_passive_smoking_restrict_to_nume_N, 
                  perc_meeting_QI_with_recording_of_passive_smoking_ever,
                  AST008_nume_alternative_N, perc_meeting_QI_with_alternate_numerator)

# get the order into something appropriate

write.csv(togeth, "X:/QOF_project/Alex/Output/secondhand_smoking_table_rough.csv", row.names = FALSE)

