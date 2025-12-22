library(dplyr)
library(data.table)
library(bit64)
library(readr)


dat <- readRDS("Z:/Group_work/QOF_project/Data/Smoking/cohort/4_cohort_smoking_with_refersssa_cols.RDS")



dat <- dat %>% 
  mutate(SMOK004_num_refersssa = if_else((SMOK004_denom_RN1 == 0) & (!is.na(REFERSSSA_DAT) & (REFERSSSA_DAT >= ACHV - 365.25*2)),
                                         1L, 0L, missing = NA_real_)) 

dat <- dat %>% 
  mutate(SMOK005_num_refersssa = if_else((SMOK004_denom_RN1 == 0) & (!is.na(REFERSSSA_DAT) & REFERSSSA_DAT >= (QSSD)),
                                         1L, 0L, missing = NA_real_)) 


##### SMOK004 numerator based on REFERSSSA criteria only

pop_004 <- dat %>%
  filter(SMOK2_REG == 1L,
    SMOK004_num_refersssa == 1L)

pop_004 %>%
  summarise(n_unique_patids = n_distinct(patid))
##### SMOK005 numerator based on REFERSSSA criteria only

pop_005 <- dat %>%
  filter(SMOK1_REG == 1L,
         SMOK005_num_refersssa == 1L)

pop_005 %>%
  summarise(n_unique_patids = n_distinct(patid))

##### Who made it according to the 6 new criteria? ######
# (RS1) have 'smoking cessation education given' without 'smoking cessation
# education declined' on the same day
# (RS2) have 'smoking cessation education referred' without 'smoking cessation
# education declined' on the same day
# (RS3) have 'referral for smoking cessation service' without 'declined smoking
# cessation service' on the same day
# (RS4) have 'patient received a smoking cessation service' without 'declined
# smoking cessation service' on the same day
# (RS5) patient declined smoking cessation education
# (RS6) declined smoking cessation service referral



##### matching function #####################################
## 1. for SCE given and SCE declined
#first made a df taking out patients who had none of these dates, left with all patients with at least one of these dates
pop_004_match1 <- pop_004 %>%
  filter(if_any(starts_with("SC_education_given"), ~ !is.na(.x)))

pop_005_match1 <- pop_005 %>%
  filter(if_any(starts_with("SC_education_given"), ~ !is.na(.x)))

REF_match_function_1 <- function(cohort, column_name, include_temp_cols = FALSE) {
  cohort <- cohort %>%
    mutate(
      SCE_given_match = as.integer(
        if_any(
          starts_with("SC_education_given"),
          ~ .x == .data[[column_name]]
        )
      ),
      pass = if_else(
        SCE_given_match == 1L, 1L, 0L, missing = 0L
      )
    ) %>%
    rename_with(~ paste0(column_name, "_pass"), pass)
  
  if (include_temp_cols == FALSE) {
    cohort <- cohort %>%
      select(-SCE_given_match)
  }
  
  cohort
}

#  then, apply the functions to SC_education_declined columns
pop_004_match1 <- REF_match_function_1(pop_004_match1, "SC_education_declined_1", include_temp_cols = FALSE)
pop_004_match1 <- REF_match_function_1(pop_004_match1, "SC_education_declined_2", include_temp_cols = FALSE)
pop_004_match1 <- REF_match_function_1(pop_004_match1, "SC_education_declined_3", include_temp_cols = FALSE)
pop_004_match1 <- REF_match_function_1(pop_004_match1, "SC_education_declined_4", include_temp_cols = FALSE)
pop_004_match1 <- REF_match_function_1(pop_004_match1, "SC_education_declined_5", include_temp_cols = FALSE)

pop_005_match1 <- REF_match_function_1(pop_005_match1, "SC_education_declined_1", include_temp_cols = FALSE)
pop_005_match1 <- REF_match_function_1(pop_005_match1, "SC_education_declined_2", include_temp_cols = FALSE)
pop_005_match1 <- REF_match_function_1(pop_005_match1, "SC_education_declined_3", include_temp_cols = FALSE)
pop_005_match1 <- REF_match_function_1(pop_005_match1, "SC_education_declined_4", include_temp_cols = FALSE)
pop_005_match1 <- REF_match_function_1(pop_005_match1, "SC_education_declined_5", include_temp_cols = FALSE)

#  Then, anyone who passes for a SC_education_declined_x_pass was not truly given SCE
pop_004_match1 <- pop_004_match1 %>% mutate(true_SCE_given = as.integer(
  !if_any(matches("^SC_education_declined_.*_pass$"), ~ .x == 1L))) 

pop_005_match1 <- pop_005_match1 %>% mutate(true_SCE_given = as.integer(
  !if_any(matches("^SC_education_declined_.*_pass$"), ~ .x == 1L)))

#results
table(pop_004_match1$true_SCE_given)
table(pop_005_match1$true_SCE_given)

    

## 2. for SCE referred and SCE declined
#first made a df taking out patients who had none of these dates, left with all patients with at least one of these dates
pop_004_match2 <- pop_004 %>%
  filter(if_any(starts_with("SC_education_referral"), ~ !is.na(.x)))

pop_005_match2 <- pop_005 %>%
  filter(if_any(starts_with("SC_education_referral"), ~ !is.na(.x)))

REF_match_function_2 <- function(cohort, column_name, include_temp_cols = FALSE) {
  cohort <- cohort %>%
    mutate(
      SCE_refd_match = as.integer(
        if_any(
          starts_with("SC_education_referral"),
          ~ .x == .data[[column_name]]
        )
      ),
      pass = if_else(
        SCE_refd_match == 1L, 1L, 0L, missing = 0L
      )
    ) %>%
    rename_with(~ paste0(column_name, "_ref_pass"), pass)

  if (include_temp_cols == FALSE) {
    cohort <- cohort %>%
      select(-SCE_refd_match)
  }

  cohort
}

# then, apply the function to SC_education_referral columns
pop_004_match2 <- REF_match_function_2(pop_004_match2, "SC_education_declined_1", include_temp_cols = FALSE)
pop_004_match2 <- REF_match_function_2(pop_004_match2, "SC_education_declined_2", include_temp_cols = FALSE)
pop_004_match2 <- REF_match_function_2(pop_004_match2, "SC_education_declined_3", include_temp_cols = FALSE)
pop_004_match2 <- REF_match_function_2(pop_004_match2, "SC_education_declined_4", include_temp_cols = FALSE)
pop_004_match2 <- REF_match_function_2(pop_004_match2, "SC_education_declined_5", include_temp_cols = FALSE)

pop_005_match2 <- REF_match_function_2(pop_005_match2, "SC_education_declined_1", include_temp_cols = FALSE)
pop_005_match2 <- REF_match_function_2(pop_005_match2, "SC_education_declined_2", include_temp_cols = FALSE)
pop_005_match2 <- REF_match_function_2(pop_005_match2, "SC_education_declined_3", include_temp_cols = FALSE)
pop_005_match2 <- REF_match_function_2(pop_005_match2, "SC_education_declined_4", include_temp_cols = FALSE)
pop_005_match2 <- REF_match_function_2(pop_005_match2, "SC_education_declined_5", include_temp_cols = FALSE)

# Then, anyone who passes for a SC_education_referral_n_pass was not truly given SCE
pop_004_match2 <- pop_004_match2 %>% mutate(true_SCE_ref = as.integer(
  !if_any(matches("^SC_education_declined_.*_ref_pass$"), ~ .x == 1L))) 

pop_005_match2 <- pop_005_match2 %>% mutate(true_SCE_ref = as.integer(
  !if_any(matches("^SC_education_declined_.*_ref_pass$"), ~ .x == 1L))) 

#results
table(pop_004_match2$true_SCE_ref)
table(pop_005_match2$true_SCE_ref)



## 3. for SCS referred and SCS declined
#first made a df taking out patients who had none of these dates, left with all patients with at least one of these dates
pop_004_match3 <- pop_004 %>%
  filter(if_any(starts_with("SCS_referral"), ~ !is.na(.x)))

pop_005_match3 <- pop_005 %>%
  filter(if_any(starts_with("SCS_referral"), ~ !is.na(.x)))


REF_match_function_3 <- function(cohort, column_name, include_temp_cols = FALSE) {
  cohort <- cohort %>%
    mutate(
      SCS_ref_match = as.integer(
        if_any(
          starts_with("SCS_referral"),
          ~ .x == .data[[column_name]]
        )
      ),
      pass = if_else(
        SCS_ref_match == 1L, 1L, 0L, missing = 0L
      )
    ) %>%
    rename_with(~ paste0(column_name, "_ref_pass"), pass)
  
  if (include_temp_cols == FALSE) {
    cohort <- cohort %>%
      select(-SCS_ref_match)
  }
  
  cohort
}

# then, apply the function to SC_education_declined columns
pop_004_match3 <- REF_match_function_3(pop_004_match3, "SCS_declined_1", include_temp_cols = FALSE)
pop_004_match3 <- REF_match_function_3(pop_004_match3, "SCS_declined_2", include_temp_cols = FALSE)
pop_004_match3 <- REF_match_function_3(pop_004_match3, "SCS_declined_3", include_temp_cols = FALSE)
pop_004_match3 <- REF_match_function_3(pop_004_match3, "SCS_declined_4", include_temp_cols = FALSE)
pop_004_match3 <- REF_match_function_3(pop_004_match3, "SCS_declined_5", include_temp_cols = FALSE)

pop_005_match3 <- REF_match_function_3(pop_005_match3, "SCS_declined_1", include_temp_cols = FALSE)
pop_005_match3 <- REF_match_function_3(pop_005_match3, "SCS_declined_2", include_temp_cols = FALSE)
pop_005_match3 <- REF_match_function_3(pop_005_match3, "SCS_declined_3", include_temp_cols = FALSE)
pop_005_match3 <- REF_match_function_3(pop_005_match3, "SCS_declined_4", include_temp_cols = FALSE)
pop_005_match3 <- REF_match_function_3(pop_005_match3, "SCS_declined_5", include_temp_cols = FALSE)

# Then, anyone who passes for a SCS_declined_n_pass was not truly given SCE
pop_004_match3 <- pop_004_match3 %>% mutate(true_SCS_ref = as.integer(
  !if_any(matches("^SCS_declined_.*_ref_pass$"), ~ .x == 1L))) 

pop_005_match3 <- pop_005_match3 %>% mutate(true_SCS_ref = as.integer(
  !if_any(matches("^SCS_declined_.*_ref_pass$"), ~ .x == 1L))) 

#results
table(pop_004_match3$true_SCS_ref)
table(pop_005_match3$true_SCS_ref)

## 4. for SCS given and SCS declined
#first made a df taking out patients who had none of these dates, left with all patients with at least one of these dates
pop_004_match4 <- pop_004 %>%
  filter(if_any(starts_with("SCS_given"), ~ !is.na(.x)))

pop_005_match4 <- pop_005 %>%
  filter(if_any(starts_with("SCS_given"), ~ !is.na(.x)))

REF_match_function_4 <- function(cohort, column_name, include_temp_cols = FALSE) {
  cohort <- cohort %>%
    mutate(
      SCS_given_match = as.integer(
        if_any(
          starts_with("SCS_given"),
          ~ .x == .data[[column_name]]
        )
      ),
      pass = if_else(
        SCS_given_match== 1L, 1L, 0L, missing = 0L
      )
    ) %>%
    rename_with(~ paste0(column_name, "_pass"), pass)
  
  if (include_temp_cols == FALSE) {
    cohort <- cohort %>%
      select(-SCS_given_match)
  }
  
  cohort
}

# then, apply the function to SC_education_declined columns
pop_004_match4 <- REF_match_function_4(pop_004_match4, "SCS_declined_1", include_temp_cols = TRUE)
pop_004_match4 <- REF_match_function_4(pop_004_match4, "SCS_declined_2", include_temp_cols = TRUE)
pop_004_match4 <- REF_match_function_4(pop_004_match4, "SCS_declined_3", include_temp_cols = TRUE)
pop_004_match4 <- REF_match_function_4(pop_004_match4, "SCS_declined_4", include_temp_cols = TRUE)
pop_004_match4 <- REF_match_function_4(pop_004_match4, "SCS_declined_5", include_temp_cols = TRUE)

pop_005_match4 <- REF_match_function_4(pop_005_match4, "SCS_declined_1", include_temp_cols = FALSE)
pop_005_match4 <- REF_match_function_4(pop_005_match4, "SCS_declined_2", include_temp_cols = FALSE)
pop_005_match4 <- REF_match_function_4(pop_005_match4, "SCS_declined_3", include_temp_cols = FALSE)
pop_005_match4 <- REF_match_function_4(pop_005_match4, "SCS_declined_4", include_temp_cols = FALSE)
pop_005_match4 <- REF_match_function_4(pop_005_match4, "SCS_declined_5", include_temp_cols = FALSE)

# anyone who passes for a SCS_declined_n_pass was not truly given SCS
pop_004_match4 <- pop_004_match4 %>% mutate(true_SCS_given = as.integer(
  !if_any(matches("^SCS_declined_.*_pass$"), ~ .x == 1L))) 

pop_005_match4 <- pop_005_match4 %>% mutate(true_SCS_given = as.integer(
  !if_any(matches("^SCS_declined_.*_pass$"), ~ .x == 1L))) 

#results
table(pop_004_match4$true_SCS_given)
table(pop_005_match4$true_SCS_given)

## 5. SCE declined
pop_004 %>%
  summarise(
    total = sum(
    across(c(SC_education_declined_1,
             SC_education_declined_2,
             SC_education_declined_3,
             SC_education_declined_4,
             SC_education_declined_5), 
           ~ !is.na(.x))
    ))

pop_005 %>%
  summarise(
    total = sum(
      across(c(SC_education_declined_1,
               SC_education_declined_2,
               SC_education_declined_3,
               SC_education_declined_4,
               SC_education_declined_5), 
             ~ !is.na(.x))
    ))

## 6. SCS declined
pop_004 %>%
  summarise(
    total = sum(
      across(c(SCS_declined_1,
               SCS_declined_2,
               SCS_declined_3,
               SCS_declined_4,
               SCS_declined_5), 
             ~ !is.na(.x))
    ))

pop_005 %>%
  summarise(
    total = sum(
      across(c(SCS_declined_1,
               SCS_declined_2,
               SCS_declined_3,
               SCS_declined_4,
               SCS_declined_5), 
             ~ !is.na(.x))
    ))







