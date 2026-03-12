
library(dplyr)
library(data.table)
library(bit64)

# read in each cohort
QOF_AST_QOF_ASTTRT <- readRDS("X:/QOF_project/Data/Asthma/cohort/3_cohort_asthma_with_AST_indicators.RDS")
QOF_AST_broad_ASTTRT <- readRDS("X:/QOF_project/Data/Asthma/cohort/5_cohort_asthma_with_AST_indicators_alt_ASTTRT_broad.RDS")
QOF_AST_ics_ASTTRT <- readRDS("X:/QOF_project/Data/Asthma/cohort/6_cohort_asthma_with_AST_indicators_alt_ASTTRT_ics.RDS")

alt_AST_QOF_ASTTRT <- readRDS("X:/QOF_project/Data/Asthma/cohort/8_cohort_asthma_with_AST_indicators_alt_AST_DAT.RDS")
alt_AST_broad_ASTTRT <- readRDS("X:/QOF_project/Data/Asthma/cohort/9_cohort_asthma_with_AST_indicators_alt_AST_DAT_alt_ASTTRT_broad.RDS")
alt_AST_ics_ASTTRT <- readRDS("X:/QOF_project/Data/Asthma/cohort/10_cohort_asthma_with_AST_indicators_alt_AST_DAT_alt_ASTTRT_ics.RDS")

# create a function that will label the variables for each cohort to distinguish them
add_deets <- function(dat, definition) {
  dat <- dat %>% dplyr::select(starts_with("patid"),
                               starts_with("AST_REG"),
                               starts_with("AST0"))
  colnames(dat)[-1] <- paste0(colnames(dat)[-1], "_", definition)
  return(dat)
}


QOF_AST_QOF_ASTTRT <- add_deets(QOF_AST_QOF_ASTTRT, "QA_QT") # QOF asthma QOF trtmnt (QA_QT)
QOF_AST_broad_ASTTRT <- add_deets(QOF_AST_broad_ASTTRT, "QA_BT") # QOF asthma broad trtmnt (QA_BT)
QOF_AST_ics_ASTTRT <- add_deets(QOF_AST_ics_ASTTRT, "QA_IT") # QOF asthma ICS trtmnt (QA_IT)

alt_AST_QOF_ASTTRT <- add_deets(alt_AST_QOF_ASTTRT, "AA_QT") # alt asthma QOF trtmnt (AA_QT)
alt_AST_broad_ASTTRT <- add_deets(alt_AST_broad_ASTTRT, "AA_BT") # alt asthma broad trtmnt (AA_BT)
alt_AST_ics_ASTTRT <- add_deets(alt_AST_ics_ASTTRT, "AA_IT") # alt asthma ICS trtmnt (AA_IT)

colnames(QOF_AST_QOF_ASTTRT)
colnames(QOF_AST_broad_ASTTRT)
colnames(QOF_AST_ics_ASTTRT)

colnames(alt_AST_QOF_ASTTRT)
colnames(alt_AST_broad_ASTTRT)
colnames(alt_AST_ics_ASTTRT)

nrow(QOF_AST_QOF_ASTTRT)
nrow(QOF_AST_broad_ASTTRT)
nrow(QOF_AST_ics_ASTTRT)

nrow(alt_AST_QOF_ASTTRT)
nrow(alt_AST_broad_ASTTRT)
nrow(alt_AST_ics_ASTTRT)

ncol(QOF_AST_QOF_ASTTRT)
ncol(QOF_AST_broad_ASTTRT)
ncol(QOF_AST_ics_ASTTRT)

ncol(alt_AST_QOF_ASTTRT)
ncol(alt_AST_broad_ASTTRT)
ncol(alt_AST_ics_ASTTRT)


# now that the variable names are unique we can bind the columns all together
# full_join is just to ensure we don't make a mistake

final <- QOF_AST_QOF_ASTTRT %>% 
  full_join(., QOF_AST_broad_ASTTRT, by = "patid") %>%
  full_join(., QOF_AST_ics_ASTTRT, by = "patid") %>%
  full_join(., alt_AST_QOF_ASTTRT, by = "patid") %>%
  full_join(., alt_AST_broad_ASTTRT, by = "patid") %>%
  full_join(., alt_AST_ics_ASTTRT, by = "patid")

nrow(final)

# now we create additional variables to classify the groups

final %>% select(!contains("RN")) %>% colnames() # these are all the groups we 
                                                 # want to compare

# comparisons of interest are:

# QA_QT vs all others
# AA_QT vs AA_BT
# AA_QT vs AA_IT

make_comparison <- function(dat, comparison_vars, comp1, comp2) {
  
  for (v in comparison_vars) {
    
    var1 <- paste0(v, "_", comp1)
    var2 <- paste0(v, "_", comp2)
    
    new_var <- paste0(v, "_", comp1, "_vs_", comp2, "_comparison")
    
    dat <- dat %>%
      mutate(
        !!new_var := case_when(
          is.na(.data[[var1]]) & is.na(.data[[var2]]) ~ NA_integer_,
          .data[[var1]] == 0 & .data[[var2]] == 0 ~ 0,
          .data[[var1]] == 1 & .data[[var2]] == 0 ~ 1,
          .data[[var1]] == 0 & .data[[var2]] == 1 ~ 2,
          .data[[var1]] == 1 & .data[[var2]] == 1 ~ 3
        )
      )
  }
  
  return(dat)
}

# 0 = not in either
# 1 = in first comparison only
# 2 = in second comparison only
# 3 = in both


# the variables to compare
comp_vars <- c("AST_REG", 
          "AST007_denom", "AST007_nume",
          "AST008_denom", "AST008_nume",
          "AST012_denom", "AST012_nume")


# all comparisons with QA_QT
final <- make_comparison(final, comp_vars, "QA_QT", "QA_BT")
final <- make_comparison(final, comp_vars, "QA_QT", "QA_IT")
final <- make_comparison(final, comp_vars, "QA_QT", "AA_QT")
final <- make_comparison(final, comp_vars, "QA_QT", "AA_BT")
final <- make_comparison(final, comp_vars, "QA_QT", "AA_IT")

# AA comparisons 
final <- make_comparison(final, comp_vars, "AA_QT", "AA_BT")
final <- make_comparison(final, comp_vars, "AA_QT", "AA_IT")

# you can see the comparisons here:
final %>% select(ends_with("comparison")) %>% lapply(., table, useNA = "ifany")

colnames(final)

# having the individual rule numbers for every situation is a bit much - we can 
# remove them and if we need them for a particular cohort then we can read in
# that cohort and get them.

final <- final %>% select(!contains("RN"))

saveRDS(final, "X:/QOF_project/Data/Asthma/cohort/11_cohort_asthma_with_comparison_vars.RDS")
