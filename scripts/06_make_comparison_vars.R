#make comparison variables each quality indicator and it's numerator and denominator


library(dplyr)
library(data.table)
library(bit64)

# read in each cohort
QOF_COPD <- fread("X:/QOF_project/Data/COPD/working_data/1_cohort_COPD_with_QIs.csv")
VAL_COPD <- fread("X:/QOF_project/Data/COPD/working_data/1_cohort_COPD_with_QIs_COMBO2.csv")
VAL_COPD_VAL_PROC <- fread("X:/QOF_project/Data/COPD/working_data/1_cohort_COPD_with_QIs_COMBO3.csv")
VAL_COPD_RES_PROC <- fread("X:/QOF_project/Data/COPD/working_data/1_cohort_COPD_with_QIs_COMBO4.csv")
UPD_COPD <- fread("X:/QOF_project/Data/COPD/working_data/1_cohort_COPD_with_QIs_COMBO5.csv")
UPD_COPD_VAL_PROC <- fread("X:/QOF_project/Data/COPD/working_data/1_cohort_COPD_with_QIs_COMBO6.csv")
UPD_COPD_RES_PROC <- fread("X:/QOF_project/Data/COPD/working_data/1_cohort_COPD_with_QIs_COMBO7.csv")

# create a function that will label the variables for each cohort to distinguish them
add_deets <- function(dat, definition) {
  dat <- dat %>% dplyr::select(starts_with("patid"),
                               starts_with("COPD_REG"),
                               starts_with("COPD0"))
  colnames(dat)[-1] <- paste0(colnames(dat)[-1], "_", definition)
  return(dat)
}


QOF_COPD <- add_deets(QOF_COPD, "QC")                      # QOF COPD
VAL_COPD <- add_deets(VAL_COPD, "VC")                      # Validated codelist, no process codes
VAL_COPD_VAL_PROC <- add_deets(VAL_COPD_VAL_PROC, "VC_VP") # Validated codelist, validated process codes
VAL_COPD_RES_PROC <- add_deets(VAL_COPD_RES_PROC, "VC_RP") # Validated codelist, restricted process codes
UPD_COPD <- add_deets(UPD_COPD, "UC")                      # Updated codelist, no process codes
UPD_COPD_VAL_PROC <- add_deets(UPD_COPD_VAL_PROC, "UC_VP") # Updated codelist, validated process codes
UPD_COPD_RES_PROC <- add_deets(UPD_COPD_RES_PROC, "UC_RP") # Updated codelist, restricted process codes

colnames(QOF_COPD)
colnames(VAL_COPD)
colnames(VAL_COPD_VAL_PROC)
colnames(VAL_COPD_RES_PROC)
colnames(UPD_COPD)
colnames(UPD_COPD_VAL_PROC)
colnames(UPD_COPD_RES_PROC)

nrow(QOF_COPD)
nrow(VAL_COPD)
nrow(VAL_COPD_VAL_PROC)
nrow(VAL_COPD_RES_PROC)
nrow(UPD_COPD)
nrow(UPD_COPD_VAL_PROC)
nrow(UPD_COPD_RES_PROC)

ncol(QOF_COPD)
ncol(VAL_COPD)
ncol(VAL_COPD_VAL_PROC)
ncol(VAL_COPD_RES_PROC)
ncol(UPD_COPD)
ncol(UPD_COPD_VAL_PROC)
ncol(UPD_COPD_RES_PROC)


# now that the variable names are unique we can bind the columns all together
# full_join is just to ensure we don't make a mistake

final <- QOF_COPD %>% 
  full_join(., VAL_COPD, by = "patid") %>%
  full_join(., VAL_COPD_VAL_PROC, by = "patid") %>%
  full_join(., VAL_COPD_RES_PROC, by = "patid") %>%
  full_join(., UPD_COPD, by = "patid") %>%
  full_join(., UPD_COPD_VAL_PROC, by = "patid") %>%
  full_join(., UPD_COPD_RES_PROC, by = "patid")

nrow(final)

# now we create additional variables to classify the groups

final %>% select(!contains("RN")) %>% colnames() # these are all the groups we 
# want to compare

# comparisons of interest are:
# QC vs all others

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
# 1 = in QOF only
# 2 = in second comparison only
# 3 = in both


# the variables to compare
comp_vars <- c("COPD_REG", 
               "COPD010_denom", "COPD010_num",
               "COPD014_denom", "COPD014_num")


# all comparisons with QC
final <- make_comparison(final, comp_vars, "QC", "VC")
final <- make_comparison(final, comp_vars, "QC", "VC_VP")
final <- make_comparison(final, comp_vars, "QC", "VC_RP")
final <- make_comparison(final, comp_vars, "QC", "UC")
final <- make_comparison(final, comp_vars, "QC", "UC_VP")
final <- make_comparison(final, comp_vars, "QC", "UC_RP")

# you can see the comparisons here:
final %>% select(ends_with("comparison")) %>% lapply(., table, useNA = "ifany")

colnames(final)

#double check reg counts look as the should
table(final$COPD_REG_QC, useNA = "always")
table(final$COPD_REG_VC, useNA = "always")
table(final$COPD_REG_VC_VP, useNA = "always")
table(final$COPD_REG_VC_RP, useNA = "always")
table(final$COPD_REG_UC, useNA = "always")
table(final$COPD_REG_UC_VP, useNA = "always")
table(final$COPD_REG_UC_RP, useNA = "always")

# having the individual rule numbers for every situation is a bit much - we can 
# remove them and if we need them for a particular cohort then we can read in
# that cohort and get them.

final <- final %>% select(!contains("RN"))

saveRDS(final, "X:/QOF_project/Data/COPD/cohort/06_cohort_COPD_with_comparison_vars.RDS")


