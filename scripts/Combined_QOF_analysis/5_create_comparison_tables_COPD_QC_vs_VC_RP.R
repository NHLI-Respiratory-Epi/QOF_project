

library(dplyr)
library(data.table)
library(bit64)
library(finalfit)
library(officer)
library(flextable)
library(ggplot2)
library(stringr)


dat <- readRDS("Z:/Group_work/QOF_project/Data/cohort/50_temp_cohort_asthma_QI_comps_added.RDS")

# # # update table titles here!!!
# comparison_OI = comparison of interest
comparison_OI <- "Current QOF COPD codes vs validated COPD codes and restricted process codes"
scenario_code <- "VC_RP"

# comparisons of interest, given that most differences are minimal:

# QA_QT vs AA_QT
# QA_QT vs QA_IT

# covars of interest


dat$outcome_asthma_attacks_bin <- NA
dat$outcome_asthma_attacks_bin[dat$outcome_asthma_attacks_rate == 0] <- "0 asthma attacks during follow-up"
dat$outcome_asthma_attacks_bin[dat$outcome_asthma_attacks_rate > 0] <- "1+ asthma attacks during follow-up"
dat$outcome_asthma_attacks_bin <- factor(dat$outcome_asthma_attacks_bin)
summary(dat$outcome_asthma_attacks_bin)

dat$outcome_aecopd_bin <- NA
dat$outcome_aecopd_bin[dat$outcome_aecopd_rate == 0] <- "0 AECOPD during follow-up"
dat$outcome_aecopd_bin[dat$outcome_aecopd_rate > 0] <- "1+ AECOPD during follow-up"
dat$outcome_aecopd_bin <- factor(dat$outcome_aecopd_bin)
summary(dat$outcome_aecopd_bin)


ind_vars_characteristics <- dat %>% select(gender, region, age, eth5, BMI_cat,
                                           gord:htn_qof, 
                                           baseline_aecopd, baseline_asthma_attacks) %>% 
  colnames()


# patient characteristics table
COPD_REG_QC_vs_VC_RP_table <- dat %>%
  filter(COPD_REG_QC_vs_VC_RP_comparison != 0) %>% 
  mutate(COPD_REG_QC_vs_VC_RP_comparison = as.character(COPD_REG_QC_vs_VC_RP_comparison)) %>%
  mutate(COPD_REG_QC_vs_VC_RP_comparison = recode(COPD_REG_QC_vs_VC_RP_comparison,
                                               `1` = "In current QOF only",
                                               `2` = "In updated QOF only",
                                               `3` = "In both")) %>%
  summary_factorlist(., dependent = "COPD_REG_QC_vs_VC_RP_comparison", 
                     explanatory = ind_vars_characteristics,
                     p = TRUE,total_col = TRUE, add_col_totals = TRUE)

ft <- flextable(COPD_REG_QC_vs_VC_RP_table)
ft <- fontsize(ft, size = 9, part = "all")
ft <- width(ft, j = 1, width = 2.2)      # first/left column
ft <- width(ft, j = 2:ncol(COPD_REG_QC_vs_VC_RP_table), width = 1.0)  # remaining columns
ft <- autofit(ft)

# this is a bit of code that can make all the documents landscape
# if added

ps_land <- prop_section(
  page_size = page_size(orient = "landscape"),
  type = "continuous"
)


doc <- read_docx() %>% #  body_end_section_landscape() %>%
  body_add_par(paste0(comparison_OI, "; Baseline characteristics"), 
               style = "Normal") %>%
  body_add_flextable(ft) %>%
  body_end_block_section(value = block_section(property = ps_land))

print(doc, target = "Z:/Group_work/QOF_project/Alex/Output/COPD/COPD_REG_QC_vs_VC_RP_table_patient_characteristics.docx")  

# outcomes table

ind_vars_copd_outcomes <- dat %>% select(outcome_aecopd_bin, 
                                         ends_with("QC")) %>% 
  select(-starts_with("COPD_REG")) %>% colnames()

COPD_REG_QC_vs_VC_RP_table_outcomes <- dat %>%
  filter(COPD_REG_QC_vs_VC_RP_comparison != 0) %>% 
  mutate(COPD_REG_QC_vs_VC_RP_comparison = as.character(COPD_REG_QC_vs_VC_RP_comparison)) %>%
  mutate(COPD_REG_QC_vs_VC_RP_comparison = recode(COPD_REG_QC_vs_VC_RP_comparison,
                                               `1` = "In current QOF only",
                                               `2` = "In updated QOF only",
                                               `3` = "In both")) %>%
  summary_factorlist(., dependent = "COPD_REG_QC_vs_VC_RP_comparison", 
                     explanatory = ind_vars_copd_outcomes,
                     p = TRUE,total_col = TRUE, add_col_totals = TRUE)

ft_outcomes <- flextable(COPD_REG_QC_vs_VC_RP_table_outcomes)
ft_outcomes <- fontsize(ft_outcomes, size = 9, part = "all")
ft_outcomes <- width(ft_outcomes, j = 1, width = 2.2)      # first/left column
ft_outcomes <- width(ft_outcomes, j = 2:ncol(COPD_REG_QC_vs_VC_RP_table_outcomes), width = 1.0)  # remaining columns
ft_outcomes <- autofit(ft_outcomes)



# and another table assessing the outcomes according to group



library(dplyr)

COPDsummaryFunction <- function(x,
                                scenario = "QC",
                                group_col = NULL,
                                add_totals = c("yes", "no")) {
  
  add_totals <- match.arg(add_totals)
  
  reg_col      <- paste0("COPD_REG_", scenario)
  copd010_num  <- paste0("COPD010_num_", scenario)
  copd010_denom <- paste0("COPD010_denom_", scenario)
  copd014_num  <- paste0("COPD014_num_", scenario)
  copd014_denom <- paste0("COPD014_denom_", scenario)
  
  needed_cols <- c(reg_col, copd010_num, copd010_denom, copd014_num, copd014_denom)
  missing_cols <- setdiff(needed_cols, names(x))
  
  if (length(missing_cols) > 0) {
    stop("These columns are missing from x: ", paste(missing_cols, collapse = ", "))
  }
  
  if (!is.null(group_col)) {
    x[[group_col]] <- recode(
      as.character(x[[group_col]]),
      "1" = "In current QOF only",
      "2" = "In updated QOF only",
      "3" = "In both"
    )
  }
  
  summarise_copd <- function(dat) {
    dat %>%
      summarise(
        COPD_REG_N = sum(.data[[reg_col]], na.rm = TRUE),
        COPD010_nume_n = sum(.data[[copd010_num]], na.rm = TRUE),
        COPD010_denom_N = sum(.data[[copd010_denom]], na.rm = TRUE),
        COPD014_nume_n = sum(.data[[copd014_num]], na.rm = TRUE),
        COPD014_denom_N = sum(.data[[copd014_denom]], na.rm = TRUE)
      ) %>%
      mutate(
        COPD010_perc_meeting_QI = round((COPD010_nume_n / COPD010_denom_N) * 100, 1),
        COPD014_perc_meeting_QI = round((COPD014_nume_n / COPD014_denom_N) * 100, 1)
      ) %>%
      relocate(COPD010_perc_meeting_QI, .after = COPD010_denom_N) %>%
      relocate(COPD014_perc_meeting_QI, .after = COPD014_denom_N)
  }
  
  if (is.null(group_col)) {
    out <- summarise_copd(x)
    return(out)
  }
  
  x_group <- x %>%
    group_by(.data[[group_col]]) %>%
    summarise(
      COPD_REG_N = sum(.data[[reg_col]], na.rm = TRUE),
      COPD010_nume_n = sum(.data[[copd010_num]], na.rm = TRUE),
      COPD010_denom_N = sum(.data[[copd010_denom]], na.rm = TRUE),
      COPD014_nume_n = sum(.data[[copd014_num]], na.rm = TRUE),
      COPD014_denom_N = sum(.data[[copd014_denom]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      COPD010_perc_meeting_QI = round((COPD010_nume_n / COPD010_denom_N) * 100, 1),
      COPD014_perc_meeting_QI = round((COPD014_nume_n / COPD014_denom_N) * 100, 1)
    ) %>%
    relocate(COPD010_perc_meeting_QI, .after = COPD010_denom_N) %>%
    relocate(COPD014_perc_meeting_QI, .after = COPD014_denom_N)
  
  if (add_totals == "yes") {
    tot <- summarise_copd(x)
    tot[[group_col]] <- "Total"
    tot <- tot %>%
      select(all_of(group_col), everything())
    
    x_group <- bind_rows(tot, x_group)
  }
  
  x_group <- x_group %>%
    filter(.data[[group_col]] != 0)
  
  return(x_group)
}

QIs <- COPDsummaryFunction(dat, scenario = "QC",
                           group_col = "COPD_REG_QC_vs_VC_RP_comparison", 
                           add_totals = "yes")

clean_COPD_names <- function(df) {
  
  df %>%
    rename(
      Group = COPD_REG_QC_vs_VC_RP_comparison,
      `Register N` = COPD_REG_N
    ) %>%
    rename_with(~ str_replace(.x, "_nume_n$", " numerator (n)")) %>%
    rename_with(~ str_replace(.x, "_denom_N$", " denominator (N)")) %>%
    rename_with(~ str_replace(.x, "_perc_meeting_QI$", " (%)"))
}

QIs <- clean_COPD_names(QIs)
QIs <- QIs %>% filter(Group != "In updated QOF only")

ft_QIs <- flextable(QIs)
ft_QIs <- fontsize(ft_QIs, size = 9, part = "all")
# ft_QIs <- width(ft_QIs, j = 1, width = 2.2)      # first/left column
ft_QIs <- width(ft_QIs, j = 2:ncol(QIs), width = 1.0)  # remaining columns
#ft_QIs <- autofit(ft_QIs)


# change the scenario here each time
QIs_alt <- COPDsummaryFunction(dat, scenario = scenario_code,
                               group_col = "COPD_REG_QC_vs_VC_RP_comparison", 
                               add_totals = "yes")
QIs_alt <- clean_COPD_names(QIs_alt)
QIs_alt <- QIs_alt %>% filter(Group != "In current QOF only")


ft_QIs_alt <- flextable(QIs_alt)
ft_QIs_alt <- fontsize(ft_QIs_alt, size = 9, part = "all")
# ft_QIs_alt <- width(ft_QIs_alt, j = 1, width = 2.2)      # first/left column
ft_QIs_alt <- width(ft_QIs_alt, j = 2:ncol(QIs_alt), width = 1.0)  # remaining columns
#ft_QIs_alt <- autofit(ft_QIs_alt)


doc_outcomes <- read_docx() %>%
  body_add_par(paste0(comparison_OI, "; Outcomes and QIs under current QOF"), 
               style = "Normal") %>%
  body_add_flextable(ft_outcomes) %>%
  body_add_par(" ") %>%
  body_add_par("More tables next page") %>%
  body_add_break() %>%
  body_add_par("Indicators, denominators and numerators under the current QOF rules/codelists", 
               style = "Normal") %>%
  body_add_flextable(ft_QIs) %>%
  body_add_par("  ") %>%
  body_add_par("Indicators, denominators and numerators under the alternate QOF rules/codelists", 
               style = "Normal") %>%
  body_add_flextable(ft_QIs_alt) %>%
  body_end_block_section(value = block_section(property = ps_land))



print(doc_outcomes, target = "Z:/Group_work/QOF_project/Alex/Output/COPD/COPD_REG_QC_vs_VC_RP_table_outcomes.docx")  

