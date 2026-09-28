

library(dplyr)
library(data.table)
library(bit64)
library(finalfit)
library(officer)
library(flextable)
library(ggplot2)
library(stringr)


dat <- readRDS("Z:/Group_work/QOF_project/Data/cohort/07_finalised_cohort_with_QI_comps_added.RDS")

# # # update table titles here!!!
# comparison_OI = comparison of interest
comparison_OI <- "QC_QR vs UC_QR"
scenario_code <- "UC_QR"

# comparisons of interest, given that most differences are minimal:

# QC_QR
# UC_QR
# QC_VT
# UC_QR
# UC_NN
# UC_VT


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
                                           gord:htn_qof) %>% 
  colnames()


# patient characteristics table
HYP_REG_QC_QR_vs_UC_QR_table <- dat %>%
  filter(HYP_REG_QC_QR_vs_UC_QR_comparison != 0) %>% 
  mutate(HYP_REG_QC_QR_vs_UC_QR_comparison = as.character(HYP_REG_QC_QR_vs_UC_QR_comparison)) %>%
  mutate(HYP_REG_QC_QR_vs_UC_QR_comparison = recode(HYP_REG_QC_QR_vs_UC_QR_comparison,
                                                    `1` = "In QOF 2025-26 only",
                                                    `2` = "In post-review only",
                                                    `3` = "In both")) %>%
  summary_factorlist_clustered(., dependent = "HYP_REG_QC_QR_vs_UC_QR_comparison", 
                     explanatory = ind_vars_characteristics,
                     p = TRUE,total_col = TRUE, add_col_totals = TRUE)

# old p 
HYP_REG_QC_QR_vs_UC_QR_table$p

# p value update
HYP_REG_QC_QR_vs_UC_QR_table$p <- dat %>%
  filter(HYP_REG_QC_QR_vs_UC_QR_comparison != 0) %>% 
  filter(HYP_REG_QC_QR_vs_UC_QR_comparison != 2) %>% # don't include these in chi square test 
  mutate(HYP_REG_QC_QR_vs_UC_QR_comparison = as.character(HYP_REG_QC_QR_vs_UC_QR_comparison)) %>%
  mutate(HYP_REG_QC_QR_vs_UC_QR_comparison = recode(HYP_REG_QC_QR_vs_UC_QR_comparison,
                                                    `1` = "In QOF 2025-26 only",
                                                    `2` = "In post-review only",
                                                    `3` = "In both")) %>%
  summary_factorlist_clustered(., dependent = "HYP_REG_QC_QR_vs_UC_QR_comparison", 
                               explanatory = ind_vars_characteristics,
                               p = TRUE,total_col = TRUE, add_col_totals = TRUE) %>% pull(p)

# new p 
HYP_REG_QC_QR_vs_UC_QR_table$p

ft <- flextable(HYP_REG_QC_QR_vs_UC_QR_table)
ft <- fontsize(ft, size = 9, part = "all")
ft <- width(ft, j = 1, width = 2.2)      # first/left column
ft <- width(ft, j = 2:ncol(HYP_REG_QC_QR_vs_UC_QR_table), width = 1.0)  # remaining columns
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

print(doc, target = "Z:/Group_work/QOF_project/Alex/Output/Hypertension/HYP_REG_QC_QR_vs_UC_QR_table_patient_characteristics.docx")  
saveRDS(ft, "Z:/Group_work/QOF_project/Alex/Output/Hypertension/HYP_REG_QC_QR_vs_UC_QR_table_patient_characteristics_flextable.RDS")

# outcomes table

ind_vars_hypertension_outcomes <- dat %>% select(outcome_aecopd_bin, 
                                         ends_with("QC_QR")) %>% 
  select(-starts_with("HYP_REG")) %>% colnames()

HYP_REG_QC_QR_vs_UC_QR_table_outcomes <- dat %>%
  filter(HYP_REG_QC_QR_vs_UC_QR_comparison != 0) %>% 
  mutate(HYP_REG_QC_QR_vs_UC_QR_comparison = as.character(HYP_REG_QC_QR_vs_UC_QR_comparison)) %>%
  mutate(HYP_REG_QC_QR_vs_UC_QR_comparison = recode(HYP_REG_QC_QR_vs_UC_QR_comparison,
                                               `1` = "In QC_QR only",
                                               `2` = "In UC_QR only",
                                               `3` = "In both")) %>%
  summary_factorlist(., dependent = "HYP_REG_QC_QR_vs_UC_QR_comparison", 
                     explanatory = ind_vars_hypertension_outcomes,
                     p = TRUE,total_col = TRUE, add_col_totals = TRUE)

ft_outcomes <- flextable(HYP_REG_QC_QR_vs_UC_QR_table_outcomes)
ft_outcomes <- fontsize(ft_outcomes, size = 9, part = "all")
ft_outcomes <- width(ft_outcomes, j = 1, width = 2.2)      # first/left column
ft_outcomes <- width(ft_outcomes, j = 2:ncol(HYP_REG_QC_QR_vs_UC_QR_table_outcomes), width = 1.0)  # remaining columns
ft_outcomes <- autofit(ft_outcomes)



# and another table assessing the outcomes according to group



library(dplyr)

hypertensionsummaryFunction <- function(x,
                                scenario = "QC_QR",
                                group_col = NULL,
                                add_totals = c("yes", "no")) {
  
  add_totals <- match.arg(add_totals)
  
  reg_col      <- paste0("HYP_REG_", scenario)
  HYP008_num  <- paste0("HYP008_nume_", scenario)
  HYP008_denom <- paste0("HYP008_denom_", scenario)
  HYP009_num  <- paste0("HYP009_nume_", scenario)
  HYP009_denom <- paste0("HYP009_denom_", scenario)
  
  needed_cols <- c(reg_col, HYP008_num, HYP008_denom, HYP009_num, HYP009_denom)
  missing_cols <- setdiff(needed_cols, names(x))
  
  if (length(missing_cols) > 0) {
    stop("These columns are missing from x: ", paste(missing_cols, collapse = ", "))
  }
  
  if (!is.null(group_col)) {
    x[[group_col]] <- recode(
      as.character(x[[group_col]]),
      "1" = "In QC_QR only",
      "2" = "In UC_QR only",
      "3" = "In both"
    )
  }
  
  summarise_hypertension <- function(dat) {
    dat %>%
      summarise(
        HYP_REG_N = sum(.data[[reg_col]], na.rm = TRUE),
        HYP008_nume_n = sum(.data[[HYP008_num]], na.rm = TRUE),
        HYP008_denom_N = sum(.data[[HYP008_denom]], na.rm = TRUE),
        HYP009_nume_n = sum(.data[[HYP009_num]], na.rm = TRUE),
        HYP009_denom_N = sum(.data[[HYP009_denom]], na.rm = TRUE)
      ) %>%
      mutate(
        HYP008_perc_meeting_QI = round((HYP008_nume_n / HYP008_denom_N) * 100, 1),
        HYP009_perc_meeting_QI = round((HYP009_nume_n / HYP009_denom_N) * 100, 1)
      ) %>%
      relocate(HYP008_perc_meeting_QI, .after = HYP008_denom_N) %>%
      relocate(HYP009_perc_meeting_QI, .after = HYP009_denom_N)
  }
  
  if (is.null(group_col)) {
    out <- summarise_hypertension(x)
    return(out)
  }
  
  x_group <- x %>%
    group_by(.data[[group_col]]) %>%
    summarise(
      HYP_REG_N = sum(.data[[reg_col]], na.rm = TRUE),
      HYP008_nume_n = sum(.data[[HYP008_num]], na.rm = TRUE),
      HYP008_denom_N = sum(.data[[HYP008_denom]], na.rm = TRUE),
      HYP009_nume_n = sum(.data[[HYP009_num]], na.rm = TRUE),
      HYP009_denom_N = sum(.data[[HYP009_denom]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      HYP008_perc_meeting_QI = round((HYP008_nume_n / HYP008_denom_N) * 100, 1),
      HYP009_perc_meeting_QI = round((HYP009_nume_n / HYP009_denom_N) * 100, 1)
    ) %>%
    relocate(HYP008_perc_meeting_QI, .after = HYP008_denom_N) %>%
    relocate(HYP009_perc_meeting_QI, .after = HYP009_denom_N)
  
  if (add_totals == "yes") {
    tot <- summarise_hypertension(x)
    tot[[group_col]] <- "Total"
    tot <- tot %>%
      select(all_of(group_col), everything())
    
    x_group <- bind_rows(tot, x_group)
  }
  
  x_group <- x_group %>%
    filter(.data[[group_col]] != 0)
  
  return(x_group)
}

QIs <- hypertensionsummaryFunction(dat, scenario = "QC_QR",
                           group_col = "HYP_REG_QC_QR_vs_UC_QR_comparison", 
                           add_totals = "yes")

clean_hypertension_names <- function(df) {
  
  df %>%
    rename(
      Group = HYP_REG_QC_QR_vs_UC_QR_comparison,
      `Register N` = HYP_REG_N
    ) %>%
    rename_with(~ str_replace(.x, "_nume_n$", " numerator (n)")) %>%
    rename_with(~ str_replace(.x, "_denom_N$", " denominator (N)")) %>%
    rename_with(~ str_replace(.x, "_perc_meeting_QI$", " (%)"))
}

QIs <- clean_hypertension_names(QIs)
QIs <- QIs %>% filter(Group != "In UC_QR only")

ft_QIs <- flextable(QIs)
ft_QIs <- fontsize(ft_QIs, size = 9, part = "all")
# ft_QIs <- width(ft_QIs, j = 1, width = 2.2)      # first/left column
ft_QIs <- width(ft_QIs, j = 2:ncol(QIs), width = 1.0)  # remaining columns
#ft_QIs <- autofit(ft_QIs)


# change the scenario here each time
QIs_alt <- hypertensionsummaryFunction(dat, scenario = scenario_code,
                               group_col = "HYP_REG_QC_QR_vs_UC_QR_comparison", 
                               add_totals = "yes")
QIs_alt <- clean_hypertension_names(QIs_alt)
QIs_alt <- QIs_alt %>% filter(Group != "In QC_QR only")


ft_QIs_alt <- flextable(QIs_alt)
ft_QIs_alt <- fontsize(ft_QIs_alt, size = 9, part = "all")
# ft_QIs_alt <- width(ft_QIs_alt, j = 1, width = 2.2)      # first/left column
ft_QIs_alt <- width(ft_QIs_alt, j = 2:ncol(QIs_alt), width = 1.0)  # remaining columns
#ft_QIs_alt <- autofit(ft_QIs_alt)


doc_outcomes <- read_docx() %>%
  body_add_par(paste0(comparison_OI, "; Outcomes and QIs under QC_QR"), 
               style = "Normal") %>%
  body_add_flextable(ft_outcomes) %>%
  body_add_par(" ") %>%
  body_add_par("More tables next page") %>%
  body_add_break() %>%
  body_add_par("Indicators, denominators and numerators under the QC_QR rules/codelists", 
               style = "Normal") %>%
  body_add_flextable(ft_QIs) %>%
  body_add_par("  ") %>%
  body_add_par("Indicators, denominators and numerators under the alternate QOF rules/codelists", 
               style = "Normal") %>%
  body_add_flextable(ft_QIs_alt) %>%
  body_end_block_section(value = block_section(property = ps_land))



print(doc_outcomes, target = "Z:/Group_work/QOF_project/Alex/Output/Hypertension/HYP_REG_QC_QR_vs_UC_QR_table_outcomes.docx")  

