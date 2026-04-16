

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
comparison_OI <- "Current QOF asthma codes vs QOF asthma codes restricted to ICS treatment"
scenario_code <- "QA_IT"

# comparisons of interest, given that most differences are minimal:

# QA_QT vs AA_QT
# QA_QT vs QA_IT

# covars of interest



ind_vars_characteristics <- dat %>% select(gender, region, imd_2019_5, age, eth5, BMI_cat,
                                           gord:htn_qof, baseline_asthma_attacks) %>% 
  colnames()

ind_vars_asthma_outcomes <- dat %>% select(outcome_asthma_attacks_bin, 
                                           followup_time_pys,
                                           ends_with("QA_QT")) %>% 
  select(-starts_with("AST_REG")) %>% 
  select(-contains("inc_exc_type")) %>% colnames()



# patient characteristics table
AST_REG_QA_QT_vs_QA_IT_table <- dat %>%
  filter(AST_REG_QA_QT_vs_QA_IT_comparison != 0) %>% 
  mutate(AST_REG_QA_QT_vs_QA_IT_comparison = as.character(AST_REG_QA_QT_vs_QA_IT_comparison)) %>%
  mutate(AST_REG_QA_QT_vs_QA_IT_comparison = recode(AST_REG_QA_QT_vs_QA_IT_comparison,
                                                    `1` = "In current QOF only",
                                                    `2` = "In updated QOF only",
                                                    `3` = "In both")) %>%
  summary_factorlist(., dependent = "AST_REG_QA_QT_vs_QA_IT_comparison", 
                     explanatory = ind_vars_characteristics,
                     p = TRUE,total_col = TRUE, add_col_totals = TRUE)

ft <- flextable(AST_REG_QA_QT_vs_QA_IT_table)
ft <- fontsize(ft, size = 9, part = "all")
ft <- width(ft, j = 1, width = 2.2)      # first/left column
ft <- width(ft, j = 2:ncol(AST_REG_QA_QT_vs_QA_IT_table), width = 1.0)  # remaining columns
ft <- autofit(ft)

# this is a bit of code that can make all the documents landscape
# if added

ps_land <- prop_section(
  page_size = page_size(orient = "landscape"),
  type = "continuous"
)


doc <- read_docx() %>% #  body_end_section_landscape() %>%
  body_add_par("Current QOF asthma codes vs updated QOF asthma codes; Baseline characteristics", 
               style = "Normal") %>%
  body_add_flextable(ft) %>%
  body_end_block_section(value = block_section(property = ps_land))

print(doc, target = "Z:/Group_work/QOF_project/Alex/Output/Asthma/AST_REG_QA_QT_vs_QA_IT_table_patient_characteristics.docx")  


# outcomes table
# patient characteristics table
AST_REG_QA_QT_vs_QA_IT_table_outcomes <- dat %>%
  filter(AST_REG_QA_QT_vs_QA_IT_comparison != 0) %>% 
  filter(in_follow_up_flag == 1) %>%
  mutate(AST_REG_QA_QT_vs_QA_IT_comparison = as.character(AST_REG_QA_QT_vs_QA_IT_comparison)) %>%
  mutate(AST_REG_QA_QT_vs_QA_IT_comparison = recode(AST_REG_QA_QT_vs_QA_IT_comparison,
                                                    `1` = "In current QOF only",
                                                    `2` = "In updated QOF only",
                                                    `3` = "In both")) %>%
  summary_factorlist(., dependent = "AST_REG_QA_QT_vs_QA_IT_comparison", 
                     explanatory = ind_vars_asthma_outcomes,
                     p = TRUE,total_col = TRUE, add_col_totals = TRUE)

# manually create the rate variables for the outcomes table, as finalfit doesn't do this for us

rate_results <- dat %>% group_by(AST_REG_QA_QT_vs_QA_IT_comparison) %>%
  filter(AST_REG_QA_QT_vs_QA_IT_comparison != 0) %>%
  filter(in_follow_up_flag == 1) %>%
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
# statistically test difference in rates

p_value_temp <- poisson.test(c(rate_results$total_attacks[1], rate_results$total_attacks[3]), 
                             c(rate_results$total_person_years[1], rate_results$total_person_years[3]))$p.value
p_value <- if_else(p_value_temp < 0.001, "<0.001", sprintf("%.3f", round(p_value_temp, 3)))

# do the same but for the 'total' column
rate_results_total <- dat %>% filter(AST_REG_QA_QT_vs_QA_IT_comparison != 0) %>% 
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

AST_REG_QA_QT_vs_QA_IT_table_outcomes[4, 1] <- "Asthma attacks per 1000 person-years (95% CI)"
AST_REG_QA_QT_vs_QA_IT_table_outcomes[4, 2] <- "Overall rate (95% CI)"
AST_REG_QA_QT_vs_QA_IT_table_outcomes[4, 3] <- rate_results$rate_CI[3]
AST_REG_QA_QT_vs_QA_IT_table_outcomes[4, 4] <- rate_results$rate_CI[1]
AST_REG_QA_QT_vs_QA_IT_table_outcomes[4, 5] <- rate_results$rate_CI[2]
AST_REG_QA_QT_vs_QA_IT_table_outcomes[4, 6] <- rate_results_total$rate_CI[1]
AST_REG_QA_QT_vs_QA_IT_table_outcomes[4, 7] <- p_value

ft_outcomes <- flextable(AST_REG_QA_QT_vs_QA_IT_table_outcomes)
ft_outcomes <- fontsize(ft_outcomes, size = 9, part = "all")
ft_outcomes <- width(ft_outcomes, j = 1, width = 2.2)      # first/left column
ft_outcomes <- width(ft_outcomes, j = 2:ncol(AST_REG_QA_QT_vs_QA_IT_table_outcomes), width = 1.0)  # remaining columns
ft_outcomes <- autofit(ft_outcomes)



# and another table assessing the outcomes according to group


library(dplyr)

ASTsummaryFunction <- function(x,
                               scenario = "QA_QT",
                               group_col = NULL,
                               add_totals = c("yes", "no")) {
  
  add_totals <- match.arg(add_totals)
  
  reg_col       <- paste0("AST_REG_", scenario)
  ast007_nume   <- paste0("AST007_nume_", scenario)
  ast007_denom  <- paste0("AST007_denom_", scenario)
  ast008_nume   <- paste0("AST008_nume_", scenario)
  ast008_denom  <- paste0("AST008_denom_", scenario)
  ast012_nume   <- paste0("AST012_nume_", scenario)
  ast012_denom  <- paste0("AST012_denom_", scenario)
  
  needed_cols <- c(
    reg_col,
    ast007_nume, ast007_denom,
    ast008_nume, ast008_denom,
    ast012_nume, ast012_denom
  )
  
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
  
  summarise_ast <- function(dat) {
    dat %>%
      summarise(
        AST_REG_N = sum(.data[[reg_col]], na.rm = TRUE),
        AST007_nume_n = sum(.data[[ast007_nume]], na.rm = TRUE),
        AST007_denom_N = sum(.data[[ast007_denom]], na.rm = TRUE),
        AST008_nume_n = sum(.data[[ast008_nume]], na.rm = TRUE),
        AST008_denom_N = sum(.data[[ast008_denom]], na.rm = TRUE),
        AST012_nume_n = sum(.data[[ast012_nume]], na.rm = TRUE),
        AST012_denom_N = sum(.data[[ast012_denom]], na.rm = TRUE)
      ) %>%
      mutate(
        AST007_perc_meeting_QI = round((AST007_nume_n / AST007_denom_N) * 100, 1),
        AST008_perc_meeting_QI = round((AST008_nume_n / AST008_denom_N) * 100, 1),
        AST012_perc_meeting_QI = round((AST012_nume_n / AST012_denom_N) * 100, 1)
      ) %>%
      relocate(AST007_perc_meeting_QI, .after = AST007_denom_N) %>%
      relocate(AST008_perc_meeting_QI, .after = AST008_denom_N) %>%
      relocate(AST012_perc_meeting_QI, .after = AST012_denom_N)
  }
  
  if (is.null(group_col)) {
    out <- summarise_ast(x)
    return(out)
  }
  
  x_group <- x %>%
    group_by(.data[[group_col]]) %>%
    summarise(
      AST_REG_N = sum(.data[[reg_col]], na.rm = TRUE),
      AST007_nume_n = sum(.data[[ast007_nume]], na.rm = TRUE),
      AST007_denom_N = sum(.data[[ast007_denom]], na.rm = TRUE),
      AST008_nume_n = sum(.data[[ast008_nume]], na.rm = TRUE),
      AST008_denom_N = sum(.data[[ast008_denom]], na.rm = TRUE),
      AST012_nume_n = sum(.data[[ast012_nume]], na.rm = TRUE),
      AST012_denom_N = sum(.data[[ast012_denom]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      AST007_perc_meeting_QI = round((AST007_nume_n / AST007_denom_N) * 100, 1),
      AST008_perc_meeting_QI = round((AST008_nume_n / AST008_denom_N) * 100, 1),
      AST012_perc_meeting_QI = round((AST012_nume_n / AST012_denom_N) * 100, 1)
    ) %>%
    relocate(AST007_perc_meeting_QI, .after = AST007_denom_N) %>%
    relocate(AST008_perc_meeting_QI, .after = AST008_denom_N) %>%
    relocate(AST012_perc_meeting_QI, .after = AST012_denom_N)
  
  if (add_totals == "yes") {
    tot <- summarise_ast(x)
    tot[[group_col]] <- "Total"
    tot <- tot %>%
      select(all_of(group_col), everything())
    
    x_group <- bind_rows(tot, x_group)
  }
  
  x_group <- x_group %>%
    filter(.data[[group_col]] != 0)
  
  return(x_group)
}

QIs <- ASTsummaryFunction(dat, scenario = "QA_QT",
                          group_col = "AST_REG_QA_QT_vs_QA_IT_comparison", 
                   add_totals = "yes")

clean_AST_names <- function(df) {
  
  df %>%
    rename(
      Group = AST_REG_QA_QT_vs_QA_IT_comparison,
      `Register N` = AST_REG_N
    ) %>%
    rename_with(~ str_replace(.x, "_nume_n$", " numerator (n)")) %>%
    rename_with(~ str_replace(.x, "_denom_N$", " denominator (N)")) %>%
    rename_with(~ str_replace(.x, "_perc_meeting_QI$", " (%)"))
}

QIs <- clean_AST_names(QIs)

QIs <- QIs %>% filter(Group != "In updated QOF only")

ft_QIs <- flextable(QIs)

ft_QIs <- fontsize(ft_QIs, size = 9, part = "all")
# ft_QIs <- width(ft_QIs, j = 1, width = 2.2)      # first/left column
ft_QIs <- width(ft_QIs, j = 2:ncol(QIs), width = 1.0)  # remaining columns
#ft_QIs <- autofit(ft_QIs)


# change the scenario here each time
QIs_alt <- ASTsummaryFunction(dat, scenario = scenario_code,
                               group_col = "AST_REG_QA_QT_vs_QA_IT_comparison", 
                               add_totals = "yes")
QIs_alt <- clean_AST_names(QIs_alt)
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


print(doc_outcomes, target = "Z:/Group_work/QOF_project/Alex/Output/Asthma/AST_REG_QA_QT_vs_QA_IT_table_outcomes.docx")  

