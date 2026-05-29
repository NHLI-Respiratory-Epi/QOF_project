


library(dplyr)
library(data.table)
library(bit64)
library(finalfit)
library(officer)
library(flextable)
library(ggplot2)
library(stringr)
library(tidyr)

dat <- readRDS("Z:/Group_work/QOF_project/Data/cohort/50_temp_cohort_asthma_QI_comps_added.RDS")


cols <- dat %>% select(starts_with("COPD_REG")) %>% select(contains("vs")) %>% colnames()

tables <- dat %>% select(starts_with("COPD_REG")) %>% select(contains("vs")) %>%
  filter(!if_all(starts_with("COPD"), ~ . == 0)) %>%
  summarise(across(all_of(cols), ~ list(table(.)))) %>% as.list()


library(dplyr)
library(tidyr)
library(stringr)

pretty_tables <- dat %>%
  select(starts_with("COPD_REG")) %>%
  select(contains("vs")) %>%
  filter(!if_all(everything(), ~ . == 0)) %>%
  pivot_longer(
    cols = everything(),
    names_to = "comparison",
    values_to = "code"
  ) %>%
  filter(code != 0) %>%
  count(comparison, code, name = "n") %>%
  group_by(comparison) %>%
  mutate(
    percent = round(100 * n / sum(n), 1),
    label = recode(
      as.character(code),
      "1" = "Current QOF only",
      "2" = "Updated QOF only",
      "3" = "Both"
    ),
    value = paste0(n, " (", percent, "%)")
  ) %>%
  ungroup() %>%
  
  mutate(
    comp_clean = comparison %>%
      str_remove("^COPD_REG_") %>%
      str_remove("_comparison$"),
    
    comp_full = comp_clean %>%
      str_replace_all("QC", "QOF COPD") %>%
      str_replace_all("VC", "Validated COPD") %>%
      str_replace_all("UC", "Updated COPD") %>%
      str_replace_all("VP", "validated process codes <2 years") %>%
      str_replace_all("RP", "restricted process codes <2 years") %>%
      str_replace_all("_vs_", " vs ") %>%
      str_replace_all("_", ", ")
  ) %>%
  
  select(Comparison = comp_full, 
         Comparison_code = comp_clean, label, value) %>%
  pivot_wider(
    names_from = label,
    values_from = value
  ) %>%
  mutate(row_id = row_number()) %>%
  arrange(desc(row_id)) %>%
  select(-row_id)

pretty_tables

write.csv(pretty_tables, 
          "Z:/Group_work/QOF_project/Alex/Output/COPD/COPD scenarios overview.csv",
          row.names = FALSE)
