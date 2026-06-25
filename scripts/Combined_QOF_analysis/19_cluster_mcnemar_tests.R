
library(dplyr)
library(data.table)
library(bit64)
library(glmmTMB)
library(parallel)

dat <- readRDS("Z:/Group_work/QOF_project/Data/cohort/07_finalised_cohort_with_QI_comps_added.RDS")

# only need the register variables

dat <- dat %>% select(patid, pracid, contains("REG"))



run_multi_mcnemar <- function(scenario_comp, scenario_baseline, dat) {

    dat1 <- dat %>% rename(REG = all_of(scenario_baseline)) %>%
    select(patid, pracid, REG) %>% mutate(scenario = scenario_baseline)
  dat2 <- dat %>% rename(REG = all_of(scenario_comp)) %>%
    select(patid, pracid, REG) %>% mutate(scenario = scenario_comp)
  dat3 <- bind_rows(dat1, dat2) %>% arrange(pracid, patid) %>%
    mutate(scenario = factor(scenario, levels = c(scenario_baseline, scenario_comp)))

  mod <- glmmTMB::glmmTMB(REG ~ scenario + (1 | pracid) + (1 | pracid:patid), 
                          data = dat3,
                          family = binomial(link = "logit"),
                          control = glmmTMBControl(parallel = 10))

  filename <- paste0("X:/QOF_project/Alex/Output/output/Mcnemar/", scenario_comp, "_vs_", 
                  scenario_baseline, ".txt")
  sink(filename)
  print(summary(mod))
  sink()

  return(summary(mod))
  
}


detectCores()
ncores <- 6

# Create a cluster
cl <- makeCluster(ncores)

# we should parallelise this process

clusterEvalQ(cl, { # these need to be read into each cluster
  library(data.table)
  library(dplyr)
  library(bit64)
  library(glmmTMB)
})


# these R objects are necessary in each cluster
clusterExport(cl, varlist = c("dat", "run_multi_mcnemar"), envir = environment())

# COPD
COPD_REG_comp_list <- dat %>% select(starts_with("COPD_REG")) %>% 
  select(-contains("comparison")) %>% select(-COPD_REG_QC) %>% colnames()

big_COPD <- parLapply(cl, COPD_REG_comp_list, function(x) {
  run_multi_mcnemar(x, scenario_baseline = "COPD_REG_QC", dat = dat)
})

# Asthma
AST_REG_comp_list <- dat %>% select(starts_with("AST_REG")) %>% 
  select(-contains("comparison")) %>% select(-AST_REG_QA_QT) %>% colnames()

big_AST <- parLapply(cl, AST_REG_comp_list, function(x) {
  run_multi_mcnemar(x, scenario_baseline = "AST_REG_QA_QT", dat = dat)
})
  
  
# Hypertension

HYP_REG_comp_list <- dat %>% select(starts_with("HYP_REG")) %>% 
  select(-contains("comparison")) %>% select(-HYP_REG_QC_QR) %>% colnames()

big_HYP <- parLapply(cl, HYP_REG_comp_list, function(x) {
  run_multi_mcnemar(x, scenario_baseline = "HYP_REG_QC_QR", dat = dat)
})


stopCluster(cl)
