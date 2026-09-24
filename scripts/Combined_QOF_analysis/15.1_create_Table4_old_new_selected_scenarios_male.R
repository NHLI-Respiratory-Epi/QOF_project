
library(dplyr)
library(data.table)
library(bit64)
library(finalfit)
library(officer)
library(flextable)
library(ggplot2)
library(stringr)


# dat <- readRDS("Z:/Group_work/QOF_project/Data/cohort/07_finalised_cohort_with_QI_comps_added.RDS")
dat <- readRDS("X:/QOF_project/Data/cohort/07_finalised_cohort_with_QI_comps_added.RDS")

dat <- dat %>% filter(gender == "Male")


# [1] "patid"                                  "pracid"                                
# [3] "usualgpstaffid"                         "gender"                                
# [5] "emis_ddate"                             "regstartdate"                          
# [7] "patienttypeid"                          "regenddate"                            
# [9] "cprd_ddate"                             "dob"                                   
# [11] "lcd"                                    "region"                                
# [13] "ACHV"                                   "QSSD"                                  
# [15] "BMI"                                    "gord"                                  
# [17] "chd_qof"                                "pad_qof"                               
# [19] "stroke_qof"                             "tia_qof"                               
# [21] "diabetes_qof"                           "ckd_qof"                               
# [23] "mhealth_qof"                            "hyperchol_qof"                         
# [25] "frailty_qof"                            "htn_qof"                               
# [27] "asthma_plan_qof"                        "smokeref_qof"                          
# [29] "smokecess_qof"                          "smokecessdrugs_qof"                    
# [31] "eth19"                                  "eth5"                                  
# [33] "baseline_aecopd"                        "baseline_asthma_attacks"               
# [35] "outcome_asthma_attacks"                 "outcomesstartdate"                     
# [37] "outcomesenddate"                        "in_follow_up_flag"                     
# [39] "followup_time"                          "followup_time_pys"                     
# [41] "outcome_asthma_attacks_rate"            "outcome_asthma_attacks_rate_1000"      
# [43] "outcome_aecopd"                         "outcome_aecopd_rate"                   
# [45] "outcome_aecopd_rate_1000"               "e2019_imd_10"                          
# [47] "imd_2019_10"                            "imd_2019_5"                            
# [49] "age"                                    "BMI_cat"                               
# [51] "outcome_asthma_attacks_bin"             "outcome_aecopd_bin"                    
# [53] "AST_REG_QA_QT"                          "AST007_denom_QA_QT"                    
# [55] "AST007_nume_QA_QT"                      "AST008_denom_QA_QT"                    
# [57] "AST008_nume_QA_QT"                      "AST012_denom_QA_QT"                    
# [59] "AST012_nume_QA_QT"                      "AST007_inc_exc_type_QA_QT"             
# [61] "AST008_inc_exc_type_QA_QT"              "AST012_inc_exc_type_QA_QT"             
# [63] "AST008_nume_alt_QA_QT"                  "AST_REG_QA_BT"                         
# [65] "AST007_denom_QA_BT"                     "AST007_nume_QA_BT"                     
# [67] "AST008_denom_QA_BT"                     "AST008_nume_QA_BT"                     
# [69] "AST012_denom_QA_BT"                     "AST012_nume_QA_BT"                     
# [71] "AST008_nume_alt_QA_BT"                  "AST_REG_QA_IT"                         
# [73] "AST007_denom_QA_IT"                     "AST007_nume_QA_IT"                     
# [75] "AST008_denom_QA_IT"                     "AST008_nume_QA_IT"                     
# [77] "AST012_denom_QA_IT"                     "AST012_nume_QA_IT"                     
# [79] "AST008_nume_alt_QA_IT"                  "AST_REG_AA_QT"                         
# [81] "AST007_denom_AA_QT"                     "AST007_nume_AA_QT"                     
# [83] "AST008_denom_AA_QT"                     "AST008_nume_AA_QT"                     
# [85] "AST012_denom_AA_QT"                     "AST012_nume_AA_QT"                     
# [87] "AST008_nume_alt_AA_QT"                  "AST_REG_AA_BT"                         
# [89] "AST007_denom_AA_BT"                     "AST007_nume_AA_BT"                     
# [91] "AST008_denom_AA_BT"                     "AST008_nume_AA_BT"                     
# [93] "AST012_denom_AA_BT"                     "AST012_nume_AA_BT"                     
# [95] "AST008_nume_alt_AA_BT"                  "AST_REG_AA_IT"                         
# [97] "AST007_denom_AA_IT"                     "AST007_nume_AA_IT"                     
# [99] "AST008_denom_AA_IT"                     "AST008_nume_AA_IT"                     
# [101] "AST012_denom_AA_IT"                     "AST012_nume_AA_IT"                     
# [103] "AST008_nume_alt_AA_IT"                  "AST_REG_QA_QT_vs_QA_BT_comparison"     
# [105] "AST007_denom_QA_QT_vs_QA_BT_comparison" "AST007_nume_QA_QT_vs_QA_BT_comparison" 
# [107] "AST008_denom_QA_QT_vs_QA_BT_comparison" "AST008_nume_QA_QT_vs_QA_BT_comparison" 
# [109] "AST012_denom_QA_QT_vs_QA_BT_comparison" "AST012_nume_QA_QT_vs_QA_BT_comparison" 
# [111] "AST_REG_QA_QT_vs_QA_IT_comparison"      "AST007_denom_QA_QT_vs_QA_IT_comparison"
# [113] "AST007_nume_QA_QT_vs_QA_IT_comparison"  "AST008_denom_QA_QT_vs_QA_IT_comparison"
# [115] "AST008_nume_QA_QT_vs_QA_IT_comparison"  "AST012_denom_QA_QT_vs_QA_IT_comparison"
# [117] "AST012_nume_QA_QT_vs_QA_IT_comparison"  "AST_REG_QA_QT_vs_AA_QT_comparison"     
# [119] "AST007_denom_QA_QT_vs_AA_QT_comparison" "AST007_nume_QA_QT_vs_AA_QT_comparison" 
# [121] "AST008_denom_QA_QT_vs_AA_QT_comparison" "AST008_nume_QA_QT_vs_AA_QT_comparison" 
# [123] "AST012_denom_QA_QT_vs_AA_QT_comparison" "AST012_nume_QA_QT_vs_AA_QT_comparison" 
# [125] "AST_REG_QA_QT_vs_AA_BT_comparison"      "AST007_denom_QA_QT_vs_AA_BT_comparison"
# [127] "AST007_nume_QA_QT_vs_AA_BT_comparison"  "AST008_denom_QA_QT_vs_AA_BT_comparison"
# [129] "AST008_nume_QA_QT_vs_AA_BT_comparison"  "AST012_denom_QA_QT_vs_AA_BT_comparison"
# [131] "AST012_nume_QA_QT_vs_AA_BT_comparison"  "AST_REG_QA_QT_vs_AA_IT_comparison"     
# [133] "AST007_denom_QA_QT_vs_AA_IT_comparison" "AST007_nume_QA_QT_vs_AA_IT_comparison" 
# [135] "AST008_denom_QA_QT_vs_AA_IT_comparison" "AST008_nume_QA_QT_vs_AA_IT_comparison" 
# [137] "AST012_denom_QA_QT_vs_AA_IT_comparison" "AST012_nume_QA_QT_vs_AA_IT_comparison" 
# [139] "AST_REG_AA_QT_vs_AA_BT_comparison"      "AST007_denom_AA_QT_vs_AA_BT_comparison"
# [141] "AST007_nume_AA_QT_vs_AA_BT_comparison"  "AST008_denom_AA_QT_vs_AA_BT_comparison"
# [143] "AST008_nume_AA_QT_vs_AA_BT_comparison"  "AST012_denom_AA_QT_vs_AA_BT_comparison"
# [145] "AST012_nume_AA_QT_vs_AA_BT_comparison"  "AST_REG_AA_QT_vs_AA_IT_comparison"     
# [147] "AST007_denom_AA_QT_vs_AA_IT_comparison" "AST007_nume_AA_QT_vs_AA_IT_comparison" 
# [149] "AST008_denom_AA_QT_vs_AA_IT_comparison" "AST008_nume_AA_QT_vs_AA_IT_comparison" 
# [151] "AST012_denom_AA_QT_vs_AA_IT_comparison" "AST012_nume_AA_QT_vs_AA_IT_comparison" 
# [153] "COPD_REG_QC"                            "COPD010_denom_QC"                      
# [155] "COPD010_num_QC"                         "COPD014_denom_QC"                      
# [157] "COPD014_num_QC"                         "COPD_REG_VC"                           
# [159] "COPD010_denom_VC"                       "COPD010_num_VC"                        
# [161] "COPD014_denom_VC"                       "COPD014_num_VC"                        
# [163] "COPD_REG_VC_VP"                         "COPD010_denom_VC_VP"                   
# [165] "COPD010_num_VC_VP"                      "COPD014_denom_VC_VP"                   
# [167] "COPD014_num_VC_VP"                      "COPD_REG_VC_RP"                        
# [169] "COPD010_denom_VC_RP"                    "COPD010_num_VC_RP"                     
# [171] "COPD014_denom_VC_RP"                    "COPD014_num_VC_RP"                     
# [173] "COPD_REG_UC"                            "COPD010_denom_UC"                      
# [175] "COPD010_num_UC"                         "COPD014_denom_UC"                      
# [177] "COPD014_num_UC"                         "COPD_REG_UC_VP"                        
# [179] "COPD010_denom_UC_VP"                    "COPD010_num_UC_VP"                     
# [181] "COPD014_denom_UC_VP"                    "COPD014_num_UC_VP"                     
# [183] "COPD_REG_UC_RP"                         "COPD010_denom_UC_RP"                   
# [185] "COPD010_num_UC_RP"                      "COPD014_denom_UC_RP"                   
# [187] "COPD014_num_UC_RP"                      "COPD_REG_QC_vs_VC_comparison"          
# [189] "COPD010_denom_QC_vs_VC_comparison"      "COPD010_num_QC_vs_VC_comparison"       
# [191] "COPD014_denom_QC_vs_VC_comparison"      "COPD014_num_QC_vs_VC_comparison"       
# [193] "COPD_REG_QC_vs_VC_VP_comparison"        "COPD010_denom_QC_vs_VC_VP_comparison"  
# [195] "COPD010_num_QC_vs_VC_VP_comparison"     "COPD014_denom_QC_vs_VC_VP_comparison"  
# [197] "COPD014_num_QC_vs_VC_VP_comparison"     "COPD_REG_QC_vs_VC_RP_comparison"       
# [199] "COPD010_denom_QC_vs_VC_RP_comparison"   "COPD010_num_QC_vs_VC_RP_comparison"    
# [201] "COPD014_denom_QC_vs_VC_RP_comparison"   "COPD014_num_QC_vs_VC_RP_comparison"    
# [203] "COPD_REG_QC_vs_UC_comparison"           "COPD010_denom_QC_vs_UC_comparison"     
# [205] "COPD010_num_QC_vs_UC_comparison"        "COPD014_denom_QC_vs_UC_comparison"     
# [207] "COPD014_num_QC_vs_UC_comparison"        "COPD_REG_QC_vs_UC_VP_comparison"       
# [209] "COPD010_denom_QC_vs_UC_VP_comparison"   "COPD010_num_QC_vs_UC_VP_comparison"    
# [211] "COPD014_denom_QC_vs_UC_VP_comparison"   "COPD014_num_QC_vs_UC_VP_comparison"    
# [213] "COPD_REG_QC_vs_UC_RP_comparison"        "COPD010_denom_QC_vs_UC_RP_comparison"  
# [215] "COPD010_num_QC_vs_UC_RP_comparison"     "COPD014_denom_QC_vs_UC_RP_comparison"  
# [217] "COPD014_num_QC_vs_UC_RP_comparison"     "COPD010_inc_exc_type"                  
# [219] "COPD014_inc_exc_type"                   "HYP_REG_QC_QR"                         
# [221] "HYP008_nume_QC_QR"                      "HYP008_denom_QC_QR"                    
# [223] "HYP009_nume_QC_QR"                      "HYP009_denom_QC_QR"                    
# [225] "HYP008_inc_exc_type_QC_QR"              "HYP009_inc_exc_type_QC_QR"             
# [227] "HYP_REG_QC_NN"                          "HYP008_nume_QC_NN"                     
# [229] "HYP008_denom_QC_NN"                     "HYP009_nume_QC_NN"                     
# [231] "HYP009_denom_QC_NN"                     "HYP_REG_QC_VT"                         
# [233] "HYP008_nume_QC_VT"                      "HYP008_denom_QC_VT"                    
# [235] "HYP009_nume_QC_VT"                      "HYP009_denom_QC_VT"                    
# [237] "HYP_REG_UC_QR"                          "HYP008_nume_UC_QR"                     
# [239] "HYP008_denom_UC_QR"                     "HYP009_nume_UC_QR"                     
# [241] "HYP009_denom_UC_QR"                     "HYP_REG_UC_NN"                         
# [243] "HYP008_nume_UC_NN"                      "HYP008_denom_UC_NN"                    
# [245] "HYP009_nume_UC_NN"                      "HYP009_denom_UC_NN"                    
# [247] "HYP_REG_UC_VT"                          "HYP008_nume_UC_VT"                     
# [249] "HYP008_denom_UC_VT"                     "HYP009_nume_UC_VT"                     
# [251] "HYP009_denom_UC_VT"                     "SMOK1_REG_current"                     
# [253] "SMOK2_REG_current"                      "SMOK002_numerator_current"             
# [255] "SMOK002_denominator_current"            "SMOK002_inc_exc_type_current"          
# [257] "SMOK004_numerator_current"              "SMOK004_denominator_current"           
# [259] "SMOK004_inc_exc_type_current"           "SMOK005_numerator_current"             
# [261] "SMOK005_denominator_current"            "SMOK005_inc_exc_type_current"          
# [263] "SMOK1_REG_new"                          "SMOK2_REG_new"                         
# [265] "SMOK002_numerator_new"                  "SMOK002_denominator_new"               
# [267] "SMOK004_numerator_new"                  "SMOK004_denominator_new"               
# [269] "SMOK005_numerator_new"                  "SMOK005_denominator_new"            

COPD_old <- dat %>% summarise(situation = "COPD QOF 2025-26",
                              pat_N = n(),
                              COPD_REG = sum(COPD_REG_QC),
                              COPD_REG_perc = sprintf("%.1f", round((sum(COPD_REG_QC)/nrow(dat))*100,1)),
                              COPD010_denom = sum(COPD010_denom_QC),
                              COPD010_num = sum(COPD010_num_QC),
                              COPD010_perc = sprintf("%.1f", round((sum(COPD010_num_QC)/sum(COPD010_denom_QC))*100,1)),
                              COPD014_denom = sum(COPD014_denom_QC),
                              COPD014_num = sum(COPD014_num_QC),
                              COPD014_perc = sprintf("%.1f", round((sum(COPD014_num_QC)/sum(COPD014_denom_QC))*100,1)))

# do COPD_VC_VP

COPD_validated <- dat %>% summarise(situation = "COPD QOF 2026-27",
                                    pat_N = n(),
                              COPD_REG = sum(COPD_REG_VC_VP),
                              COPD_REG_perc = sprintf("%.1f", round((sum(COPD_REG_VC_VP)/nrow(dat))*100,1)),
                              COPD010_denom = sum(COPD010_denom_VC_VP),
                              COPD010_num = sum(COPD010_num_VC_VP),
                              COPD010_perc = sprintf("%.1f", round((sum(COPD010_num_VC_VP)/sum(COPD010_denom_VC_VP))*100,1)),
                              COPD014_denom = sum(COPD014_denom_VC_VP),
                              COPD014_num = sum(COPD014_num_VC_VP),
                              COPD014_perc = sprintf("%.1f", round((sum(COPD014_num_VC_VP)/sum(COPD014_denom_VC_VP))*100,1)))

# do COPD_UC

COPD_updated <- dat %>% summarise(situation = "COPD Post-review",
                                  pat_N = n(),
                              COPD_REG = sum(COPD_REG_UC),
                              COPD_REG_perc = sprintf("%.1f", round((sum(COPD_REG_UC)/nrow(dat))*100,1)),
                              COPD010_denom = sum(COPD010_denom_UC),
                              COPD010_num = sum(COPD010_num_UC),
                              COPD010_perc = sprintf("%.1f", round((sum(COPD010_num_UC)/sum(COPD010_denom_UC))*100,1)),
                              COPD014_denom = sum(COPD014_denom_UC),
                              COPD014_num = sum(COPD014_num_UC),
                              COPD014_perc = sprintf("%.1f", round((sum(COPD014_num_UC)/sum(COPD014_denom_UC))*100,1)))

COPD <- bind_rows(COPD_old, COPD_validated, COPD_updated)


# and now do the same for asthma

asthma_old <- dat %>% summarise(situation = "Asthma QOF 2026-27",
                              pat_N = n(),
                              AST_REG = sum(AST_REG_QA_QT),
                              AST_REG_perc = sprintf("%.1f", round((sum(AST_REG_QA_QT)/nrow(dat))*100,1)),
                              AST007_denom = sum(AST007_denom_QA_QT),
                              AST007_num = sum(AST007_nume_QA_QT),
                              AST007_perc = sprintf("%.1f", round((sum(AST007_nume_QA_QT)/sum(AST007_denom_QA_QT))*100,1)),
                              AST008_denom = sum(AST008_denom_QA_QT),
                              AST008_num = sum(AST008_nume_QA_QT),
                              AST008_perc = sprintf("%.1f", round((sum(AST008_nume_QA_QT)/sum(AST008_denom_QA_QT))*100,1)),
                              AST012_denom = sum(AST012_denom_QA_QT),
                              AST012_num = sum(AST012_nume_QA_QT),
                              AST012_perc = sprintf("%.1f", round((sum(AST012_nume_QA_QT)/sum(AST012_denom_QA_QT))*100,1)))

# do the same for AA_QT

asthma_updated <- dat %>% summarise(situation = "Asthma post-review",
                                    pat_N = n(),
                              AST_REG = sum(AST_REG_AA_BT),
                              AST_REG_perc = sprintf("%.1f", round((sum(AST_REG_AA_BT)/nrow(dat))*100,1)),
                              AST007_denom = sum(AST007_denom_AA_BT),
                              AST007_num = sum(AST007_nume_AA_BT),
                              AST007_perc = sprintf("%.1f", round((sum(AST007_nume_AA_BT)/sum(AST007_denom_AA_BT))*100,1)),
                              AST008_denom = sum(AST008_denom_AA_BT),
                              AST008_num = sum(AST008_nume_alt_AA_BT),
                              AST008_perc = sprintf("%.1f", round((sum(AST008_nume_alt_AA_BT)/sum(AST008_denom_AA_BT))*100,1)),
                              AST012_denom = sum(AST012_denom_AA_BT),
                              AST012_num = sum(AST012_nume_AA_BT),
                              AST012_perc = sprintf("%.1f", round((sum(AST012_nume_AA_BT)/sum(AST012_denom_AA_BT))*100,1)))

asthma <- bind_rows(asthma_old, asthma_updated)


# smoking

smoking_QOF <- dat %>% summarise(situation = "Smoking QOF 2026-27",
                              pat_N = n(),
                              SMOK1_REG = sum(SMOK1_REG_current),
                              SMOK1_REG_perc = sprintf("%.1f", round((sum(SMOK1_REG_current)/nrow(dat))*100,1)),
                              SMOK2_REG = sum(SMOK2_REG_current),
                              SMOK2_REG_perc = sprintf("%.1f", round((sum(SMOK2_REG_current)/nrow(dat))*100,1)),
                              SMOK002_denom = sum(SMOK002_denominator_current),
                              SMOK002_num = sum(SMOK002_numerator_current),
                              SMOK002_perc = sprintf("%.1f", round((sum(SMOK002_numerator_current)/sum(SMOK002_denominator_current))*100,1)),
                              SMOK004_denom = sum(SMOK004_denominator_current),
                              SMOK004_num = sum(SMOK004_numerator_current),
                              SMOK004_perc = sprintf("%.1f", round((sum(SMOK004_numerator_current)/sum(SMOK004_denominator_current))*100,1)),
                              SMOK005_denom = sum(SMOK005_denominator_current),
                              SMOK005_num = sum(SMOK005_numerator_current),
                              SMOK005_perc = sprintf("%.1f", round((sum(SMOK005_numerator_current)/sum(SMOK005_denominator_current))*100,1)))

# and the updated

smoking_updated <- dat %>% summarise(situation = "Smoking post-review",
                                    pat_N = n(),
                              SMOK1_REG = sum(SMOK1_REG_new),
                              SMOK1_REG_perc = sprintf("%.1f", round((sum(SMOK1_REG_new)/nrow(dat))*100,1)),
                              SMOK2_REG = sum(SMOK2_REG_new),
                              SMOK2_REG_perc = sprintf("%.1f", round((sum(SMOK2_REG_new)/nrow(dat))*100,1)),
                              SMOK002_denom = sum(SMOK002_denominator_new),
                              SMOK002_num = sum(SMOK002_numerator_new),
                              SMOK002_perc = sprintf("%.1f", round((sum(SMOK002_numerator_new)/sum(SMOK002_denominator_new))*100,1)),
                              SMOK004_denom = sum(SMOK004_denominator_new),
                              SMOK004_num = sum(SMOK004_numerator_new),
                              SMOK004_perc = sprintf("%.1f", round((sum(SMOK004_numerator_new)/sum(SMOK004_denominator_new))*100,1)),
                              SMOK005_denom = sum(SMOK005_denominator_new),
                              SMOK005_num = sum(SMOK005_numerator_new),
                              SMOK005_perc = sprintf("%.1f", round((sum(SMOK005_numerator_new)/sum(SMOK005_denominator_new))*100,1)))

smoking <- bind_rows(smoking_QOF, smoking_updated)

# hypertension

hypertension_QOF <- dat %>% summarise(situation = "Hypertension QOF 2026-27",
                              pat_N = n(),
                              HYP_REG = sum(HYP_REG_QC_QR),
                              HYP_REG_perc = sprintf("%.1f", round((sum(HYP_REG_QC_QR)/nrow(dat))*100,1)),
                              HYP008_denom = sum(HYP008_denom_QC_QR),
                              HYP008_num = sum(HYP008_nume_QC_QR),
                              HYP008_perc = sprintf("%.1f", round((sum(HYP008_nume_QC_QR)/sum(HYP008_denom_QC_QR))*100,1)),
                              HYP009_denom = sum(HYP009_denom_QC_QR),
                              HYP009_num = sum(HYP009_nume_QC_QR),
                              HYP009_perc = sprintf("%.1f", round((sum(HYP009_nume_QC_QR)/sum(HYP009_denom_QC_QR))*100,1)))

# and do it for UC_VT

hypertension_updated <- dat %>% summarise(situation = "Hypertension post-review",
                                    pat_N = n(),
                              HYP_REG = sum(HYP_REG_UC_VT),
                              HYP_REG_perc = sprintf("%.1f", round((sum(HYP_REG_UC_VT)/nrow(dat))*100,1)),
                              HYP008_denom = sum(HYP008_denom_UC_VT),
                              HYP008_num = sum(HYP008_nume_UC_VT),
                              HYP008_perc = sprintf("%.1f", round((sum(HYP008_nume_UC_VT)/sum(HYP008_denom_UC_VT))*100,1)),
                              HYP009_denom = sum(HYP009_denom_UC_VT),
                              HYP009_num = sum(HYP009_nume_UC_VT),
                              HYP009_perc = sprintf("%.1f", round((sum(HYP009_nume_UC_VT)/sum(HYP009_denom_UC_VT))*100,1)))

hypertension <- bind_rows(hypertension_QOF, hypertension_updated)

COPD
asthma
smoking
hypertension

# clean up tables

#hypertension 

hypertension <- hypertension %>% 
  mutate(HYP_REG_clean = paste0(HYP_REG, "/", pat_N, " (", HYP_REG_perc, "%)"),
         HYP008_clean = paste0(HYP008_num, "/", HYP008_denom, " (", HYP008_perc, "%)"),
         HYP009_clean = paste0(HYP009_num, "/", HYP009_denom, " (", HYP009_perc, "%)"))

# COPD 

COPD <- COPD %>% 
  mutate(COPD_REG_clean = paste0(COPD_REG, "/", pat_N, " (", COPD_REG_perc, "%)"),
         COPD010_clean = paste0(COPD010_num, "/", COPD010_denom, " (", COPD010_perc, "%)"),
         COPD014_clean = paste0(COPD014_num, "/", COPD014_denom, " (", COPD014_perc, "%)"))

# asthma

asthma <- asthma %>% 
  mutate(AST_REG_clean = paste0(AST_REG, "/", pat_N, " (", AST_REG_perc, "%)"),
         AST007_clean = paste0(AST007_num, "/", AST007_denom, " (", AST007_perc, "%)"),
         AST008_clean = paste0(AST008_num, "/", AST008_denom, " (", AST008_perc, "%)"),
         AST012_clean = paste0(AST012_num, "/", AST012_denom, " (", AST012_perc, "%)"))

# smoking

smoking <- smoking %>% 
  mutate(SMOK1_REG_clean = paste0(SMOK1_REG, "/", pat_N, " (", SMOK1_REG_perc, "%)"),
         SMOK2_REG_clean = paste0(SMOK2_REG, "/", pat_N, " (", SMOK2_REG_perc, "%)"),
         SMOK002_clean = paste0(SMOK002_num, "/", SMOK002_denom, " (", SMOK002_perc, "%)"),
         SMOK004_clean = paste0(SMOK004_num, "/", SMOK004_denom, " (", SMOK004_perc, "%)"),
         SMOK005_clean = paste0(SMOK005_num, "/", SMOK005_denom, " (", SMOK005_perc, "%)"))

doc <- read_docx() %>% #  body_end_section_landscape() %>%
  body_add_flextable(flextable(COPD)) %>%
  body_add_flextable(flextable(asthma)) %>%
  body_add_flextable(flextable(smoking)) %>%
  body_add_flextable(flextable(hypertension)) %>%
  print(target = "X:/QOF_project/Alex/Output/combined/Table4_old_new_selected_scenarios_male.docx")
  
