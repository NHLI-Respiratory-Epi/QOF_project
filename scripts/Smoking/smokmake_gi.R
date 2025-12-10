library(tidyverse)

# smokmake_QSSD <- function(df) {
#   
#   new_df <- df %>%
#     dplyr::mutate(QSSD = "2024-04-01")
#   
#   return(new_df)
# }

# dat <- smokmake_QSSD(df=dat)
# 
# smokmake_ACHV_DAT <- function(df) {
#   
#   new_df <- df %>%
#     dplyr::mutate(ACHV_DAT = "2024-03-31")
#   
#   return(new_df)
# }

# dat <- smokmake_ACHV_DAT(df=dat)

smokmake_PPED <- function(df) {
  
  new_df <- df %>%
    dplyr::mutate(PPED = "2026-03-31")
  
  return(new_df)  
  
}

dat <- smokmake_PPED(df=dat)

# smokmake_PAT_AGE <- function(df) {
#   
#   new_df <- df %>%
#     dplyr::mutate(PAT_AGE = as.numeric(ACHV - dob)/365)
#   
#   return(new_df)  
#   
# }
# 
# dat <- smokmake_PAT_AGE(df=dat)
# 
# smokmake_MH_DAT <- function(df) {
#   
#   new_df <- df %>%
#     dplyr::mutate(MH_DAT = min(MH_COD_earliest) <= ACHV)
#   
#   return(new_df)  
#   
# }
#   
# smokmake_CKD_DAT <- function(df) {
#   
#   new_df <- df %>%
#     dplyr::mutate(CKD_DAT = max(CKD_DAT_latest) <= ACHV)
#   
#   return(new_df)  
#   
# }
# 
# smokmake_ASTTRT_DAT <- function(df) {
#   
#   new_df <- df %>%
#     dplyr::mutate(PAT_AGE = as.numeric(ACHV - dob)/365)
#   
#   return(new_df)  
#   
# }
# 
# smokmake_ASTLAT_DAT <- function(df) {
#   
#   new_df <- df %>%
#     dplyr::mutate(PAT_AGE = as.numeric(ACHV - dob)/365)
#   
#   return(new_df)  
#   
# }
# 
# smokmake_AST_DAT <- function(df) {
#   
#   new_df <- df %>%
#     dplyr::mutate(PAT_AGE = as.numeric(ACHV - dob)/365)
#   
#   return(new_df)  
#   
# }
# 
# smokmake_COPDLAT_DAT <- function(df) {
#   
#   new_df <- df %>%
#     dplyr::mutate(PAT_AGE = as.numeric(ACHV - dob)/365)
#   
#   return(new_df)  
#   
# }
# 
# smokmake_COPD_DAT <- function(df) {
#   
#   new_df <- df %>%
#     dplyr::mutate(PAT_AGE = as.numeric(ACHV - dob)/365)
#   
#   return(new_df)  
#   
# }
# 
# 
# smokmake_DMLAT_DAT <- function(df) {
#   
#   new_df <- df %>%
#     dplyr::mutate(PAT_AGE = as.numeric(ACHV - dob)/365)
#   
#   return(new_df)  
#   
# }
# 
# smokmake_DM_DAT <- function(df) {
#   
#   new_df <- df %>%
#     dplyr::mutate(PAT_AGE = as.numeric(ACHV - dob)/365)
#   
#   return(new_df)  
#   
# }
# 
# smokmake_HYPLAT_DAT <- function(df) {
#   
#   new_df <- df %>%
#     dplyr::mutate(PAT_AGE = as.numeric(ACHV - dob)/365)
#   
#   return(new_df)  
#   
# }
# 
# smokmake_HYP_DAT <- function(df) {
#   
#   new_df <- df %>%
#     dplyr::mutate(PAT_AGE = as.numeric(ACHV - dob)/365)
#   
#   return(new_df)  
#   
# }
# 
# smokmake_TIA_DAT <- function(df) {
#   
#   new_df <- df %>%
#     dplyr::mutate(PAT_AGE = as.numeric(ACHV - dob)/365)
#   
#   return(new_df)  
#   
# }
# 
# smokmake_STRK_DAT <- function(df) {
#   
#   new_df <- df %>%
#     dplyr::mutate(PAT_AGE = as.numeric(ACHV - dob)/365)
#   
#   return(new_df)  
#   
# }
# 
# smokmake_PAD_DAT <- function(df) {
#   
#   new_df <- df %>%
#     dplyr::mutate(PAT_AGE = as.numeric(ACHV - dob)/365)
#   
#   return(new_df)  
#   
# }
# 
# smokmake_CHD_DAT <- function(df) {
#   
#   new_df <- df %>%
#     dplyr::mutate(PAT_AGE = as.numeric(ACHV - dob)/365)
#   
#   return(new_df)  
#   
# }
# 
# smokmake_REG_DAT <- function(df) {
#   
#   new_df <- df %>%
#     dplyr::mutate(PAT_AGE = as.numeric(ACHV - dob)/365)
#   
#   return(new_df)  
#   
# }
# 
# smokmake_SMOKINVITE1_DAT <- function(df) {
#   
#   new_df <- df %>%
#     dplyr::mutate(PAT_AGE = as.numeric(ACHV - dob)/365)
#   
#   return(new_df)  
#   
# }
# 
# smokmake_SMOKPCADEC_DAT <- function(df) {
#   
#   new_df <- df %>%
#     dplyr::mutate(PAT_AGE = as.numeric(ACHV - dob)/365)
#   
#   return(new_df)  
#   
# }
# 
# smokmake_SMOKPCAPU_DAT <- function(df) {
#   
#   new_df <- df %>%
#     dplyr::mutate(PAT_AGE = as.numeric(ACHV - dob)/365)
#   
#   return(new_df)  
#   
# }
# 
# smokmake_PHARMDRUG_DAT <- function(df) {
#   
#   new_df <- df %>%
#     dplyr::mutate(PAT_AGE = as.numeric(ACHV - dob)/365)
#   
#   return(new_df)  
#   
# }
# 
# smokmake_PHARM_DAT <- function(df) {
#   
#   new_df <- df %>%
#     dplyr::mutate(PAT_AGE = as.numeric(ACHV - dob)/365)
#   
#   return(new_df)  
#   
# }
# 
# smokmake_REFERSSSA_DAT <- function(df) {
#   
#   new_df <- df %>%
#     dplyr::mutate(PAT_AGE = as.numeric(ACHV - dob)/365)
#   
#   return(new_df)  
#   
# }
# 
# smokmake_SMOK_DAT <- function(df) {
#   
#   new_df <- df %>%
#     dplyr::mutate(PAT_AGE = as.numeric(ACHV - dob)/365)
#   
#   return(new_df)  
#   
# }
# 
# smokmake_CSMOK_DAT <- function(df) {
#   
#   new_df <- df %>%
#     dplyr::mutate(PAT_AGE = as.numeric(ACHV - dob)/365)
#   
#   return(new_df)  
#   
# }


smokmake_ASTRES_DAT <- function(df) {
     
     new_df <- df %>%
dplyr::mutate(ASTRES_DAT = if_else(ASTRES_COD_latest > ASTLAT_DAT, 
                            ASTRES_COD_latest, NA))
     return(new_df)
     
}

smokmake_COPDRES_DAT <- function(df) {
  
  new_df <- df %>%
    dplyr::mutate(COPDRES_DAT = if_else(COPDRES_COD_latest > COPDLAT_DAT, 
                                       COPDRES_COD_latest, NA))
  return(new_df)
  
}


smokmake_DMRES_DAT <- function(df) {
  
  new_df <- df %>%
    dplyr::mutate(DMRES_DAT = if_else(DMRES_COD_latest > DMLAT_DAT, 
                                       DMRES_COD_latest, NA))
  return(new_df)
  
}

smokmake_HYPRES_DAT <- function(df) {
  
  new_df <- df %>%
    dplyr::mutate(HYPRES_DAT = if_else(HYPRES_COD_latest > HYPLAT_DAT, 
                                       HYPRES_COD_latest, NA))
  return(new_df)
  
}

smokmake_DIAG_DAT <- function(df) {
  
  new_df <- df %>%
    dplyr::mutate(DIAG_DAT = min(CHD_DAT|PAD_DAT|STRK_DAT|TIA_DAT|HYP_DATifHYPRES_DAT)
                    
                    
                    
                    
                    if_else(ASTRES_COD_latest > ASTLAT_DAT, 
                                       ASTRES_COD_latest, NA))
  return(new_df)
  
}




# smokmake_ <- function(df) {
#   
#   new_df <- df %>%
#     dplyr::mutate(PAT_AGE = as.numeric(ACHV - dob)/365)
#   
#   return(new_df)  
#   
# }
# 
# smokmake_ <- function(df) {
#   
#   new_df <- df %>%
#     dplyr::mutate(PAT_AGE = as.numeric(ACHV - dob)/365)
#   
#   return(new_df)  
#   
# }
# 
# smokmake_ <- function(df) {
#   
#   new_df <- df %>%
#     dplyr::mutate(PAT_AGE = as.numeric(ACHV - dob)/365)
#   
#   return(new_df)  
#   
# }
# 
# smokmake_ <- function(df) {
#   
#   new_df <- df %>%
#     dplyr::mutate(PAT_AGE = as.numeric(ACHV - dob)/365)
#   
#   return(new_df)  
#   
# }
# 
# smokmake_ <- function(df) {
#   
#   new_df <- df %>%
#     dplyr::mutate(PAT_AGE = as.numeric(ACHV - dob)/365)
#   
#   return(new_df)  
#   
# }
# 
# smokmake_ <- function(df) {
#   
#   new_df <- df %>%
#     dplyr::mutate(PAT_AGE = as.numeric(ACHV - dob)/365)
#   
#   return(new_df)  
#   
# }
# 
# smokmake_ <- function(df) {
#   
#   new_df <- df %>%
#     dplyr::mutate(PAT_AGE = as.numeric(ACHV - dob)/365)
#   
#   return(new_df)  
#   
# }
# 
# smokmake_ <- function(df) {
#   
#   new_df <- df %>%
#     dplyr::mutate(PAT_AGE = as.numeric(ACHV - dob)/365)
#   
#   return(new_df)  
#   
# }
# 
# smokmake_ <- function(df) {
#   
#   new_df <- df %>%
#     dplyr::mutate(PAT_AGE = as.numeric(ACHV - dob)/365)
#   
#   return(new_df)  
#   
# }



##calling all functions

f_list <- lsf.str() %>%
  as.character()

f_list <- f_list[str_detect(f_list,
                            "smokmake")]

for (f in f_list) {
  my_data <- do.call(
    f,
    list(df = my_data)
  )
}
