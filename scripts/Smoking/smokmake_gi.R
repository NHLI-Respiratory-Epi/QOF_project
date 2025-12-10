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

smokmake_PAT_AGE <- function(df) {
  
  new_df <- df %>%
    dplyr::mutate(PAT_AGE = as.numeric(ACHV - dob)/365)
  
  return(new_df)  
  
}

dat <- smokmake_PAT_AGE(df=dat)



##calling all functions
# f_list <- lsf.str()
# 
# x <- 4
# 
# for (f in f_list) {
#   x <- do.call("smokmake_" %in% f_list, list(df=df))
# }
# 
# x
