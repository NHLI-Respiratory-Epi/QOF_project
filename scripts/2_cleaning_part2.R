# it is helpful to ensure that practices and patients don't cross data chunks

# therefore we can create some code that uses what we've already created that does this. 

# it might be worth simply splitting each practice into its own chunk tbh

# there are current 200 obs, and like 300 practices I think, so doesn't split very evenly


# we can split it into each practice being separate, makes it easier to identify
# latest and earliest etc as we know they will just be in one chunk

library(data.table)
library(dplyr)
library(bit64)
library(parallel)

patient_practice <- readRDS("X:/QOF_project/Data/tidy_data/RDS/Patient_practice.RDS")

workingdatadir <- "Z:/Group_work/QOF_project/Data/working_data/" 
extract_files <- dir(workingdatadir)

obs_files <- extract_files[grepl("Observation", extract_files) == TRUE]
drugs_files <- extract_files[grepl("DrugIssue", extract_files) == TRUE]

rds_dir <- "X:/QOF_project/Data/tidy_data/RDS/"


detectCores()
ncores <- 80

# Create a cluster
cl <- makeCluster(ncores)

# we should parallelise this process

clusterEvalQ(cl, { # these need to be read into each cluster
  library(data.table)
  library(dplyr)
  library(bit64)
  library(readstata13)
})


# these R objects are necessary in each cluster
clusterExport(cl, varlist = c("obs_files", "drugs_files", "workingdatadir", "rds_dir"), envir = environment())

big <- parLapply(cl, paste0(workingdatadir, obs_files), function(x) {
  dt <- readRDS(x)
  split(dt, by = "pracid", keep.by = TRUE) 
  })



unique_pracids <- unique(patient_practice$pracid)

# Prepare final result: one element per pracid
prac_list <- setNames(vector("list", length(unique_pracids)), unique_pracids)

for (chunk_split in big) {

    for (p in names(chunk_split)) {
  
        if (is.null(prac_list[[p]])) {
      
          prac_list[[p]] <- chunk_split[[p]]
    } else {
      prac_list[[p]] <- rbind(prac_list[[p]], chunk_split[[p]])
    }
  }
}


pracidlookup <- data.frame(pracid = unique_pracids, obsno = 1:length(unique_pracids))

clusterExport(cl, varlist = c("obs_files", "drugs_files", "workingdatadir", "rds_dir", "pracidlookup"), envir = environment())


for (i in seq_along(prac_list)) {
  saveRDS(
    prac_list[[i]],
    file.path(rds_dir, sprintf("Observation_%d.RDS", i))
  )
}


bigdrug <- parLapply(cl, paste0(workingdatadir, drugs_files), function(x) {
  dt <- readRDS(x)
  split(dt, by = "pracid", keep.by = TRUE) 
})


# Prepare final result: one element per pracid
prac_list_drug <- setNames(vector("list", length(unique_pracids)), unique_pracids)

for (chunk_split in bigdrug) {
  
  for (p in names(chunk_split)) {
    
    if (is.null(prac_list_drug[[p]])) {
      
      prac_list_drug[[p]] <- chunk_split[[p]]
    } else {
      prac_list_drug[[p]] <- rbind(prac_list_drug[[p]], chunk_split[[p]])
    }
  }
}


for (i in seq_along(prac_list_drug)) {
  saveRDS(
    prac_list_drug[[i]],
    file.path(rds_dir, sprintf("DrugIssue_%d.RDS", i))
  )
}

  

stopCluster(cl)

