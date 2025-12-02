# a little R script test here.

library(data.table)
library(dplyr)
library(bit64)
library(parallel)
library(readstata13)

# Function to read in lookups

addLookup <- function(dat, column_name, lookup_filename, 
                      lookup_filepath = "Y:/CPRD_Research_Data/Lookup_Files/202509_Lookups_CPRDAurum/") {
  lkup <- read.csv(paste0(lookup_filepath, lookup_filename), sep = "\t")
  colnames(lkup)[1] <- column_name
  
  dat <- left_join(dat, lkup, by = column_name)
  
  #find the index for where the column is
  col_locate <- match(column_name, colnames(dat))
  col_locate2 <- colnames(dat)[col_locate - 1]
  
  
  dat <- dat %>% dplyr::select(!matches(column_name)) %>% 
    dplyr::relocate(Description, .after = dplyr::all_of(col_locate2)) %>%
    dplyr::rename({{column_name}} := Description)
  
  return(dat)
}


# lookup directory
lookups <- "Z:/PCPH/CPRD_Research_Data/Lookup_Files/202509_Lookups_CPRDAurum/"
list.files(lookups)

# save directory for RDS
rds_dir <- "X:/QOF_project/Data/tidy_data/RDS/"

# save directory for dta
dta_dir <- "X:/QOF_project/Data/tidy_data/dta/"

# We are trying to follow the business rules so we do minimal cleaning of the data

# let's extract the files we need and get them in a suitable format
rawdatadir <- "Z:/Group_work/raw_data/QOF/" 
extract_files <- dir(rawdatadir)

#----------------# patient:
patient_files <- extract_files[grepl("Patient", extract_files) == TRUE]

pat <- lapply(paste0(rawdatadir, patient_files), function(x) {
  dat <- fread(x)
  dat <- addLookup(dat, "gender", "Gender.txt")
  dat <- addLookup(dat, "patienttypeid", "PatientType.txt")
  dat <- dat %>% dplyr::mutate(emis_ddate = as.Date(emis_ddate, format = "%d/%m/%Y"),
                       regstartdate = as.Date(regstartdate, format = "%d/%m/%Y"),
                       regenddate = as.Date(regenddate, format = "%d/%m/%Y"),
                       cprd_ddate = as.Date(cprd_ddate, format = "%d/%m/%Y"))
  dat <- dat %>% dplyr::mutate(dob = if_else(is.na(mob), # mob only for <18s
                               as.Date(paste0("01/07/", yob), format =  "%d/%m/%Y"),
                               if_else(mob %in% c(1, 3, 5, 7, 8, 10, 12),
                                       as.Date(paste0("16/",mob,"/",yob), format =  "%d/%m/%Y"), # 31 day month
                                       as.Date(paste0("15/",mob,"/",yob), format =  "%d/%m/%Y")))) # 28-30 day month
  return(dat)
                                                     
})

pat <- rbindlist(pat)

table(pat$acceptable, useNA = "ifany")
# all patients acceptable so drop this column

# dob created so no longer need yob or dob

pat <- pat %>% dplyr::select(-acceptable, -yob, -mob)

# Let's just put the full descriptions for gender in

pat$gender[pat$gender == "F"] <- "Female"
pat$gender[pat$gender == "M"] <- "Male"
pat$gender[pat$gender == "I"] <- "Indeterminate"


saveRDS(pat, paste0(rds_dir, "Patient.RDS"))


#--------------------------------# practice files

practice_files <- extract_files[grepl("Practice", extract_files) == TRUE]

practice <- lapply(paste0(rawdatadir, practice_files), function(x) {
  dat <- fread(x)
  dat <- addLookup(dat, "region", "Region.txt")
  dat <- dat %>% dplyr::mutate(lcd = as.Date(lcd, format = "%d/%m/%Y"))
  return(dat)
})

practice <- rbindlist(practice)
practice <- unique(practice)
practice <- practice %>% dplyr::select(-uts) # empty column

saveRDS(practice, paste0(rds_dir, "Practice.RDS"))

#-----------------------# patient_practice
# we usually combine the patient and practice file together for analysis

patient_practice <- dplyr::left_join(pat, practice, by = "pracid")

saveRDS(patient_practice, paste0(rds_dir, "Patient_practice.RDS"))

#------------------------# stata files
# now we can create the stata files

# pat$patid <- as.character(pat$patid) # needs to be character for stata
# patient_practice$patid <- as.character(patient_practice$patid) # needs to be character for stata
# readstata13::save.dta13(pat, paste0(dta_dir, "Patient.dta"), convert.dates = TRUE)
# readstata13::save.dta13(practice, paste0(dta_dir, "Practice.dta"), convert.dates = TRUE)
# readstata13::save.dta13(patient_practice, paste0(dta_dir, "Patient_practice.dta"), convert.dates = TRUE)

#---------------------------------# observations and drugfiles

# Now, for the observation files and the drug files, it's helpful to use parallel
# processing to make it happen a bit quicker

medcode_to_snomed <- fread(paste0(lookups, "202509_EMISMedicalDictionary.txt"))

medcode_to_snomed <- medcode_to_snomed %>% dplyr::select(medcodeid = MedCodeId,
                                                         snomedctconceptid = SnomedCTConceptId) %>%
  unique()


# summary(medcode_to_snomed)
# nrow(unique(medcode_to_snomed))

if (length(unique(medcode_to_snomed$medcodeid)) != nrow(medcode_to_snomed)) {
  message("medcodeids not unique - this will mess up the linkage so check what's going on") # every one here must be unique
}

# set medcodeid as the key column to join on later
setkey(medcode_to_snomed, medcodeid)

prodcode_to_dmdid <- fread(paste0(lookups, "202509_EMISProductDictionary.txt"))

prodcode_to_dmdid <- prodcode_to_dmdid %>% dplyr::select(prodcodeid = ProdCodeId,
                                                         dmdid = dmdid) %>%
  unique()

if (length(unique(prodcode_to_dmdid$prodcodeid)) != nrow(prodcode_to_dmdid)) {
  message("prodcodeids not unique - this will mess up the linkage so check what's going on") # every one here must be unique
}



obs_files <- extract_files[grepl("Observation", extract_files) == TRUE]
drugs_files <- extract_files[grepl("DrugIssue", extract_files) == TRUE]

# to make these easier to cycle through later, I am going to renumber them.
# 1_Extract_Observation_001 -> Observation_1
# 1_Extract_Observation_029 -> Observation_29
# 2_Extract_Observation_001 -> Observation_30 # etc. 

# in this project we will be using snomed concept ids to identify records,
# so probably good to add them in at this step. 



length(obs_files)
detectCores()
ncores <- 2

# Create a cluster
cl <- makeCluster(ncores)

# # may need to do this step multiple times if the numbers get too large
# dat <- map(extract_files, apply_codelist_pipe, codelist = smoking_codelist, 
#     path = "Z:/Group_work/raw_data/smoking/", cleaning_for_obs = TRUE)

# we should parallelise this process

clusterEvalQ(cl, { # these need to be read into each cluster
  library(data.table)
  library(dplyr)
  library(bit64)
  library(readstata13)
})


# these R objects are necessary in each cluster
clusterExport(cl, varlist = c("obs_files", "medcode_to_snomed", "apply_codelist_pipe"), envir = environment())

big <- parLapply(cl, extract_files, function(dat) {
  apply_codelist_pipe(dat, codelist = smoking_codelist, path = "Z:/Group_work/raw_data/smoking/")
})

stopCluster(cl)

Sys.time()

dat <- fread(paste0(rawdatadir, drugs_files[1]))

head(dat)
summary(dat$issueid)
# I would prefer to do this in two pipes, but makes sense to do it all in one
# using the parallel processing

cleaning_pipe <- function(obs_file, obs_filelist, 
                          drugs_file, drugs_filelist,
                          medcode_to_snomed,
                          prodcode_to_dmdid,
                           rawdatadir = "Z:/Group_work/raw_data/QOF/", 
                           rds_dir = "X:/QOF_project/Data/tidy_data/RDS/", 
                           dta_dir = "X:/QOF_project/Data/tidy_data/dta/") {
  # obs
  datobs <- fread(paste0(rawdatadir, obs_file)) 
  endobs <- which(obs_filelist == obs_file)
  filenameobs <- paste0("Observation_", endobs)
  datobs <- datobs %>% dplyr::mutate(obsdate = as.Date(obsdate, format = "%d/%m/%Y"),
                             enterdate = as.Date(enterdate, format = "%d/%m/%Y"))
  datobs <- datobs %>% dplyr::select(-consid, -obsid, -staffid, -parentobsid,
                               -numrangelow, -numrangehigh, -probobsid)
  data.table::setkey(datobs, medcodeid)
  datobs <- medcode_to_snomed[datobs, on = "medcodeid"]
  
  saveRDS(datobs, paste0(rds_dir, filenameobs, ".RDS")) # rds files
  
  # obs stata files
  datobs$patid <- as.character(datobs$patid)
  datobs$medcodeid <- as.character(datobs$medcodeid)
  datobs$snomedctconceptid <- as.character(datobs$snomedctconceptid)
  readstata13::save.dta13(datobs, paste0(dta_dir, filenameobs, ".dta"), convert.dates = TRUE)
  
  
  # drugs
  datdrugs <- fread(paste0(rawdatadir, drugs_file)) 
  enddrugs <- which(drugs_filelist == drugs_file)
  filenamedrugs <- paste0("DrugIssue_", end)
  datdrugs <- datdrugs %>% dplyr::mutate(issuedate = as.Date(issuedate, format = "%d/%m/%Y"),
                                     enterdate = as.Date(enterdate, format = "%d/%m/%Y"))
  datdrugs <- datdrugs %>% dplyr::select(-issueid, -probobsid, -drugrecid, -staffid)
  data.table::setkey(datdrugs, prodcodeid)
  datdrugs <- prodcode_to_dmdid[datdrugs, on = "prodcodeid"]
  
  saveRDS(datdrugs, paste0(rds_dir, filenameobs, ".RDS")) # rds files
  
  # drugs stata files
  datdrugs$patid <- as.character(datdrugs$patid)
  datdrugs$prodcodeid <- as.character(datdrugs$prodcodeid)
  datdrugs$dmdid <- as.character(datdrugs$dmdid)
  readstata13::save.dta13(datdrugs, paste0(dta_dir, filenamedrugs, ".dta"), convert.dates = TRUE)
  
  
  return(list(head(datobs), head(datdrugs))) # just a token thing that can be checked
}


list(head(dat), head(dat))

# test <- lapply(obs_files[1], function(x) {
#   clean_obs_pipe(x, obs_filelist = obs_files, medcode_to_snomed = medcode_to_snomed)
# })

# these R objects are necessary in each cluster
clusterExport(cl, varlist = c("smoking_codelist", "apply_codelist_pipe"), envir = environment())

big <- parLapply(cl, extract_files, function(dat) {
  apply_codelist_pipe(dat, codelist = smoking_codelist, path = "Z:/Group_work/raw_data/smoking/")
})

stopCluster(cl)

Sys.time()



