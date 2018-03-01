script_dir <- "C:/Users/tvickers/Desktop/Projects/Duplicate_MRN_INQ"

setwd("C:/Users/tvickers/Desktop/R_General")
source("General_fxns.R")
cleverSource_DateTable(return_directory = script_dir)

require(readxl)
require(tidyr)
require(dplyr)

setwd("C:/Users/tvickers/Desktop/Projects/Duplicate_MRN_INQ/BIDS Feed/dat_20180201")
iq_regactivity_ca <- read_excel("InQuicker IDX Patients.xlsx", skip=3, col_names = c("lastname", "firstname", "birthdate", "mrn", "ssn", "registrationdate",
                                                                        "registeredby", "updateddate", "updatedby", "deleteddate", "deactivateddate",
                                                                        "comment", "groups", "groupnames"))

iq_regactivity_ca$birthdate <-  as.Date(iq_regactivity_ca$birthdate, origin = '1899-12-30')
iq_regactivity_ca$registrationdate <-  as.Date(iq_regactivity_ca$registrationdate, origin = '1899-12-30')
iq_regactivity_ca$updateddate <-  as.Date(iq_regactivity_ca$updateddate, origin = '1899-12-30')
iq_regactivity_ca$deleteddate <-  as.Date(iq_regactivity_ca$deleteddate, origin = '1899-12-30')
iq_regactivity_ca$deactivateddate <-  as.Date(iq_regactivity_ca$deactivateddate, origin = '1899-12-30')

iq_regactivity_ca$firstname <- trimws(iq_regactivity_ca$firstname, 'both')
iq_regactivity_ca$lastname <- trimws(iq_regactivity_ca$lastname, 'both')
iq_regactivity_ca$fullname_reg <- paste(iq_regactivity_ca$firstname, iq_regactivity_ca$lastname)
iq_regactivity_ca$fullname_reg <- tolower(iq_regactivity_ca$fullname_reg)

iq_regactivity_ca <- merge(iq_regactivity_ca, date_table, by.x = "registrationdate", by.y = "day")

iq_regactivity_ca$baseline_duplication <- ifelse(grepl(".*DUP USE.*", iq_regactivity_ca$comment),"duplicate", "valid")
iq_regactivity_ca$preflagged_dupe <- ifelse(grepl(".*DUP USE.*", iq_regactivity_ca$comment), 1, 0)

#1 - flatten the 'standard duplicate' file from GECB into a single table. In the previous process, this was called "INQ_CA_all_time_dupes"
setwd("C:/Users/tvickers/Desktop/Projects/Duplicate_MRN_INQ/BIDS Feed/dat_20180201/Duplicate IDX Patients - Standard_201802")

#get the sheetlist from the active file
shts <- excel_sheets("Duplicate IDX Patients - Standard_201802.xlsx")

#read through data on each sheet and put into a list to be bound together afterwards.
datalist.d1 = list()
for(i in 1:length(shts)) {
  dat <- read_excel("Duplicate IDX Patients - Standard_201802.xlsx", skip = 3, col_names = c("lastname", "firstname", "birthdate", "sex", "mrn", "ssn", "zip", "phone",
                                                                                             "registrationdate", "registeredby", "updateddate", "updatedby", 
                                                                                             "comment", "groups"),
                    sheet = shts[i])
  dat$sheetname <- shts[i]
  datalist.d1[[i]] <- dat
}
duplicate_idx_patients <- do.call(rbind,datalist.d1)

duplicate_idx_patients$sheet_splitter <- duplicate_idx_patients$sheetname
duplicate_idx_patients <- duplicate_idx_patients %>% separate(sheet_splitter, c("method", "sheetname_grp"), "-")

#note overlap between capture methods. That is, if I remove the capturing detail (from sheetnames and groups) then I find duplicate records
table(duplicated(duplicate_idx_patients[,1:14]))

#3 - done
duplicate_idx_patients$birthdate <-  as.Date(duplicate_idx_patients$birthdate, format = '%m/%d/%Y')
duplicate_idx_patients$registrationdate <-  as.Date(duplicate_idx_patients$registrationdate, origin = '1899-12-30')
duplicate_idx_patients$updateddate <-  as.Date(duplicate_idx_patients$updateddate, origin = '1899-12-30')

duplicate_idx_patients$firstname <- trimws(duplicate_idx_patients$firstname, 'both')
duplicate_idx_patients$lastname <- trimws(duplicate_idx_patients$lastname, 'both')
duplicate_idx_patients$fullname_dup <- paste(duplicate_idx_patients$firstname, duplicate_idx_patients$lastname)
duplicate_idx_patients$fullname_dup <- tolower(duplicate_idx_patients$fullname_dup)

duplicate_idx_patients_inq <- duplicate_idx_patients %>% filter(registeredby == "HCOUSERINQ" | updatedby == "HCOUSERINQ")

#4 - merge registration and duplication into a fortification file

fortify_reg_activity <- iq_regactivity_ca[which(iq_regactivity_ca$fullname_reg %in% duplicate_idx_patients_inq$fullname_dup),]

#5 - add flags to registration file

iq_regactivity_ca$fortfied_dupe <- ifelse(iq_regactivity_ca$fullname_reg %in% fortify_reg_activity$fullname_reg, 1, 0)

dupe_merge <- list(iq_regactivity_ca$preflagged_dupe, iq_regactivity_ca$fortfied_dupe)
iq_regactivity_ca$aggressive_duplication <- Reduce("|", dupe_merge)
iq_regactivity_ca$aggressive_duplication <- ifelse(iq_regactivity_ca$aggressive_duplication, "duplicate", "valid")

#adding flags for Excel to summarise easier
iq_regactivity_ca$agg_dupe_numeric <- ifelse(iq_regactivity_ca$aggressive_duplication=="duplicate",1,0)
iq_regactivity_ca$base_dupe_numeric <- ifelse(iq_regactivity_ca$baseline_duplication=="duplicate",1,0)

#more flags
iq_regactivity_ca <- iq_regactivity_ca[order(iq_regactivity_ca$registrationdate),]
iq_regactivity_ca$cumsum_baseline <- cumsum(iq_regactivity_ca$base_dupe_numeric)
iq_regactivity_ca$cumsum_aggressive <- cumsum(iq_regactivity_ca$agg_dupe_numeric)

#6 - let's see what happens when I remove obvious test records
setwd("C:/Users/tvickers/Desktop/Projects/Duplicate_MRN_INQ/auxiliary_files")
names_list <- read.csv("grab_test.csv", stringsAsFactors = F)
test_names <- names_list %>% filter(names_list$name_type=="test") %>% select(names)

iq_regactivity_ca$is_test <- ifelse(iq_regactivity_ca$fullname_reg %in% test_names$names, "test","not_test")

#table is finalized--produce a few summary statistics.

setwd("C:/Users/tvickers/Desktop/Projects/Duplicate_MRN_INQ/Report")
#write.csv(iq_regactivity_ca, "duplicate_mrn_report.csv", row.names=F)

dupes_by_month <- iq_regactivity_ca %>% group_by(yyyy_mm_dd) %>% summarise(cnt = n(),
                                                                           base_dupe = sum(baseline_duplication=="duplicate"),
                                                                           base_dupe_rate = (base_dupe/cnt),
                                                                           agg_dupe = sum(aggressive_duplication=="duplicate"),
                                                                           agg_dupe_rate = (agg_dupe/cnt)) %>% mutate(csum_base = cumsum(base_dupe), csum_agg = cumsum(agg_dupe)) %>% mutate(base_diff = c(NA, diff(csum_base)), agg_diff = c(NA, diff(csum_agg)))
dupes_by_month$yyyy_mm_dd <- as.factor(dupes_by_month$yyyy_mm_dd)

dupes_by_month_testrm <- iq_regactivity_ca %>% filter(is_test=="not_test") %>% group_by(yyyy_mm_dd) %>% summarise(cnt = n(),
                                                                           base_dupe = sum(baseline_duplication=="duplicate"),
                                                                           base_dupe_rate = (base_dupe/cnt),
                                                                           agg_dupe = sum(aggressive_duplication=="duplicate"),
                                                                           agg_dupe_rate = (agg_dupe/cnt)) %>% mutate(csum_base = cumsum(base_dupe), csum_agg = cumsum(agg_dupe)) %>% mutate(base_diff = c(NA, diff(csum_base)), agg_diff = c(NA, diff(csum_agg)))


#write.csv(dupes_by_month, "dupes_by_month.csv", row.names=F)
#write.csv(dupes_by_month_testrm, "dupes_by_month_testrm.csv", row.names=F)

#add AZ
setwd("C:/Users/tvickers/Desktop/Projects/Duplicate_MRN_INQ/AZ Feed")
iq_regactivity_az <- read_excel("InQuicker MRN Report January.xls", skip = 1, col_names = c("mrn", "birthdate", "gender", "lastname", "firstname", "group",
                                                                                     "email", "citystate", "address_one", "address_two", "phonenumber",
                                                                                     "zip", "createdon", "createdby", "mergecomment"))
iq_regactivity_az$mergecomment <- tolower(iq_regactivity_az$mergecomment)
iq_regactivity_az$validity <- NA
iq_regactivity_az$validity <- ifelse(is.na(iq_regactivity_az$mergecomment), "valid", "duplicate")
iq_regactivity_az$validity_numeric <- as.numeric(iq_regactivity_az$validity=="duplicate")
iq_regactivity_az$createdon <- as.Date(iq_regactivity_az$createdon)

iq_regactivity_az <- merge(iq_regactivity_az, date_table, by.x = "createdon", by.y = "day")

#write out raw data for az
setwd("C:/Users/tvickers/Desktop/Projects/Duplicate_MRN_INQ/Report")
write.csv(iq_regactivity_az, "duplicate_mrn_report_az.csv", row.names=F)

#calculate and write out the monthly breakdown
dupes_by_month_az <- iq_regactivity_az %>% group_by(yyyy_mm_dd) %>% summarise(cnt = n(),
                                                                           base_dupe = sum(validity=="duplicate"),
                                                                           base_dupe_rate = (base_dupe/cnt))
write.csv(dupes_by_month_az, "dupes_by_month_az.csv", row.names=F)




