#Filtering for residency, 3 years or more

#Pre-pandemic
rm(list=ls())
gc()
pacman::p_load(dplyr,readr,data.table,stringr, lubridate, janitor)

setwd('C:/Data/Shared Folder/People/Rubyath/Data/Working files/Intermediate data/Period 1')
std_cc1_virus_unq <- read_csv2("std_cc1_virus_unq.csv")

setwd('C:/Data/Shared Folder/People/Rubyath/Data/Master files')
residency <- read.csv("ari cases_residency.csv")
#colSums(is.na(residency))

class(residency$lopnr)
class(std_cc1_virus_unq$lopnr.x)
residency_cc1 <- residency %>% mutate(lopnr = as.numeric(lopnr)) %>% semi_join(std_cc1_virus_unq, by = c("lopnr" = "lopnr.x"))
attributes(residency_cc1$in_date)
attributes(residency_cc1$out_date)
attributes(std_cc1_virus_unq$first_admission_datetime)
residency_cc1 <- residency_cc1 %>% mutate(in_date = as.Date(in_date), out_date = as.Date(out_date))
std_cc1_virus <- std_cc1_virus_unq %>% mutate(first_admission_datetime = as.Date(first_admission_datetime))
residency_cc1 <- residency_cc1 %>% semi_join(std_cc1_virus, join_by(lopnr == lopnr.x, in_date <= first_admission_datetime, out_date >= first_admission_datetime))

any(duplicated(residency_cc1$lopnr))
resi_dupes <- get_dupes(residency_cc1, lopnr)
rm(resi_dupes)
#simple deduplication as no decision needed for which one to delete, departure after admission
residency_cc1 <- residency_cc1 %>% distinct(lopnr, .keep_all = TRUE)
std_cc1_virus <- left_join(std_cc1_virus, residency_cc1, by = c("lopnr.x" = "lopnr"))
std_cc1_na <- std_cc1_virus %>% filter(is.na(in_date))
std_cc1_virus <- std_cc1_virus %>% mutate(residency_years = round((interval(in_date, first_admission_datetime)/ years(1)), digits = 1))
std_cc1_virus <- std_cc1_virus %>% filter(residency_years >= 3)

setwd('C:/Data/Shared Folder/People/Rubyath/Data/Working files/Intermediate data/Period 1')
#fwrite(std_cc1_virus, file = "std_cc1_virus.csv", sep = ";", quote = TRUE, bom = TRUE)
std_cc1_na_resi <- left_join(std_cc1_na, residency, by = c("lopnr.x" = "lopnr"))
#fwrite(std_cc1_na_resi, file = "std_cc1_na_resi.csv", sep = ";", quote = TRUE, bom = TRUE)

###---#####
#Pandemic

rm(list=ls())
setwd('C:/Data/Shared Folder/People/Rubyath/Data/Working files/Intermediate data/Period 2')
std_cc2_virus_unq <- read_csv2("std_cc2_virus_unq.csv")

setwd('C:/Data/Shared Folder/People/Rubyath/Data/Master files')
residency <- read.csv("ari cases_residency.csv")

class(residency$lopnr)
class(std_cc2_virus_unq$lopnr.x)
residency_cc2 <- residency %>% mutate(lopnr = as.numeric(lopnr)) %>% semi_join(std_cc2_virus_unq, by = c("lopnr" = "lopnr.x"))
attributes(residency_cc2$in_date)
attributes(residency_cc2$out_date)
attributes(std_cc2_virus_unq$first_admission_datetime)
residency_cc2 <- residency_cc2 %>% mutate(in_date = as.Date(in_date), out_date = as.Date(out_date))
std_cc2_virus <- std_cc2_virus_unq %>% mutate(first_admission_datetime = as.Date(first_admission_datetime))
residency_cc2 <- residency_cc2 %>% semi_join(std_cc2_virus, join_by(lopnr == lopnr.x, in_date <= first_admission_datetime, out_date >= first_admission_datetime))

any(duplicated(residency_cc2$lopnr))
std_cc2_virus <- left_join(std_cc2_virus, residency_cc2, by = c("lopnr.x" = "lopnr"))
std_cc2_na <- std_cc2_virus %>% filter(is.na(in_date))
std_cc2_virus <- std_cc2_virus %>% mutate(residency_years = round((interval(in_date, first_admission_datetime)/ years(1)), digits = 1))
std_cc2_virus <- std_cc2_virus %>% filter(residency_years >= 3)

setwd('C:/Data/Shared Folder/People/Rubyath/Data/Working files/Intermediate data/Period 2')
fwrite(std_cc2_virus, file = "std_cc2_virus.csv", sep = ";", quote = TRUE, bom = TRUE)
std_cc2_na_resi <- left_join(std_cc2_na, residency, by = c("lopnr.x" = "lopnr"))
fwrite(std_cc2_na_resi, file = "std_cc2_na_resi.csv", sep = ";", quote = TRUE, bom = TRUE)

##----####
#Post-pandemic

rm(list=ls())
setwd('C:/Data/Shared Folder/People/Rubyath/Data/Working files/Intermediate data/Period 3')
std_cc3_virus_unq <- read_csv2("std_cc3_virus_unq.csv")

setwd('C:/Data/Shared Folder/People/Rubyath/Data/Master files')
residency <- read.csv("ari cases_residency.csv")
#colSums(is.na(residency))

class(residency$lopnr)
class(std_cc3_virus_unq$lopnr.x)
residency_cc3 <- residency %>% mutate(lopnr = as.numeric(lopnr)) %>% semi_join(std_cc3_virus_unq, by = c("lopnr" = "lopnr.x"))
attributes(residency_cc3$in_date)
attributes(residency_cc3$out_date)
attributes(std_cc3_virus_unq$first_admission_datetime)
residency_cc3 <- residency_cc3 %>% mutate(in_date = as.Date(in_date), out_date = as.Date(out_date))
std_cc3_virus <- std_cc3_virus_unq %>% mutate(first_admission_datetime = as.Date(first_admission_datetime))
residency_cc3 <- residency_cc3 %>% semi_join(std_cc3_virus, join_by(lopnr == lopnr.x, in_date <= first_admission_datetime, out_date >= first_admission_datetime))

any(duplicated(residency_cc3$lopnr))
std_cc3_virus <- left_join(std_cc3_virus, residency_cc3, by = c("lopnr.x" = "lopnr"))
std_cc3_na <- std_cc3_virus %>% filter(is.na(in_date))
std_cc3_virus <- std_cc3_virus %>% mutate(residency_years = round((interval(in_date, first_admission_datetime)/ years(1)), digits = 1))
std_cc3_virus <- std_cc3_virus %>% filter(residency_years >= 3)

setwd('C:/Data/Shared Folder/People/Rubyath/Data/Working files/Intermediate data/Period 3')
fwrite(std_cc3_virus, file = "std_cc3_virus.csv", sep = ";", quote = TRUE, bom = TRUE)
std_cc3_na_resi <- left_join(std_cc3_na, residency, by = c("lopnr.x" = "lopnr"))
fwrite(std_cc3_na_resi, file = "std_cc3_na_resi.csv", sep = ";", quote = TRUE, bom = TRUE)