#education cleaning and recoding
rm(list=ls())
pacman::p_load(dplyr, readr, data.table, stringr, lubridate)

setwd('C:/Data/Shared Folder/People/Rubyath/Data/Master files')
edu <- read.csv("ari cases_education.csv")
sort(unique(edu$education_level_name))

#taking the most recent year, if no information for the most recent then the last avaolable education is taken
latest_edu <- edu %>% group_by(lopnr) %>% arrange(education_level_name == "Uppgift saknas", desc(year), .by_group = TRUE) %>% slice_head(n=1) %>% ungroup()
#check_edu <- edu %>% group_by(lopnr) %>% slice_max(order_by = year, n=1, with_ties = FALSE) %>% filter(education_level_name == "Uppgift saknas") %>% ungroup()
#sort(unique(latest_edu$education_level_name))

#standardizing encoding before joining and categorizing
#guess_encoding("ari cases_education.csv")
#problem_columns <- sapply(latest_edu, function(col) {if(is.character(col) || is.factor(col)) {any(!validUTF8(as.character(col)), na.rm = TRUE)} else {FALSE}})
latest_edu$education_level_name <- iconv(latest_edu$education_level_name, from = "latin1", to = "UTF-8")
#rm(edu)
sort(unique(latest_edu$education_level_name))
latest_edu <- latest_edu %>% mutate(education_level_recoded = case_when(education_level_name == "Eftergymnasial utbildning 3 år eller längre (exkl. forskarutbildning)" ~ "Tertiary", education_level_name == "Eftergymnasial utbildning kortare än 3 år" ~ "Tertiary", education_level_name == "Förgymnasial utbildning 9 år" ~ "Primary", education_level_name == "Förgymnasial utbildning kortare än 9 år" ~ "Primary", education_level_name == "Forskarutbildning" ~ "Tertiary", education_level_name == "Gymnasial utbildning 3 år" ~ "Secondary", education_level_name == "Gymnasial utbildning högst 2-årig" ~ "Secondary", education_level_name == "Uppgift saknas" ~ "Missing"))

setwd('C:/Data/Shared Folder/People/Rubyath/Data/Working files/Intermediate data/Period 1')
cc1_microlab_recoded <- read_csv2("cc1_microlab_recoded.csv")
setwd('C:/Data/Shared Folder/People/Rubyath/Data/Working files/Intermediate data/Period 2')
cc2_microlab_recoded <- read_csv2("cc2_microlab_recoded.csv")
setwd('C:/Data/Shared Folder/People/Rubyath/Data/Working files/Intermediate data/Period 3')
cc3_microlab_recoded <- read_csv2("cc3_microlab_recoded.csv")

class(latest_edu$lopnr)
class(cc1_microlab_recoded$lopnr.x)
cc1_microlab_recoded <- cc1_microlab_recoded %>% mutate(lopnr.x = as.integer(lopnr.x))
class(cc2_microlab_recoded$lopnr.x)
cc2_microlab_recoded <- cc2_microlab_recoded %>% mutate(lopnr.x = as.integer(lopnr.x))
class(cc3_microlab_recoded$lopnr.x)
cc3_microlab_recoded <- cc3_microlab_recoded %>% mutate(lopnr.x = as.integer(lopnr.x))
cc1_microlab_scedemo <- left_join(cc1_microlab_recoded, latest_edu, by = c("lopnr.x" = "lopnr"))
cc2_microlab_scedemo <- left_join(cc2_microlab_recoded, latest_edu, by = c("lopnr.x" = "lopnr"))
cc3_microlab_scedemo <- left_join(cc3_microlab_recoded, latest_edu, by = c("lopnr.x" = "lopnr"))
rm(cc1_microlab_recoded)
rm(cc2_microlab_recoded)
rm(cc3_microlab_recoded)

#Demographic
problem_columns <- sapply(cc1_microlab_scedemo, function(col) {if(is.character(col) || is.factor(col)) {any(!validUTF8(as.character(col)), na.rm = TRUE)} else {FALSE}})
problem_columns
cc1_microlab_scedemo$fodelselandgrupp <- iconv(cc1_microlab_scedemo$fodelselandgrupp, from = "latin1", to = "UTF-8")
unique(sort(cc1_microlab_scedemo$fodelselandgrupp))
cc1_microlab_scedemo <- cc1_microlab_scedemo %>% mutate(mig_background = case_when(fodelselandgrupp == "Sverige" ~ "Swedish", fodelselandgrupp == "Okänt" ~ "Missing", TRUE ~ "Foreign"))
cc1_microlab_scedemo <- cc1_microlab_scedemo %>% mutate(age_group = case_when(age_year < 50 ~ "18-49 years", age_year < 60 ~ "50-59 years", age_year < 70 ~ "60-69 years", age_year < 80 ~ "70-79 years", TRUE ~ ">= 80 years"))
colnames(cc1_microlab_scedemo)
cc1_microlab_scedemo <- cc1_microlab_scedemo %>% select(lopnr.x, slv_table_id, age_year, age_group, sex, mig_background,  education_level_recoded, diag_code, inrattning_name, first_admission_id, first_admission_datetime, first_admission_year, SamplingDatetime, last_discharge_datetime, diagnosis_date, care_episode, days_of_stay, count_admissions_in_chain, chain_overlaps_icu, SL_No, AnalysisID, Examination, Specimen1, Analysis, recoded_analysis, specimen, organism, result, everything())
cc1_microlab_scedemo <- cc1_microlab_scedemo %>% select(-c(last_discharge_id, lopnr, fodelsear, sex_long, fodelselandgrupp, fodelselandgrupp_mor, fodelselandgrupp_far, SamplingDate, ReplyDatetime, ReplyType, NameOfCareUnit, sun2000niva_old, education_level_name, year))

problem_columns <- sapply(cc2_microlab_scedemo, function(col) {if(is.character(col) || is.factor(col)) {any(!validUTF8(as.character(col)), na.rm = TRUE)} else {FALSE}})
problem_columns
unique(sort(cc2_microlab_scedemo$fodelselandgrupp))
cc2_microlab_scedemo <- cc2_microlab_scedemo %>% mutate(mig_background = case_when(fodelselandgrupp == "Sverige" ~ "Swedish", fodelselandgrupp == "Okänt" ~ "Missing", TRUE ~ "Foreign"))
cc2_microlab_scedemo <- cc2_microlab_scedemo %>% mutate(age_group = case_when(age_year < 50 ~ "18-49 years", age_year < 60 ~ "50-59 years", age_year < 70 ~ "60-69 years", age_year < 80 ~ "70-79 years", TRUE ~ ">= 80 years"))
cc2_microlab_scedemo <- cc2_microlab_scedemo %>% select(lopnr.x, slv_table_id, age_year, age_group, sex, mig_background,  education_level_recoded, diag_code, inrattning_name, first_admission_id, first_admission_datetime, first_admission_year, SamplingDatetime, last_discharge_datetime, diagnosis_date, care_episode, days_of_stay, count_admissions_in_chain, chain_overlaps_icu, SL_No, AnalysisID, Examination, Specimen1, Analysis, recoded_analysis, specimen, organism, result, everything())
cc2_microlab_scedemo <- cc2_microlab_scedemo %>% select(-c(last_discharge_id, lopnr, fodelsear, sex_long, fodelselandgrupp, fodelselandgrupp_mor, fodelselandgrupp_far, SamplingDate, ReplyDatetime, ReplyType, NameOfCareUnit, sun2000niva_old, education_level_name, year))

problem_columns <- sapply(cc3_microlab_scedemo, function(col) {if(is.character(col) || is.factor(col)) {any(!validUTF8(as.character(col)), na.rm = TRUE)} else {FALSE}})
problem_columns
cc3_microlab_scedemo$fodelselandgrupp <- iconv(cc3_microlab_scedemo$fodelselandgrupp, from = "latin1", to = "UTF-8")
unique(sort(cc3_microlab_scedemo$fodelselandgrupp))
cc3_microlab_scedemo <- cc3_microlab_scedemo %>% mutate(mig_background = case_when(fodelselandgrupp == "Sverige" ~ "Swedish", fodelselandgrupp == "Okänt" ~ "Missing", TRUE ~ "Foreign"))
cc3_microlab_scedemo <- cc3_microlab_scedemo %>% mutate(age_group = case_when(age_year < 50 ~ "18-49 years", age_year < 60 ~ "50-59 years", age_year < 70 ~ "60-69 years", age_year < 80 ~ "70-79 years", TRUE ~ ">= 80 years"))
cc3_microlab_scedemo <- cc3_microlab_scedemo %>% select(lopnr.x, slv_table_id, age_year, age_group, sex, mig_background,  education_level_recoded, diag_code, inrattning_name, first_admission_id, first_admission_datetime, first_admission_year, SamplingDatetime, last_discharge_datetime, diagnosis_date, care_episode, days_of_stay, count_admissions_in_chain, chain_overlaps_icu, SL_No, AnalysisID, Examination, Specimen1, Analysis, recoded_analysis, specimen, organism, result, everything())
cc3_microlab_scedemo <- cc3_microlab_scedemo %>% select(-c(last_discharge_id, lopnr, fodelsear, sex_long, fodelselandgrupp, fodelselandgrupp_mor, fodelselandgrupp_far, SamplingDate, ReplyDatetime, ReplyType, NameOfCareUnit, sun2000niva_old, education_level_name, year))

fwrite(cc1_microlab_scedemo, file = "cc1_microlab_scedemo.csv", sep = ";", quote = TRUE, bom = TRUE)
fwrite(cc2_microlab_scedemo, file = "cc2_microlab_scedemo.csv", sep = ";", quote = TRUE, bom = TRUE)
fwrite(cc3_microlab_scedemo, file = "cc3_microlab_scedemo.csv", sep = ";", quote = TRUE, bom = TRUE)
