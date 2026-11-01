#Joining aRTI care chains/episodes with microbiology lab data
##In absence of admission id(slv_table_ID) for micro lab data, lopnr (pseudoID) was used
###To identify the respective sampling info, a rule of sampling within -1 day and +2days were used to join the files

#Pre-pandemic
rm(list=ls())
pacman::p_load(dplyr, readr, lubridate, data.table)

setwd('C:/Data/Shared Folder/People/Rubyath/Data/Master files/Microbial data')
micro_lab <- read_csv2("all_microlab.csv")
#setwd('C:/Data/Shared Folder/People/Rubyath/Data/Working files/Microlab_data/Period 1')
setwd('C:/Data/Shared Folder/People/Rubyath/Data/Working files/Intermediate data/Period 1')
cc1 <- read_csv2("final_cc1_demo.csv")


micro_lab <- micro_lab %>% mutate(SL_No = row_number(), .before = 1)
#Joining care chains with microb lab
cc1_micro_lab <- left_join(cc1, micro_lab, by = c("lopnr.x" = "lopnr"))
fwrite(cc1_micro_lab, file="cc1_micro_lab.csv", sep = ";", quote = TRUE, bom = TRUE)
cc1_micro_lab <- read_csv2("cc1_micro_lab.csv")

#SamplingDateTime has NA and NULL time
colSums(is.na(cc1_micro_lab)) 
cc1_micro_lab_NA <- cc1_micro_lab %>% filter(is.na(SamplingDatetime))
fwrite(cc1_micro_lab_NA, file="cc1_micro_lab_NA.csv", sep = ";", quote = TRUE, bom = TRUE)
cc1_micro_lab_without_NA <- cc1_micro_lab %>% filter(!is.na(SamplingDatetime))
colSums(is.na(cc1_micro_lab_without_NA))
fwrite(cc1_micro_lab_without_NA, file="cc1_micro_lab_without_NA.csv", sep = ";", quote = TRUE, bom = TRUE)
cc1_micro_lab_without_NA <- read_csv2("cc1_micro_lab_without_NA.csv")
cc1_micro_lab_without_NA_NULL <- cc1_micro_lab_without_NA %>% filter(SamplingDatetime != "NULL")
fwrite(cc1_micro_lab_without_NA_NULL, file="cc1_micro_lab_without_NA_NULL.csv", sep = ";", quote = TRUE, bom = TRUE)
cc1_micro_lab_without_NA_SamplingDT_NULL <- cc1_micro_lab_without_NA %>% filter(SamplingDatetime == "NULL")
fwrite(cc1_micro_lab_without_NA_SamplingDT_NULL, file="cc1_micro_lab_without_NA_SamplingDT_NULL.csv", sep = ";", quote = TRUE, bom = TRUE)

rm(list= ls())
cc1_micro_lab_without_NA_NULL <- read_csv2("cc1_micro_lab_without_NA_NULL.csv")
cc1_micro_lab_without_NA_SamplingDT_NULL <- read_csv2("cc1_micro_lab_without_NA_SamplingDT_NULL.csv")

#Checking data type and timezone
cc1_micro_lab_without_NA_SamplingDT_NULL$SamplingDate <- as.POSIXct(as.POSIXlt(cc1_micro_lab_without_NA_SamplingDT_NULL$SamplingDate, tz = "UTC"))
attributes(cc1_micro_lab_without_NA_SamplingDT_NULL$SamplingDate)
format(cc1_micro_lab_without_NA_SamplingDT_NULL$SamplingDate, "%Y-%m-%d %H:%M:%S %Z")
as.numeric(cc1_micro_lab_without_NA_SamplingDT_NULL$SamplingDate) %% 86400
cc1_micro_lab_without_NA_SamplingDT_NULL$SamplingDatetime <- cc1_micro_lab_without_NA_SamplingDT_NULL$SamplingDate

attributes(cc1_micro_lab_without_NA_NULL$SamplingDatetime)
attributes(cc1_micro_lab_without_NA_SamplingDT_NULL$SamplingDatetime)

#Both file same class and timezone before merging
cc1_micro_lab_without_NA_NULL <- cc1_micro_lab_without_NA_NULL %>% mutate(SamplingDatetime = as.POSIXct(SamplingDatetime, tz = "UTC"))
cc1_micro_lab_without_NA_SamplingDT_NULL <- cc1_micro_lab_without_NA_SamplingDT_NULL %>% mutate(SamplingDatetime = as.POSIXct(SamplingDatetime, tz = "UTC"))
cc1_micro_lab_without_NA_NULL_imputed <- bind_rows(cc1_micro_lab_without_NA_NULL, cc1_micro_lab_without_NA_SamplingDT_NULL)

#Checking incomplete data time
#cc1_micro_lab_without_NA_NULL_imputed %>% filter(!grepl(":", SamplingDatetime)) %>% summarise(n_missing_time = n())
#filtering for sampling within -1/+2 days of admission
#confirming both file for same class and tomezzone before dependant filtering
attributes(cc1_micro_lab_without_NA_NULL_imputed$SamplingDatetime)
attributes(cc1_micro_lab_without_NA_NULL_imputed$first_admission_datetime)
cc1_micro_lab_without_NA_NULL_dtf <- cc1_micro_lab_without_NA_NULL_imputed %>% filter(SamplingDatetime >= first_admission_datetime - hours(24) & SamplingDatetime <= first_admission_datetime + hours(48))
##checking if not having explicit time is a problem 
#cc1_micro_lab_without_NA_NULL_dtf %>% filter(!grepl(":", SamplingDatetime)) %>% summarise(n_missing_time = n())
#no_time_cc1 <- cc1_micro_lab_without_NA_NULL_dtf %>% filter(!grepl(":", SamplingDatetime))
fwrite(cc1_micro_lab_without_NA_NULL_dtf, file="cc1_micro_lab_without_NA_NULL_dtf.csv", sep = ";", quote = TRUE, bom = TRUE)


#Difference between first admission and sampling date
cc1_micro_lab_without_NA_NULL_dtf$diff_AdmSam <- as.numeric(difftime(cc1_micro_lab_without_NA_NULL_dtf$SamplingDatetime, cc1_micro_lab_without_NA_NULL_dtf$first_admission_datetime, units = "days"))
#If discharge date is before sampling date
cc1_micro_lab_without_NA_NULL_dtf$dif_DisSam <- as.numeric(difftime(cc1_micro_lab_without_NA_NULL_dtf$last_discharge_datetime, cc1_micro_lab_without_NA_NULL_dtf$SamplingDatetime, units = "days"))
any(duplicated(cc1_micro_lab_without_NA_NULL_dtf$SL_No))


###-----####
#Pandemic
rm(list=ls())
pacman::p_load(dplyr, readr, lubridate, data.table)
setwd('C:/Data/Shared Folder/People/Rubyath/Data/Master files/Microbial data')
micro_lab <- read_csv2("all_microlab.csv")
#setwd('C:/Data/Shared Folder/People/Rubyath/Data/Working files/Microlab_data/Period 2')
setwd('C:/Data/Shared Folder/People/Rubyath/Data/Working files/Intermediate data/Period 2')
cc2 <- read_csv2("final_cc2_demo.csv")

micro_lab <- micro_lab %>% mutate(SL_No = row_number(), .before = 1)
#Joining care chains with microb lab
cc2_micro_lab <- left_join(cc2, micro_lab, by = c("lopnr.x" = "lopnr"))
fwrite(cc2_micro_lab, file="cc2_micro_lab.csv", sep = ";", quote = TRUE, bom = TRUE)
cc2_micro_lab <- read_csv2("cc2_micro_lab.csv")

#SamplingDateTime has NA and NULL time
colSums(is.na(cc2_micro_lab)) 
cc2_micro_lab_NA <- cc2_micro_lab %>% filter(is.na(SamplingDatetime))
fwrite(cc2_micro_lab_NA, file="cc2_micro_lab_NA.csv", sep = ";", quote = TRUE, bom = TRUE)
cc2_micro_lab_without_NA <- cc2_micro_lab %>% filter(!is.na(SamplingDatetime))
colSums(is.na(cc2_micro_lab_without_NA))
fwrite(cc2_micro_lab_without_NA, file="cc2_micro_lab_without_NA.csv", sep = ";", quote = TRUE, bom = TRUE)
cc2_micro_lab_without_NA <- read_csv2("cc2_micro_lab_without_NA.csv")
cc2_micro_lab_without_NA_NULL <- cc2_micro_lab_without_NA %>% filter(SamplingDatetime != "NULL")
fwrite(cc2_micro_lab_without_NA_NULL, file="cc2_micro_lab_without_NA_NULL.csv", sep = ";", quote = TRUE, bom = TRUE)
cc2_micro_lab_without_NA_SamplingDT_NULL <- cc2_micro_lab_without_NA %>% filter(SamplingDatetime == "NULL")
fwrite(cc2_micro_lab_without_NA_SamplingDT_NULL, file="cc2_micro_lab_without_NA_SamplingDT_NULL.csv", sep = ";", quote = TRUE, bom = TRUE)

rm(list= ls())
cc2_micro_lab_without_NA_NULL <- read_csv2("cc2_micro_lab_without_NA_NULL.csv")
cc2_micro_lab_without_NA_SamplingDT_NULL <- read_csv2("cc2_micro_lab_without_NA_SamplingDT_NULL.csv")

#Checking data type and timezone
cc2_micro_lab_without_NA_SamplingDT_NULL$SamplingDate <- as.POSIXct(as.POSIXlt(cc2_micro_lab_without_NA_SamplingDT_NULL$SamplingDate, tz = "UTC"))
attributes(cc2_micro_lab_without_NA_SamplingDT_NULL$SamplingDate)
format(cc2_micro_lab_without_NA_SamplingDT_NULL$SamplingDate, "%Y-%m-%d %H:%M:%S %Z")
as.numeric(cc2_micro_lab_without_NA_SamplingDT_NULL$SamplingDate) %% 86400
cc2_micro_lab_without_NA_SamplingDT_NULL$SamplingDatetime <- cc2_micro_lab_without_NA_SamplingDT_NULL$SamplingDate

attributes(cc2_micro_lab_without_NA_NULL$SamplingDatetime)
attributes(cc2_micro_lab_without_NA_SamplingDT_NULL$SamplingDatetime)

#Both file same class and timezone before merging
cc2_micro_lab_without_NA_NULL <- cc2_micro_lab_without_NA_NULL %>% mutate(SamplingDatetime = as.POSIXct(SamplingDatetime, tz = "UTC"))
cc2_micro_lab_without_NA_SamplingDT_NULL <- cc2_micro_lab_without_NA_SamplingDT_NULL %>% mutate(SamplingDatetime = as.POSIXct(SamplingDatetime, tz = "UTC"))
cc2_micro_lab_without_NA_NULL_imputed <- bind_rows(cc2_micro_lab_without_NA_NULL, cc2_micro_lab_without_NA_SamplingDT_NULL)

#Checking incomplete data time
cc2_micro_lab_without_NA_NULL_imputed %>% filter(!grepl(":", SamplingDatetime)) %>% summarise(n_missing_time = n())
#filtering for sampling within -1/+2 days of admission
#confirming both file for same class and tomezzone before conditional filtering
attributes(cc2_micro_lab_without_NA_NULL_imputed$SamplingDatetime)
attributes(cc2_micro_lab_without_NA_NULL_imputed$first_admission_datetime)
cc2_micro_lab_without_NA_NULL_dtf <- cc2_micro_lab_without_NA_NULL_imputed %>% filter(SamplingDatetime >= first_admission_datetime - hours(24) & SamplingDatetime <= first_admission_datetime + hours(48))
##checking if not having explicit time is a problem 
#cc2_micro_lab_without_NA_NULL_dtf %>% filter(!grepl(":", SamplingDatetime)) %>% summarise(n_missing_time = n())
#no_time_cc2 <- cc2_micro_lab_without_NA_NULL_dtf %>% filter(!grepl(":", SamplingDatetime))
fwrite(cc2_micro_lab_without_NA_NULL_dtf, file="cc2_micro_lab_without_NA_NULL_dtf.csv", sep = ";", quote = TRUE, bom = TRUE) 

#Difference between first admission and sampling date
cc2_micro_lab_without_NA_NULL_dtf$diff_AdmSam <- as.numeric(difftime(cc2_micro_lab_without_NA_NULL_dtf$SamplingDatetime, cc2_micro_lab_without_NA_NULL_dtf$first_admission_datetime, units = "days"))
#If discharge date is before sampling date
cc2_micro_lab_without_NA_NULL_dtf$dif_DisSam <- as.numeric(difftime(cc2_micro_lab_without_NA_NULL_dtf$last_discharge_datetime, cc2_micro_lab_without_NA_NULL_dtf$SamplingDatetime, units = "days"))
any(duplicated(cc2_micro_lab_without_NA_NULL_dtf$SL_No))
overlapping_samples <- cc2_micro_lab_without_NA_NULL_dtf %>% filter(duplicated(SL_No) | duplicated(SL_No, fromLast = TRUE))
unique(overlapping_samples$slv_table_id)
unique(overlapping_samples$lopnr.x)
fwrite(overlapping_samples, file= "same_episode.csv", sep = ";", quote = TRUE, bom = TRUE)

###--##
#Post-pandemic
rm(list=ls())
pacman::p_load(dplyr, readr, lubridate, data.table)
setwd('C:/Data/Shared Folder/People/Rubyath/Data/Master files/Microbial data')
micro_lab <- read_csv2("all_microlab.csv")
#setwd('C:/Data/Shared Folder/People/Rubyath/Data/Working files/Microlab_data/Period 3')
setwd('C:/Data/Shared Folder/People/Rubyath/Data/Working files/Intermediate data/Period 3')
cc3 <- read_csv2("final_cc3_demo.csv")

micro_lab <- micro_lab %>% mutate(SL_No = row_number(), .before = 1)
#Joining care chains with microb lab
cc3_micro_lab <- left_join(cc3, micro_lab, by = c("lopnr.x" = "lopnr"))
fwrite(cc3_micro_lab, file="cc3_micro_lab.csv", sep = ";", quote = TRUE, bom = TRUE)
cc3_micro_lab <- read_csv2("cc3_micro_lab.csv")

#SamplingDateTime has NA and NULL time
colSums(is.na(cc3_micro_lab)) 
cc3_micro_lab_NA <- cc3_micro_lab %>% filter(is.na(SamplingDatetime))
fwrite(cc3_micro_lab_NA, file="cc3_micro_lab_NA.csv", sep = ";", quote = TRUE, bom = TRUE)
cc3_micro_lab_without_NA <- cc3_micro_lab %>% filter(!is.na(SamplingDatetime))
colSums(is.na(cc3_micro_lab_without_NA))
fwrite(cc3_micro_lab_without_NA, file="cc3_micro_lab_without_NA.csv", sep = ";", quote = TRUE, bom = TRUE)
cc3_micro_lab_without_NA <- read_csv2("cc3_micro_lab_without_NA.csv")
cc3_micro_lab_without_NA_NULL <- cc3_micro_lab_without_NA %>% filter(SamplingDatetime != "NULL")
fwrite(cc3_micro_lab_without_NA_NULL, file="cc3_micro_lab_without_NA_NULL.csv", sep = ";", quote = TRUE, bom = TRUE)
cc3_micro_lab_without_NA_SamplingDT_NULL <- cc3_micro_lab_without_NA %>% filter(SamplingDatetime == "NULL")
fwrite(cc3_micro_lab_without_NA_SamplingDT_NULL, file="cc3_micro_lab_without_NA_SamplingDT_NULL.csv", sep = ";", quote = TRUE, bom = TRUE)

rm(list= ls())
cc3_micro_lab_without_NA_NULL <- read_csv2("cc3_micro_lab_without_NA_NULL.csv")
cc3_micro_lab_without_NA_SamplingDT_NULL <- read_csv2("cc3_micro_lab_without_NA_SamplingDT_NULL.csv")

#Checking data type and timezone
cc3_micro_lab_without_NA_SamplingDT_NULL$SamplingDate <- as.POSIXct(as.POSIXlt(cc3_micro_lab_without_NA_SamplingDT_NULL$SamplingDate, tz = "UTC"))
attributes(cc3_micro_lab_without_NA_SamplingDT_NULL$SamplingDate)
format(cc3_micro_lab_without_NA_SamplingDT_NULL$SamplingDate, "%Y-%m-%d %H:%M:%S %Z")
as.numeric(cc3_micro_lab_without_NA_SamplingDT_NULL$SamplingDate) %% 86400
cc3_micro_lab_without_NA_SamplingDT_NULL$SamplingDatetime <- cc3_micro_lab_without_NA_SamplingDT_NULL$SamplingDate

attributes(cc3_micro_lab_without_NA_NULL$SamplingDatetime)
attributes(cc3_micro_lab_without_NA_SamplingDT_NULL$SamplingDatetime)

#Both file same class and timezone before merging
cc3_micro_lab_without_NA_NULL <- cc3_micro_lab_without_NA_NULL %>% mutate(SamplingDatetime = as.POSIXct(SamplingDatetime, tz = "UTC"))
cc3_micro_lab_without_NA_SamplingDT_NULL <- cc3_micro_lab_without_NA_SamplingDT_NULL %>% mutate(SamplingDatetime = as.POSIXct(SamplingDatetime, tz = "UTC"))
cc3_micro_lab_without_NA_NULL_imputed <- bind_rows(cc3_micro_lab_without_NA_NULL, cc3_micro_lab_without_NA_SamplingDT_NULL)

#Checking incomplete data time
cc3_micro_lab_without_NA_NULL_imputed %>% filter(!grepl(":", SamplingDatetime)) %>% summarise(n_missing_time = n())
#filtering for sampling within -1/+2 days of admission
#confirming both file for same class and tomezzone before dependant filtering
attributes(cc3_micro_lab_without_NA_NULL_imputed$SamplingDatetime)
attributes(cc3_micro_lab_without_NA_NULL_imputed$first_admission_datetime)
cc3_micro_lab_without_NA_NULL_dtf <- cc3_micro_lab_without_NA_NULL_imputed %>% filter(SamplingDatetime >= first_admission_datetime - hours(24) & SamplingDatetime <= first_admission_datetime + hours(48))
##checking if not having explicit time is a problem 
#cc3_micro_lab_without_NA_NULL_dtf %>% filter(!grepl(":", SamplingDatetime)) %>% summarise(n_missing_time = n())
#no_time_cc3 <- cc3_micro_lab_without_NA_NULL_dtf %>% filter(!grepl(":", SamplingDatetime))
fwrite(cc3_micro_lab_without_NA_NULL_dtf, file="cc3_micro_lab_without_NA_NULL_dtf.csv", sep = ";", quote = TRUE, bom = TRUE) 

#Difference between first admission and sampling date
cc3_micro_lab_without_NA_NULL_dtf$diff_AdmSam <- as.numeric(difftime(cc3_micro_lab_without_NA_NULL_dtf$SamplingDatetime, cc3_micro_lab_without_NA_NULL_dtf$first_admission_datetime, units = "days"))
#If discharge date is before sampling date
cc3_micro_lab_without_NA_NULL_dtf$dif_DisSam <- as.numeric(difftime(cc3_micro_lab_without_NA_NULL_dtf$last_discharge_datetime, cc3_micro_lab_without_NA_NULL_dtf$SamplingDatetime, units = "days"))
any(duplicated(cc3_micro_lab_without_NA_NULL_dtf$SL_No))


#-----additional-----#
#Chunking of big file
#library(data.table)
#n_chunks <- 10
#df %>% mutate(chunk = ntile(row_number(), n_chunks)) %>% group_by(chunk) %>% group_walk(~ fwrite(.x, file = paste0("chunk_", .y$chunk, ".csv"), sep = ";", quote = TRUE, bom = TRUE))