#Pre-pandemic

rm(list=ls())
pacman::p_load(dplyr, readr)
setwd('C:/Data/Shared Folder/People/Rubyath/Data/Working files/Care chains and Diagnoses')
diag_period1 <- read_csv2("diag_period1_pre.csv")
names(diag_period1)

#Filtering acute RTI ICD-10 codes and main diagnoses
codes_arti <- c("B349", "B974", "J00", "J01", "J010", "J011", "J012", "J013", "J014", "J018", "J019", "J02", "J020", "J028", "J029", "J03", "J030", "J038", "J039", "J04", "J040", "J041", "J042", "J05", "J050", "J06", "J060", "J069", "J09", "J090", "J099", "J10", "J100", "J101", "J108", "J11", "J110", "J111", "J118", "J12", "J120", "J121", "J122", "J123", "J128", "J129", "J13", "J130", "J139", "J14", "J140", "J149", "J15", "J150", "J151", "J152", "J153", "J154", "J155", "J156", "J157", "J158", "J159", "J16", "J160", "J168", "J17", "J170", "J171", "J172", "J173", "J178", "J18", "J180", "J181", "J182", "J188", "J189", "J20", "J200", "J201", "J202", "J203", "J204", "J205", "J206", "J207", "J208", "J209", "J21","J210", "J211", "J218", "J219", "J22", "J220", "J229", "J440", "U071", "U072")
arti_diag_period1 <- diag_period1 %>% filter(diag_code %in% codes_arti)
arti_period1_huvuddiag <- arti_diag_period1 %>% filter(diag_code_pos == 1)

#Checking duplicates
any(duplicated(arti_period1_huvuddiag$slv_table_id))
#if multiple main diagnoses present for the episodes
df_flag <- arti_period1_huvuddiag %>% group_by(slv_table_id) %>% mutate(dup = duplicated(diag_code_pos) | duplicated(diag_code_pos, fromLast = TRUE)) %>% ungroup()
df_dup <- df_flag %>% filter(dup)
rm(df_dup)
write.csv2(arti_period1_huvuddiag, "arti_period1_huvuddiag.csv")

care_chain_period1 <- read_csv2("care_chains_period1_pre.csv")
colSums(is.na(care_chain_period1))
#Joining care chains and diagnoses
care_chain_diag_period1 <- full_join(care_chain_period1, arti_period1_huvuddiag, by = c("first_admission_id" = "slv_table_id"))
#cross-checking if no match
unmatched_diag1 <- anti_join(arti_period1_huvuddiag, care_chain_period1, by = c("slv_table_id" = "first_admission_id"))
#checking mismatch of lopnr
care_chain_diag_period1$care_chain_diag_period1_check <- ifelse(care_chain_diag_period1$lopnr.x == care_chain_diag_period1$lopnr.y, 0, 1)
lopnr_check <- sum(care_chain_diag_period1$care_chain_diag_period1_check == 1, na.rm = TRUE)

####----###
#Pandemic

rm(list=ls())
pacman::p_load(dplyr, readr)
setwd('C:/Data/Shared Folder/People/Rubyath/Data/Working files/Care chains and Diagnoses')
diag_period2 <- read_csv2("diag_period2_pand.csv")
names(diag_period2)

#Filtering acute RTI ICD-10 codes
codes_arti <- c("B349", "B974", "J00", "J01", "J010", "J011", "J012", "J013", "J014", "J018", "J019", "J02", "J020", "J028", "J029", "J03", "J030", "J038", "J039", "J04", "J040", "J041", "J042", "J05", "J050", "J06", "J060", "J069", "J09", "J090", "J099", "J10", "J100", "J101", "J108", "J11", "J110", "J111", "J118", "J12", "J120", "J121", "J122", "J123", "J128", "J129", "J13", "J130", "J139", "J14", "J140", "J149", "J15", "J150", "J151", "J152", "J153", "J154", "J155", "J156", "J157", "J158", "J159", "J16", "J160", "J168", "J17", "J170", "J171", "J172", "J173", "J178", "J18", "J180", "J181", "J182", "J188", "J189", "J20", "J200", "J201", "J202", "J203", "J204", "J205", "J206", "J207", "J208", "J209", "J21","J210", "J211", "J218", "J219", "J22", "J220", "J229", "J440", "U071", "U072")
arti_diag_period2 <- diag_period2 %>% filter(diag_code %in% codes_arti)
arti_period2_huvuddiag <- arti_diag_period2 %>% filter(diag_code_pos == 1)

#Checking duplicates
any(duplicated(arti_period2_huvuddiag$slv_table_id))
#if multiple main diagnoses present for the episodes
df_flag <- arti_period2_huvuddiag %>% group_by(slv_table_id) %>% mutate(dup = duplicated(diag_code_pos) | duplicated(diag_code_pos, fromLast = TRUE)) %>% ungroup()
df_dup <- df_flag %>% filter(dup)
rm(df_dup)
write.csv2(arti_period2_huvuddiag, "arti_period2_huvuddiag.csv")

care_chain_period2 <- read_csv2("care_chains_period2_pand.csv")
colSums(is.na(care_chain_period2))
#Joining care chains and diagnoses
care_chain_diag_period2 <- full_join(care_chain_period2, arti_period2_huvuddiag, by = c("first_admission_id" = "slv_table_id"))
#cross-checking if no match
unmatched_diag2 <- anti_join(arti_period2_huvuddiag, care_chain_period2, by = c("slv_table_id" = "first_admission_id"))
#checking mismatch of lopnr
care_chain_diag_period2$care_chain_diag_period2_check <- ifelse(care_chain_diag_period2$lopnr.x == care_chain_diag_period2$lopnr.y, 0, 1)
lopnr_check <- sum(care_chain_diag_period2$care_chain_diag_period2_check == 1, na.rm = TRUE)

###----#####
#Post-pandemic


rm(list=ls())
pacman::p_load(dplyr, readr)
setwd('C:/Data/Shared Folder/People/Rubyath/Data/Master files/Master_Care chains and Diagnoses')
diag_period3 <- read_csv2("diag_period3_post.csv")
names(diag_period3)

#Filtering acute RTI ICD-10 codes
codes_arti <- c("B349", "B974", "J00", "J01", "J010", "J011", "J012", "J013", "J014", "J018", "J019", "J02", "J020", "J028", "J029", "J03", "J030", "J038", "J039", "J04", "J040", "J041", "J042", "J05", "J050", "J06", "J060", "J069", "J09", "J090", "J099", "J10", "J100", "J101", "J108", "J11", "J110", "J111", "J118", "J12", "J120", "J121", "J122", "J123", "J128", "J129", "J13", "J130", "J139", "J14", "J140", "J149", "J15", "J150", "J151", "J152", "J153", "J154", "J155", "J156", "J157", "J158", "J159", "J16", "J160", "J168", "J17", "J170", "J171", "J172", "J173", "J178", "J18", "J180", "J181", "J182", "J188", "J189", "J20", "J200", "J201", "J202", "J203", "J204", "J205", "J206", "J207", "J208", "J209", "J21","J210", "J211", "J218", "J219", "J22", "J220", "J229", "J440", "U071", "U072")
arti_diag_period3 <- diag_period3 %>% filter(diag_code %in% codes_arti)
arti_period3_huvuddiag <- arti_diag_period3 %>% filter(diag_code_pos == 1)

#Checking duplicates
any(duplicated(arti_period3_huvuddiag$slv_table_id))
df_flag <- arti_period3_huvuddiag %>% group_by(slv_table_id) %>% mutate(dup = duplicated(diag_code_pos) | duplicated(diag_code_pos, fromLast = TRUE)) %>% ungroup()
df_dup <- df_flag %>% filter(dup)
rm(df_dup)
write.csv2(arti_period3_huvuddiag, "arti_period3_huvuddiag.csv")

care_chain_period3 <- read_csv2("care_chains_period3_post.csv")
colSums(is.na(care_chain_period3))
#Joining care chains and diagnoses
care_chain_diag_period3 <- full_join(care_chain_period3, arti_period3_huvuddiag, by = c("first_admission_id" = "slv_table_id"))
#cross-checking if no match
unmatched_diag2 <- anti_join(arti_period3_huvuddiag, care_chain_period3, by = c("slv_table_id" = "first_admission_id"))
#checking mismatch of lopnr
care_chain_diag_period3$care_chain_diag_period3_check <- ifelse(care_chain_diag_period3$lopnr.x == care_chain_diag_period3$lopnr.y, 0, 1)
lopnr_check <- sum(care_chain_diag_period3$care_chain_diag_period3_check == 1, na.rm = TRUE)

##-----#####
#Adjusting the files as per period defintion of the study
#Pre-pandemic (2017-01-01 to 2020-02-28)
#Pandemic (2020-03-01 to 2022-03-31)
#Post-pandemic (2022-04-01 to 2024-12-31)

rm(list=ls())
pacman::p_load(dplyr, readr, lubridate, data.table)
setwd('C:/Data/Shared Folder/People/Rubyath/Data/Working files/Intermediate data')

#confirming they have the same encoding before merging
#guess_encoding("care_chain_diag_period1.csv")
#guess_encoding("care_chain_diag_period2.csv")
#guess_encoding("care_chain_diag_period3.csv")

cc1 <- read_csv2("care_chain_diag_period1.csv")
care_chain_diag_period1 <- cc1 %>% select(-c(lopnr.y, diag_code_pos))
cc2 <- read_csv2("care_chain_diag_period2.csv")
cc3 <- read_csv2("care_chain_diag_period3.csv")

cc2_3 <- bind_rows(cc2, cc3)
attributes(cc2_3$first_admission_datetime)
cc2_3 <- cc2_3 %>% mutate(first_admission_date = as.Date(first_admission_datetime, tz = "UTC"))

care_chain_diag_period2 <- cc2_3 %>% filter(first_admission_date < "2022-04-01")
care_chain_diag_period2 <- care_chain_diag_period2 %>% select(-c(lopnr.y, diag_code_pos, first_admission_date))
care_chain_diag_period3 <- cc2_3 %>% filter(first_admission_date > "2022-03-31")
care_chain_diag_period3 <- care_chain_diag_period3 %>% select(-c(lopnr.y, diag_code_pos, first_admission_date))

fwrite(care_chain_diag_period1, "care_chain_diag_period1.csv", sep = ";", quote = TRUE, bom = TRUE)
fwrite(care_chain_diag_period2, "care_chain_diag_period2.csv", sep = ";", quote = TRUE, bom = TRUE)
fwrite(care_chain_diag_period3, "care_chain_diag_period3.csv", sep = ";", quote = TRUE, bom = TRUE)

##------####
#Joining hospital names where first admission happened to filter target hospitals later

rm(list = ls())
pacman::p_load(dplyr, readr, data.table, lubridate, stringr, janitor)

setwd('C:/Data/Shared Folder/People/Rubyath/Data/Working files/Intermediate data')
care_chain_diag_period1 <- read_csv2("care_chain_diag_period1.csv")
care_chain_diag_period2 <- read_csv2("care_chain_diag_period2.csv")
care_chain_diag_period3 <- read_csv2("care_chain_diag_period3.csv")
setwd('C:/Data/Shared Folder/People/Rubyath/Data/Master files')
hospital_first <- read.csv("ari cases_hospital name.csv")
unique(care_chain_diag_period1$inrattning_name)
target_hos <- c("Karolinska Universitetssjukhuset Huddinge", "Karolinska Universitetssjukhuset  Solna", "Danderyds sjukhus", "Södertälje sjukhus", "Södersjukhuset", "NKS", "Norrtälje sjh / Norrtälje Norra Vc")
any(duplicated(hospital_first$slv_table_id))

care_chain_diag_period1 <- left_join(care_chain_diag_period1, hospital_first, by = "slv_table_id")
checking_na <- sum(is.na(care_chain_diag_period1$inrattning_name))
#rm(checking_na)
care_chain_diag_period1 <- care_chain_diag_period1 %>% filter(inrattning_name %in% target_hos)

care_chain_diag_period2 <- left_join(care_chain_diag_period2, hospital_first, by = "slv_table_id")
checking_na <- sum(is.na(care_chain_diag_period1$inrattning_name))
#rm(checking_na)
care_chain_diag_period2 <- care_chain_diag_period2 %>% filter(inrattning_name %in% target_hos)

care_chain_diag_period3 <- left_join(care_chain_diag_period3, hospital_first, by = "slv_table_id")
checking_na <- sum(is.na(care_chain_diag_period3$inrattning_name))
#rm(checking_na)
care_chain_diag_period3 <- care_chain_diag_period3 %>% filter(inrattning_name %in% target_hos)

setwd('C:/Data/Shared Folder/People/Rubyath/Data/Working files/Intermediate data')
fwrite(care_chain_diag_period1, "care_chain_diag_period1.csv", sep = ";", quote = TRUE, bom = TRUE)
fwrite(care_chain_diag_period2, "care_chain_diag_period2.csv", sep = ";", quote = TRUE, bom = TRUE)
fwrite(care_chain_diag_period3, "care_chain_diag_period3.csv", sep = ";", quote = TRUE, bom = TRUE)
