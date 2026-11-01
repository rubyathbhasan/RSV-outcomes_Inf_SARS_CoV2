#joining icu and mechanical ventilation data
#filtering icu and mv data for period 1, 2 and 3 in separate files

rm(list=ls())
gc()
pacman::p_load(dplyr, readr, data.table, lubridate, stringr, janitor)

setwd('C:/Data/Shared Folder/People/Rubyath/Data/Master files/Clinical outcome')
icu_adm <- read.csv("ari cases_icu admissions.csv")
mec_vent <- read.csv("ari cases_mechanical ventilation.csv")
problem_columns <- sapply(icu_adm, function(col) {if(is.character(col) || is.factor(col)) {any(!validUTF8(as.character(col)), na.rm = TRUE)} else {FALSE}})
problem_columns
rm(problem_columns)
icu_adm$avd <- iconv(icu_adm$avd, from = "latin1", to = "UTF-8")
icu_adm$vardtyp <- iconv(icu_adm$vardtyp, from = "latin1", to = "UTF-8")

icua_mecv <- full_join(icu_adm, mec_vent, by = "vtfhuvudid")
unmatched <- anti_join(mec_vent, icu_adm, by = "vtfhuvudid")
icua_mecv <- icua_mecv %>% mutate(mech_ven = case_when(!is.na(lopnr.y) ~ "Yes", TRUE ~ "No"))
icua_mecv$icu <- "Yes"

#duplicated entry of vtfhuvudid with same information
any(duplicated(icua_mecv$vtfhuvudid))
any(duplicated(icua_mecv$vtfhuvudid))
icua_mecv <- icua_mecv %>% distinct(vtfhuvudid, .keep_all = TRUE)
rm(icu_adm)
rm(mec_vent)
rm(unmatched)

setwd('C:/Data/Shared Folder/People/Rubyath/Data/Working files/Intermediate data/Period 1')
std_cc1_virus <- read_csv2("std_cc1_virus.csv")
setwd('C:/Data/Shared Folder/People/Rubyath/Data/Working files/Intermediate data/Period 2')
std_cc2_virus <- read_csv2("std_cc2_virus.csv")
setwd('C:/Data/Shared Folder/People/Rubyath/Data/Working files/Intermediate data/Period 3')
std_cc3_virus <- read_csv2("std_cc3_virus.csv")

icu_mv_cc1 <- icua_mecv %>% semi_join(std_cc1_virus, by = c("lopnr.x" = "lopnr.x"))
icu_mv_cc2 <- icua_mecv %>% semi_join(std_cc2_virus, by = c("lopnr.x" = "lopnr.x"))
icu_mv_cc3 <- icua_mecv %>% semi_join(std_cc3_virus, by = c("lopnr.x" = "lopnr.x"))
rm(icua_mecv)

#Pre-pandemic

#Joining icu and mv with study population
icu_mv_cc1 <- icu_mv_cc1 %>% mutate(inskrtidpunkt = as.Date(inskrtidpunkt), utskrtidpunkt = as.Date(utskrtidpunkt))
std_cc1_icu_mv <- std_cc1_virus %>% left_join(icu_mv_cc1, join_by(lopnr.x, first_admission_datetime <= inskrtidpunkt, last_discharge_datetime >= inskrtidpunkt))
#duplicate slv_table_ids mean that one episode has multiple ICU admissions
#just to confirm #slv_duplicates <- get_dupes(std_cc1_icu_mv, slv_table_id) 
std_cc1_icu_mv <- std_cc1_icu_mv %>% group_by(slv_table_id) %>% arrange(desc(mech_ven == "Yes"), inskrtidpunkt, .by_group = TRUE) %>% slice(1) %>% ungroup()

#Reducing dimensionality
colnames(std_cc1_icu_mv)
std_cc1_icu_mv <- std_cc1_icu_mv %>% select(-c("lopnr.y","avd", "vardtyp"))
class(std_cc1_icu_mv$mech_ven)
class(std_cc1_icu_mv$icu)
std_cc1_icu_mv <- std_cc1_icu_mv %>% mutate(mech_ven = if_else(is.na(mech_ven), "No", mech_ven))
std_cc1_icu_mv <- std_cc1_icu_mv %>% mutate(icu = if_else(is.na(icu), "No", icu))

setwd('C:/Data/Shared Folder/People/Rubyath/Data/Working files/Intermediate data/Period 1')
fwrite(std_cc1_icu_mv, file = "std_cc1_icu_mv.csv", sep = ";", quote = TRUE, bom = TRUE)

#Pandemic

#Joining icu and mv with study population
icu_mv_cc2 <- icu_mv_cc2 %>% mutate(inskrtidpunkt = as.Date(inskrtidpunkt), utskrtidpunkt = as.Date(utskrtidpunkt))
std_cc2_icu_mv <- std_cc2_virus %>% left_join(icu_mv_cc2, join_by(lopnr.x, first_admission_datetime <= inskrtidpunkt, last_discharge_datetime >= inskrtidpunkt))
#duplicate slv_table_ids mean that one episode has multiple ICU admissions
#just to confirm #slv_duplicates <- get_dupes(std_cc2_icu_mv, slv_table_id) 
std_cc2_icu_mv <- std_cc2_icu_mv %>% group_by(slv_table_id) %>% arrange(desc(mech_ven == "Yes"), inskrtidpunkt, .by_group = TRUE) %>% slice(1) %>% ungroup()

#Reducing dimensionality
#colnames(std_cc2_icu_mv)
std_cc2_icu_mv <- std_cc2_icu_mv %>% select(-c("lopnr.y","avd", "vardtyp"))
class(std_cc2_icu_mv$mech_ven)
class(std_cc2_icu_mv$icu)
std_cc2_icu_mv <- std_cc2_icu_mv %>% mutate(mech_ven = if_else(is.na(mech_ven), "No", mech_ven))
std_cc2_icu_mv <- std_cc2_icu_mv %>% mutate(icu = if_else(is.na(icu), "No", icu))

setwd('C:/Data/Shared Folder/People/Rubyath/Data/Working files/Intermediate data/Period 2')
fwrite(std_cc2_icu_mv, file = "std_cc2_icu_mv.csv", sep = ";", quote = TRUE, bom = TRUE)

#Post-pandemic

#Joining icu and mv with study population
icu_mv_cc3 <- icu_mv_cc3 %>% mutate(inskrtidpunkt = as.Date(inskrtidpunkt), utskrtidpunkt = as.Date(utskrtidpunkt))
std_cc3_icu_mv <- std_cc3_virus %>% left_join(icu_mv_cc3, join_by(lopnr.x, first_admission_datetime <= inskrtidpunkt, last_discharge_datetime >= inskrtidpunkt))
#duplicate slv_table_ids mean that one episode has multiple ICU admissions
#just to confirm #slv_duplicates <- get_dupes(std_cc3_icu_mv, slv_table_id) 
std_cc3_icu_mv <- std_cc3_icu_mv %>% group_by(slv_table_id) %>% arrange(desc(mech_ven == "Yes"), inskrtidpunkt, .by_group = TRUE) %>% slice(1) %>% ungroup()

#Reducing dimensionality
colnames(std_cc3_icu_mv)
std_cc3_icu_mv <- std_cc3_icu_mv %>% select(-c("lopnr.y","avd", "vardtyp"))
class(std_cc3_icu_mv$mech_ven)
class(std_cc3_icu_mv$icu)
std_cc3_icu_mv <- std_cc3_icu_mv %>% mutate(mech_ven = if_else(is.na(mech_ven), "No", mech_ven))
std_cc3_icu_mv <- std_cc3_icu_mv %>% mutate(icu = if_else(is.na(icu), "No", icu))

setwd('C:/Data/Shared Folder/People/Rubyath/Data/Working files/Intermediate data/Period 3')
fwrite(std_cc3_icu_mv, file = "std_cc3_icu_mv.csv", sep = ";", quote = TRUE, bom = TRUE)

###---###
#High flow nasal cannula
rm(list = ls())
setwd('C:/Data/Shared Folder/People/Rubyath/Data/Master files/Clinical outcome')
oxy_sup <- read.csv("ari cases_high flow oxygen.csv")
any_missing <- sum(is.na(oxy_sup$table_source))
problem_columns <- sapply(oxy_sup, function(col) {if(is.character(col) || is.factor(col)) {any(!validUTF8(as.character(col)), na.rm = TRUE)} else {FALSE}})
oxy_sup$action_name <- iconv(oxy_sup$action_name, from = "latin1", to = "UTF-8")
oxy_sup_slv <- oxy_sup %>% filter(table_source == "slv")
oxy_sup_ovr <- oxy_sup %>% filter(table_source == "ovr")
#deduplicating as HFNC is yes/no for the study
#hfnc_slv_dup <- get_dupes(oxy_sup_slv, slv_table_id)
oxy_sup_slv <- oxy_sup_slv %>% distinct(slv_table_id, .keep_all = TRUE)
oxy_sup_ovr <- oxy_sup_ovr %>% distinct(ovr_table_id, .keep_all = TRUE)
#setwd('C:/Data/Shared Folder/People/Rubyath/Data/Working files/Intermediate data/Clinical outcome')
##fwrite(oxy_sup_slv, file = "oxy_sup_slv.csv", sep = ";", quote = TRUE, bom = TRUE)
##fwrite(oxy_sup_ovr, file = "oxy_sup_ovr.csv", sep = ";", quote = TRUE, bom = TRUE)

#Pre-pandemic

setwd('C:/Data/Shared Folder/People/Rubyath/Data/Working files/Intermediate data/Period 1')
std_cc1_icu_mv <- read_csv2("std_cc1_icu_mv.csv")
std_cc1_icu_mv_hfnc <- left_join(std_cc1_icu_mv, oxy_sup_slv, by = "slv_table_id")
std_cc1_icu_mv_hfnc <- std_cc1_icu_mv_hfnc %>% select(-c("lopnr", "action_code_pos", "ovr_table_id", "table_source"))
std_cc1_icu_mv_hfnc <- rename(std_cc1_icu_mv_hfnc, hfnc = action_name)
std_cc1_icu_mv_hfnc <- std_cc1_icu_mv_hfnc %>% mutate(hfnc = case_when(!is.na(hfnc) ~ "Yes", TRUE ~ "No"))
#nothing matched from ovr table
#oxy_sup_ovr <- oxy_sup_ovr %>% mutate(action_date = as.Date(action_date)) 
#std_cc1_icu_mv_hfnc_ovr <- std_cc1_icu_mv_hfnc %>% left_join(oxy_sup_ovr, join_by(lopnr.x == lopnr, first_admission_datetime <= action_date, last_discharge_datetime >= action_date))
#rm(std_cc1_icu_mv_hfnc_ovr)
fwrite(std_cc1_icu_mv_hfnc, file = "std_cc1_icu_mv_hfnc.csv", sep = ";", quote = TRUE, bom = TRUE)

#Pandemic

setwd('C:/Data/Shared Folder/People/Rubyath/Data/Working files/Intermediate data/Period 2')
std_cc2_icu_mv <- read_csv2("std_cc2_icu_mv.csv")
std_cc2_icu_mv_hfnc <- left_join(std_cc2_icu_mv, oxy_sup_slv, by = "slv_table_id")
std_cc2_icu_mv_hfnc <- std_cc2_icu_mv_hfnc %>% select(-c("lopnr", "action_code_pos", "ovr_table_id", "table_source"))
std_cc2_icu_mv_hfnc <- rename(std_cc2_icu_mv_hfnc, hfnc = action_name)
std_cc2_icu_mv_hfnc <- std_cc2_icu_mv_hfnc %>% mutate(hfnc = case_when(!is.na(hfnc) ~ "Yes", TRUE ~ "No"))
#match from ovr table
oxy_sup_ovr <- oxy_sup_ovr %>% mutate(action_date = as.Date(action_date)) 
std_cc2_icu_mv_hfnc_ovr <- std_cc2_icu_mv_hfnc %>% left_join(oxy_sup_ovr, join_by(lopnr.x == lopnr, first_admission_datetime <= action_date, last_discharge_datetime >= action_date))
#rm(std_cc2_icu_mv_hfnc_ovr)
fwrite(std_cc2_icu_mv_hfnc_ovr, file = "std_cc2_icu_mv_hfnc_ovr.csv", sep = ";", quote = TRUE, bom = TRUE)

#Post-pandemic

setwd('C:/Data/Shared Folder/People/Rubyath/Data/Working files/Intermediate data/Period 3')
std_cc3_icu_mv <- read_csv2("std_cc3_icu_mv.csv")
std_cc3_icu_mv_hfnc <- left_join(std_cc3_icu_mv, oxy_sup_slv, by = "slv_table_id")
std_cc3_icu_mv_hfnc <- std_cc3_icu_mv_hfnc %>% select(-c("lopnr", "action_code_pos", "ovr_table_id", "table_source"))
std_cc3_icu_mv_hfnc <- rename(std_cc3_icu_mv_hfnc, hfnc = action_name)
std_cc3_icu_mv_hfnc <- std_cc3_icu_mv_hfnc %>% mutate(hfnc = case_when(!is.na(hfnc) ~ "Yes", TRUE ~ "No"))
#match from ovr table
oxy_sup_ovr <- oxy_sup_ovr %>% mutate(action_date = as.Date(action_date)) 
std_cc3_icu_mv_hfnc_ovr <- std_cc3_icu_mv_hfnc %>% left_join(oxy_sup_ovr, join_by(lopnr.x == lopnr, first_admission_datetime <= action_date, last_discharge_datetime >= action_date))
#rm(std_cc3_icu_mv_hfnc_ovr)
fwrite(std_cc3_icu_mv_hfnc_ovr, file = "std_cc3_icu_mv_hfnc_ovr.csv", sep = ";", quote = TRUE, bom = TRUE)


###---######
#emigration_death
#emigration info is not useful because not complete info
rm(list=ls())
gc()
setwd('C:/Data/Shared Folder/People/Rubyath/Data/Master files/Clinical outcome')
all_mort <- read_csv2("all_death date.csv")

#Pre-pandemic

setwd('C:/Data/Shared Folder/People/Rubyath/Data/Working files/Intermediate data/Period 1')
std_cc1_icu_mv_hfnc <- read_csv2("std_cc1_icu_mv_hfnc.csv")
std_cc1_icu_mv_hfnc <- rename(std_cc1_icu_mv_hfnc, lopnr = lopnr.x)
std_cc1_co <- left_join(std_cc1_icu_mv_hfnc, all_mort, by = "lopnr")
#attributes(std_cc1_co$death_date)
#attributes(std_cc1_co$last_discharge_datetime)
#QC for data
std_cc1_co <- std_cc1_co %>% mutate(qc_diff = round(difftime(death_date, last_discharge_datetime, units = "days"), 1))
std_cc1_co <- std_cc1_co %>% select(-qc_diff)
#coding 30-days and 90-days mortality, defined from first admission to 30 and 90 days follow-up
std_cc1_co <- std_cc1_co %>% mutate(diff_mor = round(difftime(death_date, first_admission_datetime, units = "days"), 1))
std_cc1_co <- std_cc1_co %>% mutate(mort_30days = ifelse(diff_mor <= 30, "Yes", "No")) %>% mutate(mort_30days = ifelse(is.na(mort_30days), "No", mort_30days))
std_cc1_co <- std_cc1_co %>% mutate(mort_90days = ifelse(diff_mor <= 90, "Yes", "No")) %>% mutate(mort_90days = ifelse(is.na(mort_90days), "No", mort_90days))
#removing low quality mortality data
std_cc1_co <- std_cc1_co %>% filter(!lopnr %in% c(675651, 1072105, 2671432))
fwrite(std_cc1_co, file = "std_cc1_co.csv", sep = ";", quote = TRUE, bom = TRUE)

#Pandemic

setwd('C:/Data/Shared Folder/People/Rubyath/Data/Working files/Intermediate data/Period 2')
std_cc2_icu_mv_hfnc <- read_csv2("std_cc2_icu_mv_hfnc_ovr.csv")
std_cc2_icu_mv_hfnc <- rename(std_cc2_icu_mv_hfnc, lopnr = lopnr.x)
std_cc2_icu_mv_hfnc <- std_cc2_icu_mv_hfnc %>% select(-c("action_code_pos", "slv_table_id.y", "table_source"))
std_cc2_co <- left_join(std_cc2_icu_mv_hfnc, all_mort, by = "lopnr")
#attributes(std_cc2_co$death_date)
#attributes(std_cc2_co$last_discharge_datetime)
#QC for data
std_cc2_co <- std_cc2_co %>% mutate(qc_diff = round(difftime(death_date, last_discharge_datetime, units = "days"), 1))
std_cc2_co <- std_cc2_co %>% select(-qc_diff)
#coding 30-days and 90-days mortality, defined from first admission to 30 and 90 days follow-up
std_cc2_co <- std_cc2_co %>% mutate(diff_mor = round(difftime(death_date, first_admission_datetime, units = "days"), 1))
std_cc2_co <- std_cc2_co %>% mutate(mort_30days = ifelse(diff_mor <= 30, "Yes", "No")) %>% mutate(mort_30days = ifelse(is.na(mort_30days), "No", mort_30days))
std_cc2_co <- std_cc2_co %>% mutate(mort_90days = ifelse(diff_mor <= 90, "Yes", "No")) %>% mutate(mort_90days = ifelse(is.na(mort_90days), "No", mort_90days))
#removing low quality mortality data
std_cc2_co <- std_cc2_co %>% filter(!lopnr %in% c(675651, 1072105, 2671432))
fwrite(std_cc2_co, file = "std_cc2_co.csv", sep = ";", quote = TRUE, bom = TRUE)

#Post-pandemic

setwd('C:/Data/Shared Folder/People/Rubyath/Data/Working files/Intermediate data/Period 3')
std_cc3_icu_mv_hfnc <- read_csv2("std_cc3_icu_mv_hfnc_ovr.csv")
std_cc3_icu_mv_hfnc <- rename(std_cc3_icu_mv_hfnc, lopnr = lopnr.x)
std_cc3_icu_mv_hfnc <- std_cc3_icu_mv_hfnc %>% select(-c("action_code_pos", "slv_table_id.y", "table_source"))
std_cc3_co <- left_join(std_cc3_icu_mv_hfnc, all_mort, by = "lopnr")
#attributes(std_cc3_co$death_date)
#attributes(std_cc3_co$last_discharge_datetime)
#QC for data
std_cc3_co <- std_cc3_co %>% mutate(qc_diff = round(difftime(death_date, last_discharge_datetime, units = "days"), 1))
std_cc3_co <- std_cc3_co %>% select(-qc_diff)
#coding 30-days and 90-days mortality, defined from first admission to 30 and 90 days follow-up
std_cc3_co <- std_cc3_co %>% mutate(diff_mor = round(difftime(death_date, first_admission_datetime, units = "days"), 1))
std_cc3_co <- std_cc3_co %>% mutate(mort_30days = ifelse(diff_mor <= 30, "Yes", "No")) %>% mutate(mort_30days = ifelse(is.na(mort_30days), "No", mort_30days))
std_cc3_co <- std_cc3_co %>% mutate(mort_90days = ifelse(diff_mor <= 90, "Yes", "No")) %>% mutate(mort_90days = ifelse(is.na(mort_90days), "No", mort_90days))
#removing low quality mortality data
std_cc3_co <- std_cc3_co %>% filter(!lopnr %in% c(675651, 1072105, 2671432))
fwrite(std_cc3_co, file = "std_cc3_co.csv", sep = ";", quote = TRUE, bom = TRUE)

#addtional codes
#std_cc1_co <- std_cc1_co %>% select(lopnr, slv_table_id, first_admission_datetime, last_discharge_datetime, death_date, diff_mor, qc_diff, everything())
##merging ICU episodes within 24 hours #gap <- 24
#icu_mv_updated <- icu_mv_cc1 %>% arrange(lopnr.x, inskrtidpunkt) %>% group_by(lopnr.x) %>% mutate(prev_column = lag(utskrtidpunkt), is_new_group = case_when(is.na(prev_column) ~ TRUE, as.numeric(difftime(inskrtidpunkt, prev_column, units = "hours")) > gap ~ TRUE, TRUE ~ FALSE), episode_id = cumsum(is_new_group)) %>% group_by(lopnr.x, episode_id) %>% arrange(desc(tolower(mech_ven) == "yes"), inskrtidpunkt, .by_group = TRUE) %>% summarise(inDate = min(inskrtidpunkt), outDate = max(utskrtidpunkt), rows_merged_count = n(), across(-c(inskrtidpunkt, utskrtidpunkt, prev_column, is_new_group), dplyr::first), .groups = "drop")
