#comorbidity file preparation

rm(list=ls())
gc()
pacman::p_load(dplyr, readr, lubridate, data.table, stringr, janitor)

setwd('C:/Data/Shared Folder/People/Rubyath/Data/Working files/Intermediate data/Period 1')
std_cc1_co <- read_csv2("std_cc1_co.csv")
setwd('C:/Data/Shared Folder/People/Rubyath/Data/Working files/Intermediate data/Period 2')
std_cc2_co <- read_csv2("std_cc2_co.csv")
setwd('C:/Data/Shared Folder/People/Rubyath/Data/Working files/Intermediate data/Period 3')
std_cc3_co <- read_csv2("std_cc3_co.csv")

setwd('C:/Data/Shared Folder/People/Rubyath/Data/Master files/Comorbidity data')
comor_opt_12_13 <- read.csv("ari cases_comorbidity_outpat_2012-13.csv")
problem_columns <- sapply(comor_opt_12_13, function(col) {if(is.character(col) || is.factor(col)) {any(!validUTF8(as.character(col)), na.rm = TRUE)} else {FALSE}})
problem_columns
comor_opt_12_13$diag_code <- iconv(comor_opt_12_13$diag_code, from = "latin1", to = "UTF-8")

class(comor_opt_12_13$lopnr)
class(std_cc1_co$lopnr)
comor_opt_12_13_cc1 <- comor_opt_12_13 %>% mutate(lopnr = as.numeric(lopnr)) %>% semi_join(std_cc1_co, by = c("lopnr" = "lopnr"))
fwrite(comor_opt_12_13_cc1, file = "comor_opt_12_13_cc1.csv", sep = ";", quote = TRUE, bom = TRUE)

class(std_cc2_co$lopnr)
comor_opt_12_13_cc2 <- comor_opt_12_13 %>% mutate(lopnr = as.numeric(lopnr)) %>% semi_join(std_cc2_co, by = c("lopnr" = "lopnr"))
fwrite(comor_opt_12_13_cc2, file = "comor_opt_12_13_cc2.csv", sep = ";", quote = TRUE, bom = TRUE)

class(std_cc3_co$lopnr)
comor_opt_12_13_cc3 <- comor_opt_12_13 %>% mutate(lopnr = as.numeric(lopnr)) %>% semi_join(std_cc3_co, by = c("lopnr" = "lopnr"))
fwrite(comor_opt_12_13_cc3, file = "comor_opt_12_13_cc3.csv", sep = ";", quote = TRUE, bom = TRUE)
rm(comor_opt_12_13)
rm(comor_opt_12_13_cc1)
rm(comor_opt_12_13_cc2)
rm(comor_opt_12_13_cc3)

comor_opt_14_15 <- read.csv("ari cases_comorbidity_outpat_2014-15.csv")
problem_columns <- sapply(comor_opt_14_15, function(col) {if(is.character(col) || is.factor(col)) {any(!validUTF8(as.character(col)), na.rm = TRUE)} else {FALSE}})
problem_columns
comor_opt_14_15$diag_code <- iconv(comor_opt_14_15$diag_code, from = "latin1", to = "UTF-8")

class(comor_opt_14_15$lopnr)
class(std_cc1_co$lopnr)
comor_opt_14_15_cc1 <- comor_opt_14_15 %>% mutate(lopnr = as.numeric(lopnr)) %>% semi_join(std_cc1_co, by = c("lopnr" = "lopnr"))
fwrite(comor_opt_14_15_cc1, file = "comor_opt_14_15_cc1.csv", sep = ";", quote = TRUE, bom = TRUE)

class(std_cc2_co$lopnr)
comor_opt_14_15_cc2 <- comor_opt_14_15 %>% mutate(lopnr = as.numeric(lopnr)) %>% semi_join(std_cc2_co, by = c("lopnr" = "lopnr"))
fwrite(comor_opt_14_15_cc2, file = "comor_opt_14_15_cc2.csv", sep = ";", quote = TRUE, bom = TRUE)

class(std_cc3_co$lopnr)
comor_opt_14_15_cc3 <- comor_opt_14_15 %>% mutate(lopnr = as.numeric(lopnr)) %>% semi_join(std_cc3_co, by = c("lopnr" = "lopnr"))
fwrite(comor_opt_14_15_cc3, file = "comor_opt_14_15_cc3.csv", sep = ";", quote = TRUE, bom = TRUE)
rm(comor_opt_14_15)
rm(comor_opt_14_15_cc1)
rm(comor_opt_14_15_cc2)
rm(comor_opt_14_15_cc3)

comor_opt_16_17 <- read.csv("ari cases_comorbidity_outpat_2016-17.csv")
problem_columns <- sapply(comor_opt_16_17, function(col) {if(is.character(col) || is.factor(col)) {any(!validUTF8(as.character(col)), na.rm = TRUE)} else {FALSE}})
problem_columns
comor_opt_16_17$diag_code <- iconv(comor_opt_16_17$diag_code, from = "latin1", to = "UTF-8")

class(comor_opt_16_17$lopnr)
class(std_cc1_co$lopnr)
comor_opt_16_17_cc1 <- comor_opt_16_17 %>% mutate(lopnr = as.numeric(lopnr)) %>% semi_join(std_cc1_co, by = c("lopnr" = "lopnr"))
fwrite(comor_opt_16_17_cc1, file = "comor_opt_16_17_cc1.csv", sep = ";", quote = TRUE, bom = TRUE)

class(std_cc2_co$lopnr)
comor_opt_16_17_cc2 <- comor_opt_16_17 %>% mutate(lopnr = as.numeric(lopnr)) %>% semi_join(std_cc2_co, by = c("lopnr" = "lopnr"))
fwrite(comor_opt_16_17_cc2, file = "comor_opt_16_17_cc2.csv", sep = ";", quote = TRUE, bom = TRUE)

class(std_cc3_co$lopnr)
comor_opt_16_17_cc3 <- comor_opt_16_17 %>% mutate(lopnr = as.numeric(lopnr)) %>% semi_join(std_cc3_co, by = c("lopnr" = "lopnr"))
fwrite(comor_opt_16_17_cc3, file = "comor_opt_16_17_cc3.csv", sep = ";", quote = TRUE, bom = TRUE)
rm(comor_opt_16_17)
rm(comor_opt_16_17_cc1)
rm(comor_opt_16_17_cc2)
rm(comor_opt_16_17_cc3)

comor_opt_18_19 <- read.csv("ari cases_comorbidity_outpat_2018-19.csv")
problem_columns <- sapply(comor_opt_18_19, function(col) {if(is.character(col) || is.factor(col)) {any(!validUTF8(as.character(col)), na.rm = TRUE)} else {FALSE}})
problem_columns
comor_opt_18_19$diag_code <- iconv(comor_opt_18_19$diag_code, from = "latin1", to = "UTF-8")

class(comor_opt_18_19$lopnr)
class(std_cc1_co$lopnr)
comor_opt_18_19_cc1 <- comor_opt_18_19 %>% mutate(lopnr = as.numeric(lopnr)) %>% semi_join(std_cc1_co, by = c("lopnr" = "lopnr"))
fwrite(comor_opt_18_19_cc1, file = "comor_opt_18_19_cc1.csv", sep = ";", quote = TRUE, bom = TRUE)

class(std_cc2_co$lopnr)
comor_opt_18_19_cc2 <- comor_opt_18_19 %>% mutate(lopnr = as.numeric(lopnr)) %>% semi_join(std_cc2_co, by = c("lopnr" = "lopnr"))
fwrite(comor_opt_18_19_cc2, file = "comor_opt_18_19_cc2.csv", sep = ";", quote = TRUE, bom = TRUE)

class(std_cc3_co$lopnr)
comor_opt_18_19_cc3 <- comor_opt_18_19 %>% mutate(lopnr = as.numeric(lopnr)) %>% semi_join(std_cc3_co, by = c("lopnr" = "lopnr"))
fwrite(comor_opt_18_19_cc3, file = "comor_opt_18_19_cc3.csv", sep = ";", quote = TRUE, bom = TRUE)
rm(comor_opt_18_19)
rm(comor_opt_18_19_cc1)
rm(comor_opt_18_19_cc2)
rm(comor_opt_18_19_cc3)

comor_opt_20_21 <- read.csv("ari cases_comorbidity_outpat_2020-21.csv")
problem_columns <- sapply(comor_opt_20_21, function(col) {if(is.character(col) || is.factor(col)) {any(!validUTF8(as.character(col)), na.rm = TRUE)} else {FALSE}})
problem_columns
comor_opt_20_21$diag_code <- iconv(comor_opt_20_21$diag_code, from = "latin1", to = "UTF-8")

class(comor_opt_20_21$lopnr)
class(std_cc1_co$lopnr)
comor_opt_20_21_cc1 <- comor_opt_20_21 %>% mutate(lopnr = as.numeric(lopnr)) %>% semi_join(std_cc1_co, by = c("lopnr" = "lopnr"))
fwrite(comor_opt_20_21_cc1, file = "comor_opt_20_21_cc1.csv", sep = ";", quote = TRUE, bom = TRUE)

class(std_cc2_co$lopnr)
comor_opt_20_21_cc2 <- comor_opt_20_21 %>% mutate(lopnr = as.numeric(lopnr)) %>% semi_join(std_cc2_co, by = c("lopnr" = "lopnr"))
fwrite(comor_opt_20_21_cc2, file = "comor_opt_20_21_cc2.csv", sep = ";", quote = TRUE, bom = TRUE)

class(std_cc3_co$lopnr)
comor_opt_20_21_cc3 <- comor_opt_20_21 %>% mutate(lopnr = as.numeric(lopnr)) %>% semi_join(std_cc3_co, by = c("lopnr" = "lopnr"))
fwrite(comor_opt_20_21_cc3, file = "comor_opt_20_21_cc3.csv", sep = ";", quote = TRUE, bom = TRUE)
rm(comor_opt_20_21)
rm(comor_opt_20_21_cc1)
rm(comor_opt_20_21_cc2)
rm(comor_opt_20_21_cc3)

comor_opt_22_23 <- read.csv("ari cases_comorbidity_outpat_2022-23.csv")
problem_columns <- sapply(comor_opt_22_23, function(col) {if(is.character(col) || is.factor(col)) {any(!validUTF8(as.character(col)), na.rm = TRUE)} else {FALSE}})
problem_columns
comor_opt_22_23$diag_code <- iconv(comor_opt_22_23$diag_code, from = "latin1", to = "UTF-8")

class(comor_opt_22_23$lopnr)
class(std_cc1_co$lopnr)
comor_opt_22_23_cc1 <- comor_opt_22_23 %>% mutate(lopnr = as.numeric(lopnr)) %>% semi_join(std_cc1_co, by = c("lopnr" = "lopnr"))
fwrite(comor_opt_22_23_cc1, file = "comor_opt_22_23_cc1.csv", sep = ";", quote = TRUE, bom = TRUE)

class(std_cc2_co$lopnr)
comor_opt_22_23_cc2 <- comor_opt_22_23 %>% mutate(lopnr = as.numeric(lopnr)) %>% semi_join(std_cc2_co, by = c("lopnr" = "lopnr"))
fwrite(comor_opt_22_23_cc2, file = "comor_opt_22_23_cc2.csv", sep = ";", quote = TRUE, bom = TRUE)

class(std_cc3_co$lopnr)
comor_opt_22_23_cc3 <- comor_opt_22_23 %>% mutate(lopnr = as.numeric(lopnr)) %>% semi_join(std_cc3_co, by = c("lopnr" = "lopnr"))
fwrite(comor_opt_22_23_cc3, file = "comor_opt_22_23_cc3.csv", sep = ";", quote = TRUE, bom = TRUE)
rm(comor_opt_22_23)
rm(comor_opt_22_23_cc1)
rm(comor_opt_22_23_cc2)
rm(comor_opt_22_23_cc3)


comor_opt_24 <- read.csv("ari cases_comorbidity_outpat_2024.csv")
problem_columns <- sapply(comor_opt_24, function(col) {if(is.character(col) || is.factor(col)) {any(!validUTF8(as.character(col)), na.rm = TRUE)} else {FALSE}})
problem_columns
comor_opt_24$diag_code <- iconv(comor_opt_24$diag_code, from = "latin1", to = "UTF-8")

class(comor_opt_24$lopnr)
class(std_cc1_co$lopnr)
comor_opt_24_cc1 <- comor_opt_24 %>% mutate(lopnr = as.numeric(lopnr)) %>% semi_join(std_cc1_co, by = c("lopnr" = "lopnr"))
fwrite(comor_opt_24_cc1, file = "comor_opt_24_cc1.csv", sep = ";", quote = TRUE, bom = TRUE)

class(std_cc2_co$lopnr)
comor_opt_24_cc2 <- comor_opt_24 %>% mutate(lopnr = as.numeric(lopnr)) %>% semi_join(std_cc2_co, by = c("lopnr" = "lopnr"))
fwrite(comor_opt_24_cc2, file = "comor_opt_24_cc2.csv", sep = ";", quote = TRUE, bom = TRUE)

class(std_cc3_co$lopnr)
comor_opt_24_cc3 <- comor_opt_24 %>% mutate(lopnr = as.numeric(lopnr)) %>% semi_join(std_cc3_co, by = c("lopnr" = "lopnr"))
fwrite(comor_opt_24_cc3, file = "comor_opt_24_cc3.csv", sep = ";", quote = TRUE, bom = TRUE)
rm(comor_opt_24)
rm(comor_opt_24_cc1)
rm(comor_opt_24_cc2)
rm(comor_opt_24_cc3)

rm(list = ls())
setwd('C:/Data/Shared Folder/People/Rubyath/Data/Master files/Comorbidity data')
comor_opt_12_13 <- read_csv2("comor_opt_12_13_cc1.csv")
comor_opt_14_15 <- read_csv2("comor_opt_14_15_cc1.csv")
comor_opt_16_17 <- read_csv2("comor_opt_16_17_cc1.csv")
comor_opt_18_19 <- read_csv2("comor_opt_18_19_cc1.csv")
comor_opt_20_21 <- read_csv2("comor_opt_20_21_cc1.csv")
comor_opt_22_23 <- read_csv2("comor_opt_22_23_cc1.csv")
comor_opt_24 <- read_csv2("comor_opt_24_cc1.csv")
comor_opt_cc1 <- bind_rows(comor_opt_12_13, comor_opt_14_15, comor_opt_16_17, comor_opt_18_19, comor_opt_20_21, comor_opt_22_23, comor_opt_24)
fwrite(comor_opt_cc1, file = "comor_opt_cc1.csv", sep = ";", quote = TRUE, bom = TRUE)

rm(list = ls())
comor_opt_12_13 <- read_csv2("comor_opt_12_13_cc2.csv")
comor_opt_14_15 <- read_csv2("comor_opt_14_15_cc2.csv")
comor_opt_16_17 <- read_csv2("comor_opt_16_17_cc2.csv")
comor_opt_18_19 <- read_csv2("comor_opt_18_19_cc2.csv")
comor_opt_20_21 <- read_csv2("comor_opt_20_21_cc2.csv")
comor_opt_22_23 <- read_csv2("comor_opt_22_23_cc2.csv")
comor_opt_24 <- read_csv2("comor_opt_24_cc2.csv")
comor_opt_cc2 <- bind_rows(comor_opt_12_13, comor_opt_14_15, comor_opt_16_17, comor_opt_18_19, comor_opt_20_21, comor_opt_22_23, comor_opt_24)
fwrite(comor_opt_cc2, file = "comor_opt_cc2.csv", sep = ";", quote = TRUE, bom = TRUE)

rm(list = ls())
comor_opt_12_13 <- read_csv2("comor_opt_12_13_cc3.csv")
comor_opt_14_15 <- read_csv2("comor_opt_14_15_cc3.csv")
comor_opt_16_17 <- read_csv2("comor_opt_16_17_cc3.csv")
comor_opt_18_19 <- read_csv2("comor_opt_18_19_cc3.csv")
comor_opt_20_21 <- read_csv2("comor_opt_20_21_cc3.csv")
comor_opt_22_23 <- read_csv2("comor_opt_22_23_cc3.csv")
comor_opt_24 <- read_csv2("comor_opt_24_cc3.csv")
comor_opt_cc3 <- bind_rows(comor_opt_12_13, comor_opt_14_15, comor_opt_16_17, comor_opt_18_19, comor_opt_20_21, comor_opt_22_23, comor_opt_24)
fwrite(comor_opt_cc3, file = "comor_opt_cc3.csv", sep = ";", quote = TRUE, bom = TRUE)

####-----####
rm(list=ls())

setwd('C:/Data/Shared Folder/People/Rubyath/Data/Working files/Intermediate data/Period 1')
std_cc1_co <- read_csv2("std_cc1_co.csv")
setwd('C:/Data/Shared Folder/People/Rubyath/Data/Working files/Intermediate data/Period 2')
std_cc2_co <- read_csv2("std_cc2_co.csv")
setwd('C:/Data/Shared Folder/People/Rubyath/Data/Working files/Intermediate data/Period 3')
std_cc3_co <- read_csv2("std_cc3_co.csv")

setwd('C:/Data/Shared Folder/People/Rubyath/Data/Master files/Comorbidity data')
comor_ipt_17_20 <- read_csv2("diag_period1_pre.csv")
comor_ipt_20_22 <- read_csv2("diag_period2_pand.csv")
comor_ipt_22_24 <- read_csv2("diag_period3_post.csv")
comor_ipt_12_16 <- read.csv("ari cases_comorbidity_inpat_2012-16.csv")
problem_columns <- sapply(comor_ipt_12_16, function(col) {if(is.character(col) || is.factor(col)) {any(!validUTF8(as.character(col)), na.rm = TRUE)} else {FALSE}})
problem_columns
comor_ipt_12_16$diag_code <- iconv(comor_ipt_12_16$diag_code, from = "latin1", to = "UTF-8")
fwrite(comor_ipt_12_16, file = "comor_ipt_12_16.csv", sep = ";", quote = TRUE, bom = TRUE)
comor_ipt_12_16 <- read_csv2("comor_ipt_12_16.csv")
comor_ipt <- bind_rows(comor_ipt_12_16, comor_ipt_17_20, comor_ipt_20_22, comor_ipt_22_24)
rm(comor_ipt_12_16)
rm(comor_ipt_17_20)
rm(comor_ipt_20_22)
rm(comor_ipt_22_24)

class(comor_ipt$lopnr)
comor_ipt_cc1 <- comor_ipt %>% semi_join(std_cc1_co, by = c("lopnr" = "lopnr"))
comor_ipt_cc2 <- comor_ipt %>% semi_join(std_cc2_co, by = c("lopnr" = "lopnr"))
comor_ipt_cc3 <- comor_ipt %>% semi_join(std_cc3_co, by = c("lopnr" = "lopnr"))
fwrite(comor_ipt_cc1, file = "comor_ipt_cc1.csv", sep = ";", quote = TRUE, bom = TRUE)
fwrite(comor_ipt_cc2, file = "comor_ipt_cc2.csv", sep = ";", quote = TRUE, bom = TRUE)
fwrite(comor_ipt_cc3, file = "comor_ipt_cc3.csv", sep = ";", quote = TRUE, bom = TRUE)

#comorbidity transformation

rm(list= ls())
pacman::p_load(dplyr, readr, lubridate, data.table, stringr, janitor, comorbidity)
setwd('C:/Data/Shared Folder/People/Rubyath/Data/Working files/Intermediate data/Comorbidity')
comor_ipt_cc1 <- read_csv2("comor_ipt_cc1.csv", col_types = cols(slv_table_id = col_skip()))
comor_ipt_cc2 <- read_csv2("comor_ipt_cc2.csv", col_types = cols(slv_table_id = col_skip()))
comor_ipt_cc3 <- read_csv2("comor_ipt_cc3.csv", col_types = cols(slv_table_id = col_skip()))
comor_opt_cc1 <- read_csv2("comor_opt_cc1.csv", col_types = cols(ovr_table_id = col_skip()))
comor_opt_cc2 <- read_csv2("comor_opt_cc2.csv", col_types = cols(ovr_table_id = col_skip()))
comor_opt_cc3 <- read_csv2("comor_opt_cc3.csv", col_types = cols(ovr_table_id = col_skip()))
all_comor <- list(comor_ipt_cc1, comor_ipt_cc2, comor_ipt_cc3, comor_opt_cc1, comor_opt_cc2, comor_opt_cc3)
lapply(all_comor, str)

comor_cc1 <- bind_rows(comor_ipt_cc1, comor_opt_cc1)
comor_cc2 <- bind_rows(comor_ipt_cc2, comor_opt_cc2)
comor_cc3 <- bind_rows(comor_ipt_cc3, comor_opt_cc3)
all_comor <- c("comor_ipt_cc1", "comor_ipt_cc2", "comor_ipt_cc3", "comor_opt_cc1", "comor_opt_cc2", "comor_opt_cc3")
rm(list = all_comor)

comor_cc1 <- comor_cc1 %>% mutate(diag_code = word(diag_code, 1)) %>% filter(str_detect(diag_code, "^[A-Z][0-9]+"))
comor_cc2 <- comor_cc2 %>% mutate(diag_code = word(diag_code, 1)) %>% filter(str_detect(diag_code, "^[A-Z][0-9]+"))
comor_cc3 <- comor_cc3 %>% mutate(diag_code = word(diag_code, 1)) %>% filter(str_detect(diag_code, "^[A-Z][0-9]+"))

#Pre-pandemic

setwd('C:/Data/Shared Folder/People/Rubyath/Data/Working files/Intermediate data/Period 1')
std_cc1_co <- read_csv2("std_cc1_co.csv")
std_cc1_comor <- std_cc1_co %>% mutate(lookback_date = first_admission_datetime - years(3)) %>% 
  inner_join(comor_cc1, join_by(lopnr, first_admission_datetime > diagnosis_date, lookback_date <= diagnosis_date))
all(std_cc1_comor$first_admission_datetime > std_cc1_comor$diagnosis_date.y, na.rm = TRUE)
std_cc1_comor <- std_cc1_comor %>% rename(icd_code = diag_code.y) %>% mutate(icd_code = as.character(icd_code))

#calculation of CCI
cci_cc1 <- comorbidity(x = std_cc1_comor, id = "slv_table_id", code = "icd_code", map = "charlson_icd10_se", assign0 = FALSE)
cci_cc1$cci_score <- score(x = cci_cc1, weights = "quan", assign0 = TRUE)
std_cc1_co_cc1 <- std_cc1_co %>% left_join(cci_cc1, by = "slv_table_id")
checking_na <- std_cc1_co_cc1 %>% filter(is.na(cci_score))
std_cc1_co_cc1 <- std_cc1_co_cc1 %>% mutate(cci_score = coalesce(cci_score, 0))
fwrite(std_cc1_co_cc1, file = "std_cc1_co_cci.csv", sep = ";", quote = TRUE, bom = TRUE)

#Pandemic

setwd('C:/Data/Shared Folder/People/Rubyath/Data/Working files/Intermediate data/Period 2')
std_cc2_co <- read_csv2("std_cc2_co.csv")
std_cc2_comor <- std_cc2_co %>% mutate(lookback_date = first_admission_datetime - years(3)) %>% 
  inner_join(comor_cc2, join_by(lopnr, first_admission_datetime > diagnosis_date, lookback_date <= diagnosis_date))
all(std_cc2_comor$first_admission_datetime > std_cc2_comor$diagnosis_date.y, na.rm = TRUE)
std_cc2_comor <- std_cc2_comor %>% rename(icd_code = diag_code.y) %>% mutate(icd_code = as.character(icd_code))

#calculation of CCI
cci_cc2 <- comorbidity(x = std_cc2_comor, id = "slv_table_id.x", code = "icd_code", map = "charlson_icd10_se", assign0 = FALSE)
cci_cc2$cci_score <- score(x = cci_cc2, weights = "quan", assign0 = TRUE)
std_cc2_co_cc2 <- std_cc2_co %>% left_join(cci_cc2, by = "slv_table_id.x")
checking_na <- std_cc2_co_cc2 %>% filter(is.na(cci_score))
std_cc2_co_cc2 <- std_cc2_co_cc2 %>% mutate(cci_score = coalesce(cci_score, 0))
fwrite(std_cc2_co_cc2, file = "std_cc2_co_cci.csv", sep = ";", quote = TRUE, bom = TRUE)

#Post-pandemic
setwd('C:/Data/Shared Folder/People/Rubyath/Data/Working files/Intermediate data/Period 3')
std_cc3_co <- read_csv2("std_cc3_co.csv")
std_cc3_comor <- std_cc3_co %>% mutate(lookback_date = first_admission_datetime - years(3)) %>% 
  inner_join(comor_cc3, join_by(lopnr, first_admission_datetime > diagnosis_date, lookback_date <= diagnosis_date))
all(std_cc3_comor$first_admission_datetime > std_cc3_comor$diagnosis_date.y, na.rm = TRUE)
std_cc3_comor <- std_cc3_comor %>% rename(icd_code = diag_code.y) %>% mutate(icd_code = as.character(icd_code))

#calculation of CCI
cci_cc3 <- comorbidity(x = std_cc3_comor, id = "slv_table_id.x", code = "icd_code", map = "charlson_icd10_se", assign0 = FALSE)
cci_cc3$cci_score <- score(x = cci_cc3, weights = "quan", assign0 = TRUE)
std_cc3_co_cc3 <- std_cc3_co %>% left_join(cci_cc3, by = "slv_table_id.x")
checking_na <- std_cc3_co_cc3 %>% filter(is.na(cci_score))
std_cc3_co_cc3 <- std_cc3_co_cc3 %>% mutate(cci_score = coalesce(cci_score, 0))
fwrite(std_cc3_co_cc3, file = "std_cc3_co_cci.csv", sep = ";", quote = TRUE, bom = TRUE)
