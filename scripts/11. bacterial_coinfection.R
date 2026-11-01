#bacterial coinfection, if positive for any of the following

rm(list=ls())
gc()
pacman::p_load(dplyr, tidyr, readr, lubridate, data.table, stringr, janitor)

setwd('C:/Data/Shared Folder/People/Rubyath/Data/Working files/Intermediate data/Period 1')
cc1_microlab_scedemo <- read_csv2("cc1_microlab_scedemo.csv")
std_cc1_co_cci <- read_csv2("std_cc1_co_cci.csv")
setwd('C:/Data/Shared Folder/People/Rubyath/Data/Working files/Intermediate data/Period 2')
cc2_microlab_scedemo <- read_csv2("cc2_microlab_scedemo.csv")
std_cc2_co_cci <- read_csv2("std_cc2_co_cci.csv")
setwd('C:/Data/Shared Folder/People/Rubyath/Data/Working files/Intermediate data/Period 3')
cc3_microlab_scedemo <- read_csv2("cc3_microlab_scedemo.csv")
std_cc3_co_cci <- read_csv2("std_cc3_co_cci.csv")

std_microlab_cc1 <- cc1_microlab_scedemo %>% semi_join(std_cc1_co_cci, by = c("slv_table_id" = "slv_table_id"))
std_microlab_cc2 <- cc2_microlab_scedemo %>% semi_join(std_cc2_co_cci, by = c("slv_table_id" = "slv_table_id.x"))
std_microlab_cc3 <- cc3_microlab_scedemo %>% semi_join(std_cc3_co_cci, by = c("slv_table_id" = "slv_table_id.x"))

rm_list <- c("cc1_microlab_scedemo", "cc2_microlab_scedemo", "cc3_microlab_scedemo", "std_cc1_co_cci", "std_cc2_co_cci", "std_cc3_co_cci")
rm(list = rm_list)
rm(rm_list)

#Urine antigen, organism (Legionella pneumo, Legionella species, Streptococcus pneum)
#Pre-pandemic

sort(unique(std_microlab_cc1$Examination))
urine_ag_cc1 <- std_microlab_cc1 %>% filter(Examination == "Bakterier, antigen")
urine_ag_cc1 %>% group_by(specimen) %>% tally()
sum_urine_ag <- urine_ag_cc1 %>% group_by(Analysis, organism, result) %>% tally()
urine_ag_cc1 <- urine_ag_cc1 %>% mutate(urine_coinf = case_when(result == "Positive" ~ 1, result == "Negative" ~ 0, result == "No Information" ~ 99))
urine_ag_cc1 <- urine_ag_cc1 %>% filter(urine_coinf == 1)
any(duplicated(urine_ag_cc1$slv_table_id))
dupes <- get_dupes(urine_ag_cc1, slv_table_id)
urine_ag_cc1 <- urine_ag_cc1 %>% distinct(slv_table_id, .keep_all = TRUE)
urine_ag_cc1 <- urine_ag_cc1 %>% select(slv_table_id, urine_coinf)
setwd('C:/Data/Shared Folder/People/Rubyath/Data/Working files/Intermediate data/Period 1')
fwrite(urine_ag_cc1, file = "urine_ag_cc1.csv", sep = ";", quote = TRUE, bom = TRUE)
rm(urine_ag_cc1)
rm(dupes)

#Pandemic

sort(unique(std_microlab_cc2$Examination))
urine_ag_cc2 <- std_microlab_cc2 %>% filter(Examination == "Bakterier, antigen")
urine_ag_cc2 %>% group_by(specimen) %>% tally()
sum_urine_ag <- urine_ag_cc2 %>% group_by(Analysis, organism, result) %>% tally()
urine_ag_cc2 <- urine_ag_cc2 %>% mutate(urine_coinf = case_when(result == "Positive" ~ 1, result == "Negative" ~ 0, result == "No Information" ~ 99))
urine_ag_cc2 <- urine_ag_cc2 %>% filter(urine_coinf == 1)
any(duplicated(urine_ag_cc2$slv_table_id))
dupes <- get_dupes(urine_ag_cc2, slv_table_id)
urine_ag_cc2 <- urine_ag_cc2 %>% distinct(slv_table_id, .keep_all = TRUE)
urine_ag_cc2 <- urine_ag_cc2 %>% select(slv_table_id, urine_coinf)
setwd('C:/Data/Shared Folder/People/Rubyath/Data/Working files/Intermediate data/Period 2')
fwrite(urine_ag_cc2, file = "urine_ag_cc2.csv", sep = ";", quote = TRUE, bom = TRUE)
rm(urine_ag_cc2)
rm(dupes)

#Post-pandemic

sort(unique(std_microlab_cc3$Examination))
urine_ag_cc3 <- std_microlab_cc3 %>% filter(Examination == "Bakterier, antigen")
urine_ag_cc3 %>% group_by(specimen) %>% tally()
sum_urine_ag <- urine_ag_cc3 %>% group_by(Analysis, organism, result) %>% tally()
urine_ag_cc3 <- urine_ag_cc3 %>% mutate(urine_coinf = case_when(result == "Positive" ~ 1, result == "Negative" ~ 0, result == "No Information" ~ 99))
urine_ag_cc3 <- urine_ag_cc3 %>% filter(urine_coinf == 1)
any(duplicated(urine_ag_cc3$slv_table_id))
dupes <- get_dupes(urine_ag_cc3, slv_table_id)
urine_ag_cc3 <- urine_ag_cc3 %>% distinct(slv_table_id, .keep_all = TRUE)
urine_ag_cc3 <- urine_ag_cc3 %>% select(slv_table_id, urine_coinf)
setwd('C:/Data/Shared Folder/People/Rubyath/Data/Working files/Intermediate data/Period 3')
fwrite(urine_ag_cc3, file = "urine_ag_cc3.csv", sep = ";", quote = TRUE, bom = TRUE)
rm(urine_ag_cc3)
rm(sum_urine_ag)
rm(dupes)

#Respiratory bacterial DNA assay
#Pre-pandemic

resp_dna_cc1 <- std_microlab_cc1 %>% filter(str_detect(tolower(organism), tolower("Chlamydophila|Legionella|Mycoplasma")))
sum_resp_dna_cc1 <- resp_dna_cc1 %>% group_by(Examination, Specimen1, organism) %>% tally()
rm(sum_resp_dna_cc1)
resp_dna_cc1 <- resp_dna_cc1 %>% filter(Examination == "Molekylär diagnostik") %>% filter(result == "Positive")
setwd('C:/Data/Shared Folder/People/Rubyath/Data/Working files/Intermediate data/Period 1')
fwrite(resp_dna_cc1, file = "resp_dna_cc1.csv", sep = ";", quote = TRUE, bom = TRUE)
rm(resp_dna_cc1)

#Pandemic

sort(unique(std_microlab_cc2$organism))
resp_dna_cc2 <- std_microlab_cc2 %>% filter(str_detect(tolower(organism), tolower("Chlamydophila|Legionella|Mycoplasma")))
sum_resp_dna_cc2 <- resp_dna_cc2 %>% group_by(Examination, Specimen1, organism) %>% tally()
rm(sum_resp_dna_cc2)
resp_dna_cc2 <- resp_dna_cc2 %>% filter(Examination == "Molekylär diagnostik") %>% filter(result == "Positive")
#no positive result found
rm(resp_dna_cc2)

#Post-pandemic

sort(unique(std_microlab_cc3$organism))
resp_dna_cc3 <- std_microlab_cc3 %>% filter(str_detect(tolower(organism), tolower("Chlamydophila|Legionella|Mycoplasma")))
sum_resp_dna_cc3 <- resp_dna_cc3 %>% group_by(Examination, Specimen1, organism) %>% tally()
rm(sum_resp_dna_cc3)
resp_dna_cc3 <- resp_dna_cc3 %>% filter(Examination == "Molekylär diagnostik") %>% filter(result == "Positive")
any(duplicated(resp_dna_cc3$slv_table_id))
dupes <- get_dupes(resp_dna_cc3, slv_table_id)
rm(dupes)
resp_dna_cc3 <- resp_dna_cc3 %>% distinct(slv_table_id, .keep_all = TRUE)
setwd('C:/Data/Shared Folder/People/Rubyath/Data/Working files/Intermediate data/Period 3')
fwrite(resp_dna_cc3, file = "resp_dna_cc3.csv", sep = ";", quote = TRUE, bom = TRUE)
rm(resp_dna_cc3)

##LRT culture
#Pre-pandemic

empty_specimen <- std_microlab_cc1 %>% filter(is.na(specimen))
empty_specimen <- empty_specimen %>% group_by(Examination, Specimen1) %>% tally()
std_microlab_cc1$specimen[str_detect(tolower(std_microlab_cc1$Specimen1), tolower("^Isolat, bronksekret, borste 19DQ510216"))] <-  "Broncheal secretion"
std_microlab_cc1$specimen[str_detect(tolower(std_microlab_cc1$Specimen1), tolower("^Isolat, Nasopharynx 17DP500287"))] <-  "Nasopharyngeal"
std_microlab_cc1$specimen[str_detect(tolower(std_microlab_cc1$Specimen1), tolower("^Isolat, Sputum"))] <-  "Sputum"
std_microlab_cc1$specimen[str_detect(tolower(std_microlab_cc1$Specimen1), tolower("^Övrigt provmaterial, bakre svalgvägg"))] <-  "Pharyngeal swab"
std_microlab_cc1$specimen[str_detect(tolower(std_microlab_cc1$Specimen1), tolower("^Övrigt provmaterial, näsa"))] <-  "Nasal"
std_microlab_cc1$specimen[str_detect(tolower(std_microlab_cc1$Specimen1), tolower("^Övrigt provmaterial, Näsa"))] <-  "Nasal"
std_microlab_cc1$specimen[str_detect(tolower(std_microlab_cc1$Specimen1), tolower("^Övrigt provmaterial, nf"))] <-  "Nasopharyngeal"
std_microlab_cc1$specimen[str_detect(tolower(std_microlab_cc1$Specimen1), tolower("^Övrigt provmaterial, nph"))] <-  "Nasopharyngeal"
std_microlab_cc1$specimen[str_detect(tolower(std_microlab_cc1$Specimen1), tolower("^Övrigt provmaterial, sputum"))] <-  "Sputum"
std_microlab_cc1$specimen[str_detect(tolower(std_microlab_cc1$Specimen1), tolower("^Övrigt provmaterial, svalg"))] <-  "Throat"
std_microlab_cc1$specimen[str_detect(tolower(std_microlab_cc1$Specimen1), tolower("^Övrigt provmaterial, svalgsekret"))] <-  "Pharyngeal secretion"
std_microlab_cc1$specimen[str_detect(tolower(std_microlab_cc1$Specimen1), tolower("^Sekret, endotrachealtub"))] <-  "Endotracheal tube secretion"
std_microlab_cc1$specimen[str_detect(tolower(std_microlab_cc1$Specimen1), tolower("^Sekret, Tracheostoma"))] <-  "Tracheostoma secretion"

sort(unique(std_microlab_cc1$specimen))
lrt_specimen <- "^Bronch|^Sputum|^Trach|^Endotrach"
std_lrt_cc1 <- std_microlab_cc1 %>% filter(str_detect(specimen, regex(lrt_specimen, ignore_case = TRUE)))
sort(unique(std_lrt_cc1$Examination))
bac_culture <- "^Bakterieodl|^Mykobakterier|^Sputumodl"
std_lrt_cc1 <- std_lrt_cc1 %>% filter(str_detect(Examination, regex(bac_culture, ignore_case = TRUE))) %>% filter(is.na(result))
sum(is.na(std_lrt_cc1$organism))
sort(unique(std_lrt_cc1$organism))

std_lrt_cc1$result[str_detect(tolower(std_lrt_cc1$organism), tolower("^Aspergillus"))] <-  "Contaminant"
std_lrt_cc1$result[str_detect(tolower(std_lrt_cc1$organism), tolower("^Candida"))] <- "Contaminant"
std_lrt_cc1$result[str_detect(tolower(std_lrt_cc1$organism), tolower("^Gram negative mixed flora"))] <- "Contaminant"
std_lrt_cc1$result[str_detect(tolower(std_lrt_cc1$organism), tolower("^Yeast"))] <- "Contaminant"
std_lrt_cc1$result[str_detect(tolower(std_lrt_cc1$organism), tolower("^No growth"))] <- "Contaminant"                            
std_lrt_cc1$result[str_detect(tolower(std_lrt_cc1$organism), tolower("^Normal respiratory flora"))] <- "Contaminant"
std_lrt_cc1 <- std_lrt_cc1 %>% filter(is.na(result))

#brush > 10^3 CFU/ml, BAL > 10^4 CFU/ml, sputum/tracheal secretions > 10^5 CFU/ml
std_lrt_cc1 <- std_lrt_cc1 %>% mutate(specimen = if_else(Specimen1 == "Bronksekret, borste", "Brush", specimen))
std_lrt_cc1 <- std_lrt_cc1 %>% mutate(result = if_else(str_detect(specimen, regex("brush", ignore_case = TRUE)), if_else(str_detect(Analysis, ">[eE]3|[eE][4-9]"), "Positive", "Negative"), result))
std_lrt_cc1 <- std_lrt_cc1 %>% mutate(result = if_else(str_detect(specimen, regex("Sputum", ignore_case = TRUE)), if_else(str_detect(Analysis, ">[eE]5|[eE][6-9]"), "Positive", "Negative"), result))
std_lrt_cc1 <- std_lrt_cc1 %>% mutate(result = if_else(str_detect(specimen, regex("Trach", ignore_case = TRUE)), if_else(str_detect(Analysis, ">[eE]5|[eE][6-9]"), "Positive", "Negative"), result))
std_lrt_cc1 <- std_lrt_cc1 %>% mutate(result = if_else(str_detect(specimen, regex("Broncheal", ignore_case = TRUE)), if_else(str_detect(Analysis, ">[eE]5|[eE][6-9]"), "Positive", "Negative"), result))
std_lrt_cc1 <- std_lrt_cc1 %>% mutate(result = if_else(str_detect(specimen, regex("Bronchoalveolar", ignore_case = TRUE)), if_else(str_detect(Analysis, ">[eE]4|[eE][5-9]"), "Positive", "Negative"), result))
std_lrt_cc1 <- std_lrt_cc1 %>% filter(result == "Positive") %>% mutate(lrt_result = result) %>% select(slv_table_id, lrt_result)
setwd('C:/Data/Shared Folder/People/Rubyath/Data/Working files/Intermediate data/Period 1')
fwrite(std_lrt_cc1, file = "lrt_cc1.csv", sep = ";", quote = TRUE, bom = TRUE)

#Pandemic

empty_specimen <- std_microlab_cc2 %>% filter(is.na(specimen))
empty_specimen <- empty_specimen %>% group_by(Examination, Specimen1, result) %>% tally()
std_microlab_cc2$specimen[str_detect(tolower(std_microlab_cc2$Specimen1), tolower("^Isolat, bronksekret, borste"))] <-  "Broncheal secretion"
std_microlab_cc2$specimen[str_detect(tolower(std_microlab_cc2$Specimen1), tolower("^Isolat, Nasopharynx"))] <-  "Nasopharyngeal"
std_microlab_cc2$specimen[str_detect(tolower(std_microlab_cc2$Specimen1), tolower("^Isolat, Sputum"))] <-  "Sputum"
std_microlab_cc2$specimen[str_detect(tolower(std_microlab_cc2$Specimen1), tolower("^Övrigt provmaterial, bakre svalgvägg"))] <-  "Pharyngeal swab"
std_microlab_cc2$specimen[str_detect(tolower(std_microlab_cc2$Specimen1), tolower("^Övrigt provmaterial, näsa"))] <-  "Nasal"
std_microlab_cc2$specimen[str_detect(tolower(std_microlab_cc2$Specimen1), tolower("^Övrigt provmaterial, Näsa"))] <-  "Nasal"
std_microlab_cc2$specimen[str_detect(tolower(std_microlab_cc2$Specimen1), tolower("^Övrigt provmaterial, nf"))] <-  "Nasopharyngeal"
std_microlab_cc2$specimen[str_detect(tolower(std_microlab_cc2$Specimen1), tolower("^Övrigt provmaterial, nph"))] <-  "Nasopharyngeal"
std_microlab_cc2$specimen[str_detect(tolower(std_microlab_cc2$Specimen1), tolower("^Övrigt provmaterial, sputum"))] <-  "Sputum"
std_microlab_cc2$specimen[str_detect(tolower(std_microlab_cc2$Specimen1), tolower("^Övrigt provmaterial, svalg"))] <-  "Throat"
std_microlab_cc2$specimen[str_detect(tolower(std_microlab_cc2$Specimen1), tolower("^Övrigt provmaterial, svalgsekret"))] <-  "Pharyngeal secretion"
std_microlab_cc2$specimen[str_detect(tolower(std_microlab_cc2$Specimen1), tolower("^Sekret, endotrachealtub"))] <-  "Endotracheal tube secretion"
std_microlab_cc2$specimen[str_detect(tolower(std_microlab_cc2$Specimen1), tolower("^Sekret, Tracheostoma"))] <-  "Tracheostoma secretion"
std_microlab_cc2$specimen[str_detect(tolower(std_microlab_cc2$Specimen1), tolower("^Övrigt provmaterial, bal"))] <-  "Bronchoalveolar Lavage"
std_microlab_cc2$specimen[str_detect(tolower(std_microlab_cc2$Specimen1), tolower("^Övrigt provmaterial, Blind borste"))] <-  "Brush"
std_microlab_cc2$specimen[str_detect(tolower(std_microlab_cc2$Specimen1), tolower("^Övrigt provmaterial, Borste"))] <-  "Brush"
std_microlab_cc2$specimen[str_detect(tolower(std_microlab_cc2$Specimen1), tolower("^Övrigt provmaterial, bronk"))] <-  "Bronchi"
std_microlab_cc2$specimen[str_detect(tolower(std_microlab_cc2$Specimen1), tolower("^Övrigt provmaterial, Bronksekret, borste"))] <-  "Brush"
std_microlab_cc2$specimen[str_detect(tolower(std_microlab_cc2$Specimen1), tolower("^Övrigt provmaterial, nasofarynx"))] <-  "Nasopharyngeal"
std_microlab_cc2$specimen[str_detect(tolower(std_microlab_cc2$Specimen1), tolower("^Övrigt provmaterial, nasopharynx"))] <-  "Nasopharyngeal"
std_microlab_cc2$specimen[str_detect(tolower(std_microlab_cc2$Specimen1), tolower("^Övrigt provmaterial, trach"))] <-  "Tracheal"
std_microlab_cc2$specimen[str_detect(tolower(std_microlab_cc2$Specimen1), tolower("^Övrigt provmaterial, Trackealsekret"))] <-  "Tracheal secretion"
std_microlab_cc2$specimen[str_detect(tolower(std_microlab_cc2$Specimen1), tolower("^Övrigt provmaterial, trakealsekret"))] <-  "Tracheal secretion"

sort(unique(std_microlab_cc2$specimen))
lrt_specimen <- "^Bronch|^Sputum|^Brush|^Trach|^Endotrach"
std_lrt_cc2 <- std_microlab_cc2 %>% filter(str_detect(specimen, regex(lrt_specimen, ignore_case = TRUE)))
sort(unique(std_lrt_cc2$Examination))
bac_culture <- "^Bakterieodl|^Mykobakterier|^Sputumodl|^Legionellaodl"
std_lrt_cc2 <- std_lrt_cc2 %>% filter(str_detect(Examination, regex(bac_culture, ignore_case = TRUE))) %>% filter(is.na(result))
sum(is.na(std_lrt_cc2$organism))
sort(unique(std_lrt_cc2$organism))

std_lrt_cc2$result[str_detect(tolower(std_lrt_cc2$organism), tolower("^Aspergillus"))] <-  "Contaminant"
std_lrt_cc2$result[str_detect(tolower(std_lrt_cc2$organism), tolower("^Candida"))] <- "Contaminant"
std_lrt_cc2$result[str_detect(tolower(std_lrt_cc2$organism), tolower("^Dermatophyte"))] <- "Contaminant"
std_lrt_cc2$result[str_detect(tolower(std_lrt_cc2$organism), tolower("^Gram negative mixed flora"))] <- "Contaminant"
std_lrt_cc2$result[str_detect(tolower(std_lrt_cc2$organism), tolower("^Mold"))] <- "Contaminant"
std_lrt_cc2$result[str_detect(tolower(std_lrt_cc2$organism), tolower("^No growth"))] <- "Contaminant"                            
std_lrt_cc2$result[str_detect(tolower(std_lrt_cc2$organism), tolower("^Normal bacteria"))] <- "Contaminant"
std_lrt_cc2$result[str_detect(tolower(std_lrt_cc2$organism), tolower("^Normal oral flora"))] <- "Contaminant"
std_lrt_cc2$result[str_detect(tolower(std_lrt_cc2$organism), tolower("^Normal respiratory flora"))] <- "Contaminant"
std_lrt_cc2$result[str_detect(tolower(std_lrt_cc2$organism), tolower("^Yeast"))] <- "Contaminant"
unique(std_lrt_cc2$result)
std_lrt_cc2 <- std_lrt_cc2 %>% filter(is.na(result))

#brush > 10^3 CFU/ml, BAL > 10^4 CFU/ml, sputum/tracheal secretions > 10^5 CFU/ml
std_lrt_cc2 <- std_lrt_cc2 %>% mutate(specimen = if_else(Specimen1 == "Bronksekret, borste", "Brush", specimen))
std_lrt_cc2 <- std_lrt_cc2 %>% mutate(result = if_else(str_detect(specimen, regex("brush", ignore_case = TRUE)), if_else(str_detect(Analysis, ">[eE]3|[eE][4-9]"), "Positive", "Negative"), result))
std_lrt_cc2 <- std_lrt_cc2 %>% mutate(result = if_else(str_detect(specimen, regex("Sputum", ignore_case = TRUE)), if_else(str_detect(Analysis, ">[eE]5|[eE][6-9]"), "Positive", "Negative"), result))
std_lrt_cc2 <- std_lrt_cc2 %>% mutate(result = if_else(str_detect(specimen, regex("Trach", ignore_case = TRUE)), if_else(str_detect(Analysis, ">[eE]5|[eE][6-9]"), "Positive", "Negative"), result))
std_lrt_cc2 <- std_lrt_cc2 %>% mutate(result = if_else(str_detect(specimen, regex("Broncheal", ignore_case = TRUE)), if_else(str_detect(Analysis, ">[eE]5|[eE][6-9]"), "Positive", "Negative"), result))
std_lrt_cc2 <- std_lrt_cc2 %>% mutate(result = if_else(str_detect(specimen, regex("Bronchoalveolar", ignore_case = TRUE)), if_else(str_detect(Analysis, ">[eE]4|[eE][5-9]"), "Positive", "Negative"), result))
std_lrt_cc2 <- std_lrt_cc2 %>% filter(result == "Positive") %>% mutate(lrt_result = result) %>% select(slv_table_id, lrt_result)
setwd('C:/Data/Shared Folder/People/Rubyath/Data/Working files/Intermediate data/Period 2')
fwrite(std_lrt_cc2, file = "lrt_cc2.csv", sep = ";", quote = TRUE, bom = TRUE)

#Post-pandemic

empty_specimen <- std_microlab_cc3 %>% filter(is.na(specimen))
empty_specimen <- empty_specimen %>% group_by(Examination, Specimen1, result) %>% tally()
std_microlab_cc3$specimen[str_detect(tolower(std_microlab_cc3$Specimen1), tolower("^Isolat, bronksekret, borste"))] <-  "Broncheal secretion"
std_microlab_cc3$specimen[str_detect(tolower(std_microlab_cc3$Specimen1), tolower("^Isolat, Nasopharynx"))] <-  "Nasopharyngeal"
std_microlab_cc3$specimen[str_detect(tolower(std_microlab_cc3$Specimen1), tolower("^Isolat, Sputum"))] <-  "Sputum"
std_microlab_cc3$specimen[str_detect(tolower(std_microlab_cc3$Specimen1), tolower("^Övrigt provmaterial, bakre svalgvägg"))] <-  "Pharyngeal swab"
std_microlab_cc3$specimen[str_detect(tolower(std_microlab_cc3$Specimen1), tolower("^Övrigt provmaterial, näsa"))] <-  "Nasal"
std_microlab_cc3$specimen[str_detect(tolower(std_microlab_cc3$Specimen1), tolower("^Övrigt provmaterial, Näsa"))] <-  "Nasal"
std_microlab_cc3$specimen[str_detect(tolower(std_microlab_cc3$Specimen1), tolower("^Övrigt provmaterial, nf"))] <-  "Nasopharyngeal"
std_microlab_cc3$specimen[str_detect(tolower(std_microlab_cc3$Specimen1), tolower("^Övrigt provmaterial, nph"))] <-  "Nasopharyngeal"
std_microlab_cc3$specimen[str_detect(tolower(std_microlab_cc3$Specimen1), tolower("^Övrigt provmaterial, sputum"))] <-  "Sputum"
std_microlab_cc3$specimen[str_detect(tolower(std_microlab_cc3$Specimen1), tolower("^Övrigt provmaterial, svalg"))] <-  "Throat"
std_microlab_cc3$specimen[str_detect(tolower(std_microlab_cc3$Specimen1), tolower("^Övrigt provmaterial, svalgsekret"))] <-  "Pharyngeal secretion"
std_microlab_cc3$specimen[str_detect(tolower(std_microlab_cc3$Specimen1), tolower("^Sekret, endotrachealtub"))] <-  "Endotracheal tube secretion"
std_microlab_cc3$specimen[str_detect(tolower(std_microlab_cc3$Specimen1), tolower("^Sekret, Tracheostoma"))] <-  "Tracheostoma secretion"
std_microlab_cc3$specimen[str_detect(tolower(std_microlab_cc3$Specimen1), tolower("^Övrigt provmaterial, bal"))] <-  "Bronchoalveolar Lavage"
std_microlab_cc3$specimen[str_detect(tolower(std_microlab_cc3$Specimen1), tolower("^Övrigt provmaterial, Blind borste"))] <-  "Brush"
std_microlab_cc3$specimen[str_detect(tolower(std_microlab_cc3$Specimen1), tolower("^Övrigt provmaterial, Borste"))] <-  "Brush"
std_microlab_cc3$specimen[str_detect(tolower(std_microlab_cc3$Specimen1), tolower("^Övrigt provmaterial, bronk"))] <-  "Bronchi"
std_microlab_cc3$specimen[str_detect(tolower(std_microlab_cc3$Specimen1), tolower("^Övrigt provmaterial, Bronksekret, borste"))] <-  "Brush"
std_microlab_cc3$specimen[str_detect(tolower(std_microlab_cc3$Specimen1), tolower("^Övrigt provmaterial, nasofarynx"))] <-  "Nasopharyngeal"
std_microlab_cc3$specimen[str_detect(tolower(std_microlab_cc3$Specimen1), tolower("^Övrigt provmaterial, nasopharynx"))] <-  "Nasopharyngeal"
std_microlab_cc3$specimen[str_detect(tolower(std_microlab_cc3$Specimen1), tolower("^Övrigt provmaterial, trach"))] <-  "Tracheal"
std_microlab_cc3$specimen[str_detect(tolower(std_microlab_cc3$Specimen1), tolower("^Övrigt provmaterial, Trackealsekret"))] <-  "Tracheal secretion"
std_microlab_cc3$specimen[str_detect(tolower(std_microlab_cc3$Specimen1), tolower("^Övrigt provmaterial, trakealsekret"))] <-  "Tracheal secretion"

sort(unique(std_microlab_cc3$specimen))
lrt_specimen <- "^Bronch|^Sputum|^Brush|^Trach|^Endotrach"
std_lrt_cc3 <- std_microlab_cc3 %>% filter(str_detect(specimen, regex(lrt_specimen, ignore_case = TRUE)))
sort(unique(std_lrt_cc3$Examination))
bac_culture <- "^Bakterieodl|^Mykobakterier|^Sputumodl|^Legionellaodl"
std_lrt_cc3 <- std_lrt_cc3 %>% filter(str_detect(Examination, regex(bac_culture, ignore_case = TRUE))) %>% filter(is.na(result))
sum(is.na(std_lrt_cc3$organism))
sort(unique(std_lrt_cc3$organism))

std_lrt_cc3$result[str_detect(tolower(std_lrt_cc3$organism), tolower("^Aspergillus"))] <-  "Contaminant"
std_lrt_cc3$result[str_detect(tolower(std_lrt_cc3$organism), tolower("^Candida"))] <- "Contaminant"
std_lrt_cc3$result[str_detect(tolower(std_lrt_cc3$organism), tolower("^Dermatophyte"))] <- "Contaminant"
std_lrt_cc3$result[str_detect(tolower(std_lrt_cc3$organism), tolower("^Gram negative mixed flora"))] <- "Contaminant"
std_lrt_cc3$result[str_detect(tolower(std_lrt_cc3$organism), tolower("^Mold"))] <- "Contaminant"
std_lrt_cc3$result[str_detect(tolower(std_lrt_cc3$organism), tolower("^No growth"))] <- "Contaminant"                            
std_lrt_cc3$result[str_detect(tolower(std_lrt_cc3$organism), tolower("^Normal bacteria"))] <- "Contaminant"
std_lrt_cc3$result[str_detect(tolower(std_lrt_cc3$organism), tolower("^Normal oral flora"))] <- "Contaminant"
std_lrt_cc3$result[str_detect(tolower(std_lrt_cc3$organism), tolower("^Normal respiratory flora"))] <- "Contaminant"
std_lrt_cc3$result[str_detect(tolower(std_lrt_cc3$organism), tolower("^Yeast"))] <- "Contaminant"
unique(std_lrt_cc3$result)
std_lrt_cc3 <- std_lrt_cc3 %>% filter(is.na(result))

#brush > 10^3 CFU/ml, BAL > 10^4 CFU/ml, sputum/tracheal secretions > 10^5 CFU/ml
std_lrt_cc3 <- std_lrt_cc3 %>% mutate(specimen = if_else(Specimen1 == "Bronksekret, borste", "Brush", specimen))
std_lrt_cc3 <- std_lrt_cc3 %>% mutate(result = if_else(str_detect(specimen, regex("brush", ignore_case = TRUE)), if_else(str_detect(Analysis, ">[eE]3|[eE][4-9]"), "Positive", "Negative"), result))
std_lrt_cc3 <- std_lrt_cc3 %>% mutate(result = if_else(str_detect(specimen, regex("Sputum", ignore_case = TRUE)), if_else(str_detect(Analysis, ">[eE]5|[eE][6-9]"), "Positive", "Negative"), result))
std_lrt_cc3 <- std_lrt_cc3 %>% mutate(result = if_else(str_detect(specimen, regex("Trach", ignore_case = TRUE)), if_else(str_detect(Analysis, ">[eE]5|[eE][6-9]"), "Positive", "Negative"), result))
std_lrt_cc3 <- std_lrt_cc3 %>% mutate(result = if_else(str_detect(specimen, regex("Broncheal", ignore_case = TRUE)), if_else(str_detect(Analysis, ">[eE]5|[eE][6-9]"), "Positive", "Negative"), result))
std_lrt_cc3 <- std_lrt_cc3 %>% mutate(result = if_else(str_detect(specimen, regex("Bronchoalveolar", ignore_case = TRUE)), if_else(str_detect(Analysis, ">[eE]4|[eE][5-9]"), "Positive", "Negative"), result))
std_lrt_cc3 <- std_lrt_cc3 %>% filter(result == "Positive") %>% mutate(lrt_result = result) %>% select(slv_table_id, lrt_result)
setwd('C:/Data/Shared Folder/People/Rubyath/Data/Working files/Intermediate data/Period 3')
fwrite(std_lrt_cc3, file = "lrt_cc3.csv", sep = ";", quote = TRUE, bom = TRUE)

##Blood culture

#Pre-pandemic
blood_cult <- std_microlab_cc1[str_detect(tolower(std_microlab_cc1$Examination), tolower("blod")) | str_detect(tolower(std_microlab_cc1$Specimen1), tolower("blod")),]
sum_blood_cult <- blood_cult %>% group_by(Examination, Specimen1, Analysis) %>% tally()
blood_cult <- blood_cult[str_detect(tolower(blood_cult$Examination), tolower("blododl")),]
sum_blood_cult <- blood_cult %>% group_by(Analysis, organism, result) %>% tally()
blood_cult <- blood_cult %>% mutate(result = replace_na(result, "tbd"))
blood_cult <- blood_cult %>% filter(result != "Negative")

blood_cult_organism <- blood_cult %>% filter(result != "Time to detection")
#blood_cult_dt <- blood_cult %>% filter(result == "Time to detection") %>% select(slv_table_id, Examination, Analysis)
#blood_cult_org_dt <- left_join(blood_cult_organism, blood_cult_dt, by = "slv_table_id")
sort(unique(blood_cult_organism$organism))

blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Abiotrophia species"))] <- "Pathogen"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Aerococcus viridans"))] <- "Pathogen"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Clostridium perfringens"))] <- "Pathogen"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Enterobacter cloacae"))] <- "Pathogen"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Enterococcus faecalis"))] <- "Pathogen"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Escherichia coli"))] <- "Pathogen"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Haemophilus parainfluenzae"))] <- "Pathogen"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Klebsiella pneumoniae"))] <- "Pathogen"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Proteus mirabilis"))] <- "Pathogen"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Pseudomonas aeruginosa"))] <- "Pathogen"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Pseudomonas species"))] <- "Pathogen"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Staphylococcus aureus"))] <- "Pathogen"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Streptococcus anginosus"))] <- "Pathogen"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Streptococcus bovis"))] <- "Pathogen"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Streptococcus mitis"))] <- "Pathogen"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Streptococcus mutans"))] <- "Pathogen"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Streptococcus pneumoniae"))] <- "Pathogen"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Streptococcus salivarius"))] <- "Pathogen"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Streptococcus sanguinis"))] <- "Pathogen"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Streptococci, Alpha"))] <- "Pathogen"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Streptococci, beta-hemolytic"))] <- "Pathogen"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Corynebacterium pseudodiphtheriticum"))] <- "Contaminant"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Bacillus species"))] <- "Contaminant"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Bacillus cereus"))] <- "Contaminant"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Actinomyces species"))] <- "Contaminant"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Coagulase negative Staphylococci"))] <- "Contaminant"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Corynebacterium species"))] <- "Contaminant"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Cutibacterium acnes"))] <- "Contaminant"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Cutibacterium species"))] <- "Contaminant"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Micrococcus species"))] <- "Contaminant"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Micrococcus luteus"))] <- "Contaminant"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Propionimicrobium species"))] <- "Contaminant"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Staphylococcus epidermidis"))] <- "Contaminant"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Staphylococcus saccharolyticus"))] <- "Contaminant"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Veillonella species"))] <- "Contaminant"

blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Gram positive coccus"))] <- "Inconclusive"
blood_cult_cc1 <- blood_cult_organism %>% filter(result == "Pathogen") %>% select(slv_table_id, result)
blood_cult_inc <- blood_cult_organism %>% filter(result == "Inconclusive")
setwd('C:/Data/Shared Folder/People/Rubyath/Data/Working files/Intermediate data/Period 1')
fwrite(blood_cult_cc1, file = "blood_cult_cc1.csv", sep = ";", quote = TRUE, bom = TRUE)
fwrite(blood_cult_inc, file = "blood_cult_inc.csv", sep = ";", quote = TRUE, bom = TRUE)

#Pandemic

blood_cult <- std_microlab_cc2[str_detect(tolower(std_microlab_cc2$Examination), tolower("blod")) | str_detect(tolower(std_microlab_cc2$Specimen1), tolower("blod")),]
sum_blood_cult <- blood_cult %>% group_by(Examination, Specimen1, Analysis) %>% tally()
blood_cult <- blood_cult[str_detect(tolower(blood_cult$Examination), tolower("blododl")),]
sum_blood_cult <- blood_cult %>% group_by(Analysis, organism, result) %>% tally()
blood_cult <- blood_cult %>% mutate(result = replace_na(result, "tbd"))
blood_cult <- blood_cult %>% filter(result != "Negative")

blood_cult_organism <- blood_cult %>% filter(result != "Time to detection")
blood_cult_dt <- blood_cult %>% filter(result == "Time to detection") %>% select(slv_table_id, Examination, Analysis)
blood_cult_org_dt <- left_join(blood_cult_organism, blood_cult_dt, by = "slv_table_id")
sort(unique(blood_cult_organism$organism))

blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Abiotrophia species"))] <-  "Pathogen"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Actinomyces neuii"))] <-  "Contaminant"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Actinomyces odontolyticus"))] <-  "Contaminant"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Actinomyces species"))] <-  "Contaminant"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Actinotignum species"))] <-  "Pathogen"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Aerococcus sanguinicola"))] <-  "Pathogen"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Aerococcus urinae"))] <-  "Pathogen"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Aerococcus viridans"))] <-  "Pathogen"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Bacillus cereus"))] <-  "Contaminant"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Bacillus species"))] <-  "Contaminant"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Bacteroides fragilis"))] <-  "Pathogen"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Bacteroides species"))] <-  "Pathogen"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Bifidobacterium species"))] <-  "Contaminant"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Brevibacillus species"))] <-  "Contaminant"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Brevibacterium casei"))] <-  "Contaminant"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Cellulomonas species"))] <-  "Contaminant"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Citrobacter freundii"))] <-  "Pathogen"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Citrobacter koseri"))] <-  "Pathogen"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Citrobacter species"))] <-  "Pathogen"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Clostridium perfringens"))] <-  "Pathogen"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Clostridium ramosum"))] <-  "Pathogen"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Coagulase negative Staphylococci"))] <-  "Contaminant"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Corynebacterium jeikeium"))] <-  "Contaminant"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Corynebacterium pseudodiphtheriticum"))] <-  "Contaminant"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Corynebacterium species"))] <-  "Contaminant"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Cutibacterium acnes"))] <-  "Contaminant"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Cutibacterium species"))] <-  "Contaminant"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Dermabacter hominis"))] <-  "Contaminant"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Dietzia species"))] <-  "Contaminant"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Eggerthella lenta"))] <-  "Contaminant"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Eikenella corrodens"))] <-  "Pathogen"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Enterobacter cloacae"))] <-  "Pathogen"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Enterobacter species"))] <-  "Pathogen"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Enterococcus avium"))] <-  "Pathogen"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Enterococcus faecalis"))] <-  "Pathogen"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Enterococcus faecium"))] <-  "Pathogen"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Escherichia coli"))] <-  "Pathogen"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Filifactor alocis"))] <-  "Contaminant"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Fusobacterium nucleatum"))] <-  "Contaminant"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Fusobacterium species"))] <-  "Contaminant"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Globicatella species"))] <-  "Pathogen"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Gordonia species"))] <-  "Contaminant"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Gram positive rod"))] <-  "Contaminant"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Granulicatella adiacens"))] <-  "Contaminant"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Haemophilus parainfluenzae"))] <-  "Pathogen"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Klebsiella aerogenes"))] <-  "Pathogen"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Klebsiella oxytoca"))] <-  "Pathogen"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Klebsiella pneumoniae"))] <-  "Pathogen"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Klebsiella variicola"))] <-  "Pathogen"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Kocuria species"))] <-  "Contaminant"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Brevibacterium species"))] <- "Contaminant"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Dermabacter species"))] <- "Contaminant"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Enterococcus hirae"))] <- "Pathogen"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Listeria monocytogenes"))] <- "Pathogen"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Lysinibacillus sphaericus"))] <-  "Contaminant"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Micrococcus luteus"))] <-  "Contaminant"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Micrococcus species"))] <-  "Contaminant"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Mixed bacterial flora"))] <-  "Contaminant"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Moraxella species"))] <-  "Contaminant"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Peptoniphilus species"))] <-  "Contaminant"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Prevotella species"))] <-  "Contaminant"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Propionimicrobium species"))] <-  "Contaminant"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Proteus mirabilis"))] <-  "Pathogen"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Proteus vulgaris"))] <-  "Pathogen"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Pseudoglutamicibacter cumminsii"))] <-  "Contaminant"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Pseudomonas aeruginosa"))] <-  "Pathogen"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Pseudomonas species"))] <-  "Pathogen"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Rothia species"))] <-  "Contaminant"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Staphylococcus argenteus"))] <-  "Pathogen"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Staphylococcus aureus"))] <-  "Pathogen"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Staphylococcus epidermidis"))] <-  "Contaminant"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Staphylococcus lugdunensis"))] <-  "Pathogen"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Staphylococcus saccharolyticus"))] <-  "Contaminant"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Stenotrophomonas maltophilia"))] <-  "Pathogen"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Streptococci, Alpha"))] <-  "Pathogen"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Streptococci, beta-hemolytic"))] <- "Pathogen"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Streptococcus agalactiae"))] <-  "Pathogen"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Streptococcus anginosus"))] <-  "Pathogen"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Streptococcus bovis"))] <-  "Pathogen"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Streptococcus mitis"))] <-  "Pathogen"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Streptococcus mutans"))] <-  "Pathogen"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Streptococcus pneumoniae"))] <-  "Pathogen"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Streptococcus salivarius"))] <-  "Pathogen"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Streptococcus sanguinis"))] <-  "Pathogen"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Veillonella species"))] <-  "Contaminant"

organism_na <- blood_cult_organism %>% filter(result == "tbd")
sort(unique(organism_na$organism))

blood_cult_cc2 <- blood_cult_organism %>% filter(result == "Pathogen") %>% select(slv_table_id, result)
#blood_cult_inc <- blood_cult_organism %>% filter(result == "Inconclusive")
setwd('C:/Data/Shared Folder/People/Rubyath/Data/Working files/Intermediate data/Period 2')
fwrite(blood_cult_cc2, file = "blood_cult_cc2.csv", sep = ";", quote = TRUE, bom = TRUE)
#fwrite(blood_cult_inc, file = "blood_cult_inc.csv", sep = ";", quote = TRUE, bom = TRUE)

#Post-pandemic

blood_cult <- std_microlab_cc3[str_detect(tolower(std_microlab_cc3$Examination), tolower("blod")) | str_detect(tolower(std_microlab_cc3$Specimen1), tolower("blod")),]
sum_blood_cult <- blood_cult %>% group_by(Examination, Specimen1, Analysis) %>% tally()
blood_cult <- blood_cult[str_detect(tolower(blood_cult$Examination), tolower("blododl")),]
sum_blood_cult <- blood_cult %>% group_by(Analysis, organism, result) %>% tally()
blood_cult <- blood_cult %>% mutate(result = replace_na(result, "tbd"))
blood_cult <- blood_cult %>% filter(result != "Negative")

blood_cult_organism <- blood_cult %>% filter(result != "Time to detection")
blood_cult_dt <- blood_cult %>% filter(result == "Time to detection") %>% select(slv_table_id, Examination, Analysis)
blood_cult_org_dt <- left_join(blood_cult_organism, blood_cult_dt, by = "slv_table_id")
sort(unique(blood_cult_organism$organism))

blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Abiotrophia species"))] <-  "Pathogen"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Actinomyces neuii"))] <-  "Contaminant"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Actinomyces odontolyticus"))] <-  "Contaminant"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Actinomyces species"))] <-  "Contaminant"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Actinotignum species"))] <-  "Pathogen"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Aerococcus sanguinicola"))] <-  "Pathogen"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Aerococcus urinae"))] <-  "Pathogen"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Aerococcus viridans"))] <-  "Pathogen"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Bacillus cereus"))] <-  "Contaminant"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Bacillus species"))] <-  "Contaminant"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Bacteroides fragilis"))] <-  "Pathogen"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Bacteroides species"))] <-  "Pathogen"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Bifidobacterium species"))] <-  "Contaminant"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Brevibacillus species"))] <-  "Contaminant"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Brevibacterium casei"))] <-  "Contaminant"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Cellulomonas species"))] <-  "Contaminant"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Citrobacter freundii"))] <-  "Pathogen"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Citrobacter koseri"))] <-  "Pathogen"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Citrobacter species"))] <-  "Pathogen"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Clostridium perfringens"))] <-  "Pathogen"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Clostridium ramosum"))] <-  "Pathogen"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Coagulase negative Staphylococci"))] <-  "Contaminant"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Corynebacterium jeikeium"))] <-  "Contaminant"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Corynebacterium pseudodiphtheriticum"))] <-  "Contaminant"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Corynebacterium species"))] <-  "Contaminant"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Cutibacterium acnes"))] <-  "Contaminant"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Cutibacterium species"))] <-  "Contaminant"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Dermabacter hominis"))] <-  "Contaminant"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Dietzia species"))] <-  "Contaminant"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Eggerthella lenta"))] <-  "Contaminant"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Eikenella corrodens"))] <-  "Pathogen"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Enterobacter cloacae"))] <-  "Pathogen"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Enterobacter species"))] <-  "Pathogen"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Enterococcus avium"))] <-  "Pathogen"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Enterococcus faecalis"))] <-  "Pathogen"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Enterococcus faecium"))] <-  "Pathogen"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Escherichia coli"))] <-  "Pathogen"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Filifactor alocis"))] <-  "Contaminant"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Fusobacterium nucleatum"))] <-  "Contaminant"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Fusobacterium species"))] <-  "Contaminant"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Globicatella species"))] <-  "Pathogen"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Gordonia species"))] <-  "Contaminant"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Gram positive rod"))] <-  "Contaminant"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Granulicatella adiacens"))] <-  "Contaminant"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Haemophilus parainfluenzae"))] <-  "Pathogen"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Klebsiella aerogenes"))] <-  "Pathogen"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Klebsiella oxytoca"))] <-  "Pathogen"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Klebsiella pneumoniae"))] <-  "Pathogen"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Klebsiella variicola"))] <-  "Pathogen"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Kocuria species"))] <-  "Contaminant"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Brevibacterium species"))] <- "Contaminant"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Dermabacter species"))] <- "Contaminant"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Enterococcus hirae"))] <- "Pathogen"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Listeria monocytogenes"))] <- "Pathogen"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Lysinibacillus sphaericus"))] <-  "Contaminant"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Micrococcus luteus"))] <-  "Contaminant"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Micrococcus species"))] <-  "Contaminant"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Mixed bacterial flora"))] <-  "Contaminant"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Moraxella species"))] <-  "Contaminant"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Peptoniphilus species"))] <-  "Contaminant"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Prevotella species"))] <-  "Contaminant"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Propionimicrobium species"))] <-  "Contaminant"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Proteus mirabilis"))] <-  "Pathogen"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Proteus vulgaris"))] <-  "Pathogen"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Pseudoglutamicibacter cumminsii"))] <-  "Contaminant"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Pseudomonas aeruginosa"))] <-  "Pathogen"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Pseudomonas species"))] <-  "Pathogen"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Rothia species"))] <-  "Contaminant"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Staphylococcus argenteus"))] <-  "Pathogen"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Staphylococcus aureus"))] <-  "Pathogen"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Staphylococcus epidermidis"))] <-  "Contaminant"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Staphylococcus lugdunensis"))] <-  "Pathogen"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Staphylococcus saccharolyticus"))] <-  "Contaminant"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Stenotrophomonas maltophilia"))] <-  "Pathogen"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Streptococci, Alpha"))] <-  "Pathogen"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Streptococci, beta-hemolytic"))] <- "Pathogen"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Streptococcus agalactiae"))] <-  "Pathogen"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Streptococcus anginosus"))] <-  "Pathogen"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Streptococcus bovis"))] <-  "Pathogen"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Streptococcus mitis"))] <-  "Pathogen"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Streptococcus mutans"))] <-  "Pathogen"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Streptococcus pneumoniae"))] <-  "Pathogen"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Streptococcus salivarius"))] <-  "Pathogen"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Streptococcus sanguinis"))] <-  "Pathogen"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Veillonella species"))] <-  "Contaminant"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Acinetobacter species"))] <- "Contaminant"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Actinotignum schaalii"))] <- "Pathogen"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Campylobacter ureolyticus"))] <- "Contaminant"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Corynebacterium striatum"))] <- "Contaminant"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Dialister species"))] <- "Contaminant"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Eikenella species"))] <- "Pathogen"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Enterococcus casseliflavus"))] <- "Pathogen"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Eubacterium species"))] <- "Contaminant"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Haemophilus influenzae"))] <- "Pathogen"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Hungatella hathewayi"))] <- "Pathogen"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Hungatella species"))] <- "Pathogen"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Gram negative rod"))] <- "Contaminant"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Rothia mucilaginosa"))] <- "Contaminant"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Salmonella dublin"))] <- "Pathogen"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Moraxella catarrhalis"))] <- "Pathogen"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Serratia marcescens"))] <- "Pathogen"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Neisseria species"))] <- "Contaminant"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Psychrobacter species"))] <- "Contaminant"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Gram positive coccus"))] <- "Inconclusive"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Brevundimonas species"))] <- "Inconclusive"
blood_cult_organism$result[str_detect(tolower(blood_cult_organism$organism), tolower("^Rhizobium species"))] <- "Inconclusive"

organism_na <- blood_cult_organism %>% filter(result == "tbd")
sort(unique(organism_na$organism))

blood_cult_cc3 <- blood_cult_organism %>% filter(result == "Pathogen") %>% select(slv_table_id, result)
blood_cult_inc <- blood_cult_organism %>% filter(result == "Inconclusive")
setwd('C:/Data/Shared Folder/People/Rubyath/Data/Working files/Intermediate data/Period 3')
fwrite(blood_cult_cc3, file = "blood_cult_cc3.csv", sep = ";", quote = TRUE, bom = TRUE)
fwrite(blood_cult_inc, file = "blood_cult_inc.csv", sep = ";", quote = TRUE, bom = TRUE)


##joining bacterial co-infection information
#Pre-pandemic


rm(list=ls())
gc()
pacman::p_load(dplyr, readr, lubridate, data.table, stringr, janitor)

setwd('C:/Data/Shared Folder/People/Rubyath/Data/Working files/Intermediate data/Period 1')
std_cc1_co_cci <- read_csv2("std_cc1_co_cci.csv")
blood_cult_cc1 <- read_csv2("blood_cult_cc1.csv")
lrt_cc1 <- read_csv2("lrt_cc1.csv")
resp_dna_cc1 <- read_csv2("resp_dna_cc1.csv")
urine_ag_cc1 <- read_csv2("urine_ag_cc1.csv")

blood_cult_cc1 <- blood_cult_cc1 %>% rename(bc_inf = result) %>% mutate(bc_inf = ifelse(bc_inf == "Pathogen", 1, 0)) %>% distinct(slv_table_id, .keep_all = TRUE)
std_cc1_co_cov <- left_join(std_cc1_co_cci, blood_cult_cc1, by = "slv_table_id")
rm(std_cc1_co_cci)
std_cc1_co_cov <- std_cc1_co_cov %>% mutate(bc_inf = ifelse(is.na(bc_inf), 0, bc_inf))
lrt_cc1 <- lrt_cc1 %>% mutate(lrt_result = ifelse(lrt_result == "Positive", 1, 0)) %>% distinct(slv_table_id, .keep_all = TRUE)
std_cc1_co_cov <- left_join(std_cc1_co_cov, lrt_cc1, by = "slv_table_id")
std_cc1_co_cov <- std_cc1_co_cov %>% mutate(lrt_result = ifelse(is.na(lrt_result), 0, lrt_result))
urine_ag_cc1 <- urine_ag_cc1 %>% distinct(slv_table_id, .keep_all = TRUE)
std_cc1_co_cov <- left_join(std_cc1_co_cov, urine_ag_cc1, by = "slv_table_id")
std_cc1_co_cov <- std_cc1_co_cov %>% mutate(urine_coinf = ifelse(is.na(urine_coinf), 0, urine_coinf))

resp_dna_cc1 <- resp_dna_cc1 %>% rename(rdna_inf = result) %>% select(slv_table_id, rdna_inf) %>% distinct(slv_table_id, .keep_all = TRUE)
resp_dna_cc1 <- resp_dna_cc1 %>% mutate(rdna_inf = ifelse(rdna_inf == "Positive", 1, 0))
std_cc1_co_cov <- left_join(std_cc1_co_cov, resp_dna_cc1, by = "slv_table_id") 
std_cc1_co_cov <- std_cc1_co_cov %>% mutate(rdna_inf = ifelse(is.na(rdna_inf), 0, rdna_inf))
std_cc1_co_cov <- std_cc1_co_cov %>% mutate(coinf = bc_inf + lrt_result + urine_coinf + rdna_inf, bac_coinf = ifelse(coinf >= 1, "Yes", "No"))
std_cc1_co_cov <- std_cc1_co_cov %>% select(-c(AnalysisID, first_admission_id, diag_code, SamplingDatetime, recoded_analysis, Specimen1, in_date, out_date, vtfhuvudid, inskrtidpunkt, action_code, diff_mor, kvakod, action_date, bc_inf, rdna_inf, urine_coinf, utskrtidpunkt, lrt_result, coinf))
fwrite(std_cc1_co_cov, file = "std_cc1_co_cov.csv", sep = ";", quote = TRUE, bom = TRUE)


#Pandemic

rm(list=ls())
setwd('C:/Data/Shared Folder/People/Rubyath/Data/Working files/Intermediate data/Period 2')
std_cc2_co_cci <- read_csv2("std_cc2_co_cci.csv")
blood_cult_cc2 <- read_csv2("blood_cult_cc2.csv")
lrt_cc2 <- read_csv2("lrt_cc2.csv")
urine_ag_cc2 <- read_csv2("urine_ag_cc2.csv")

std_cc2_co_cci <- std_cc2_co_cci %>% mutate(hfnc = ifelse(!is.na(ovr_table_id), "Yes", hfnc))
std_cc2_co_cci <- std_cc2_co_cci %>% select(-c(ovr_table_id, action_code.y, action_name, action_date.y)) %>% rename(slv_table_id = slv_table_id.x)
blood_cult_cc2 <- blood_cult_cc2 %>% rename(bc_inf = result) %>% mutate(bc_inf = ifelse(bc_inf == "Pathogen", 1, 0)) %>% distinct(slv_table_id, .keep_all = TRUE)
std_cc2_co_cov <- left_join(std_cc2_co_cci, blood_cult_cc2, by = "slv_table_id")
rm(std_cc2_co_cci)
std_cc2_co_cov <- std_cc2_co_cov %>% mutate(bc_inf = ifelse(is.na(bc_inf), 0, bc_inf))
lrt_cc2 <- lrt_cc2 %>% mutate(lrt_result = ifelse(lrt_result == "Positive", 1, 0)) %>% distinct(slv_table_id, .keep_all = TRUE)
std_cc2_co_cov <- left_join(std_cc2_co_cov, lrt_cc2, by = "slv_table_id")
std_cc2_co_cov <- std_cc2_co_cov %>% mutate(lrt_result = ifelse(is.na(lrt_result), 0, lrt_result))
urine_ag_cc2 <- urine_ag_cc2 %>% distinct(slv_table_id, .keep_all = TRUE)
std_cc2_co_cov <- left_join(std_cc2_co_cov, urine_ag_cc2, by = "slv_table_id")
std_cc2_co_cov <- std_cc2_co_cov %>% mutate(urine_coinf = ifelse(is.na(urine_coinf), 0, urine_coinf))

#resp_dna_cc2 <- resp_dna_cc2 %>% rename(rdna_inf = result) %>% select(slv_table_id, rdna_inf) %>% distinct(slv_table_id, .keep_all = TRUE)
#resp_dna_cc2 <- resp_dna_cc2 %>% mutate(rdna_inf = ifelse(rdna_inf == "Positive", 1, 0))
#std_cc2_co_cov <- left_join(std_cc2_co_cov, resp_dna_cc2, by = "slv_table_id") 
#std_cc2_co_cov <- std_cc2_co_cov %>% mutate(rdna_inf = ifelse(is.na(rdna_inf), 0, rdna_inf))
std_cc2_co_cov <- std_cc2_co_cov %>% mutate(coinf = bc_inf + lrt_result + urine_coinf, bac_coinf = ifelse(coinf >= 1, "Yes", "No"))
std_cc2_co_cov <- std_cc2_co_cov %>% select(-c(AnalysisID, first_admission_id, diag_code, SamplingDatetime, recoded_analysis, Specimen1, in_date, out_date, vtfhuvudid, inskrtidpunkt, action_code.x, diff_mor, kvakod, action_date.x, bc_inf, urine_coinf, utskrtidpunkt, lrt_result, coinf))
fwrite(std_cc2_co_cov, file = "std_cc2_co_cov.csv", sep = ";", quote = TRUE, bom = TRUE)


#Post-pandemic

rm(list=ls())
setwd('C:/Data/Shared Folder/People/Rubyath/Data/Working files/Intermediate data/Period 3')
std_cc3_co_cci <- read_csv2("std_cc3_co_cci.csv")
blood_cult_cc3 <- read_csv2("blood_cult_cc3.csv")
lrt_cc3 <- read_csv2("lrt_cc3.csv")
resp_dna_cc3 <- read_csv2("resp_dna_cc3.csv")
urine_ag_cc3 <- read_csv2("urine_ag_cc3.csv")

std_cc3_co_cci <- std_cc3_co_cci %>% mutate(hfnc = ifelse(!is.na(ovr_table_id), "Yes", hfnc))
std_cc3_co_cci <- std_cc3_co_cci %>% select(-c(ovr_table_id, action_code.y, action_name, action_date.y)) %>% rename(slv_table_id = slv_table_id.x)
blood_cult_cc3 <- blood_cult_cc3 %>% rename(bc_inf = result) %>% mutate(bc_inf = ifelse(bc_inf == "Pathogen", 1, 0)) %>% distinct(slv_table_id, .keep_all = TRUE)
std_cc3_co_cov <- left_join(std_cc3_co_cci, blood_cult_cc3, by = "slv_table_id")
rm(std_cc3_co_cci)
std_cc3_co_cov <- std_cc3_co_cov %>% mutate(bc_inf = ifelse(is.na(bc_inf), 0, bc_inf))
lrt_cc3 <- lrt_cc3 %>% mutate(lrt_result = ifelse(lrt_result == "Positive", 1, 0)) %>% distinct(slv_table_id, .keep_all = TRUE)
std_cc3_co_cov <- left_join(std_cc3_co_cov, lrt_cc3, by = "slv_table_id")
std_cc3_co_cov <- std_cc3_co_cov %>% mutate(lrt_result = ifelse(is.na(lrt_result), 0, lrt_result))
urine_ag_cc3 <- urine_ag_cc3 %>% distinct(slv_table_id, .keep_all = TRUE)
std_cc3_co_cov <- left_join(std_cc3_co_cov, urine_ag_cc3, by = "slv_table_id")
std_cc3_co_cov <- std_cc3_co_cov %>% mutate(urine_coinf = ifelse(is.na(urine_coinf), 0, urine_coinf))

resp_dna_cc3 <- resp_dna_cc3 %>% rename(rdna_inf = result) %>% select(slv_table_id, rdna_inf) %>% distinct(slv_table_id, .keep_all = TRUE)
resp_dna_cc3 <- resp_dna_cc3 %>% mutate(rdna_inf = ifelse(rdna_inf == "Positive", 1, 0))
std_cc3_co_cov <- left_join(std_cc3_co_cov, resp_dna_cc3, by = "slv_table_id") 
std_cc3_co_cov <- std_cc3_co_cov %>% mutate(rdna_inf = ifelse(is.na(rdna_inf), 0, rdna_inf))
std_cc3_co_cov <- std_cc3_co_cov %>% mutate(coinf = bc_inf + lrt_result + rdna_inf + urine_coinf, bac_coinf = ifelse(coinf >= 1, "Yes", "No"))
std_cc3_co_cov <- std_cc3_co_cov %>% select(-c(AnalysisID, first_admission_id, diag_code, SamplingDatetime, recoded_analysis, Specimen1, in_date, out_date, vtfhuvudid, inskrtidpunkt, action_code.x, diff_mor, kvakod, action_date.x, bc_inf, urine_coinf, utskrtidpunkt, lrt_result, rdna_inf, coinf))
fwrite(std_cc3_co_cov, file = "std_cc3_co_cov.csv", sep = ";", quote = TRUE, bom = TRUE)
