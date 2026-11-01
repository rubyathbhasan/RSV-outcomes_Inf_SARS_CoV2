#Influenza, RSV, SARS_COV-2

rm(list=ls())
pacman::p_load(dplyr, readr, data.table, stringr, lubridate)
setwd('C:/Data/Shared Folder/People/Rubyath/Data/Working files/Intermediate data/Period 1')
cc1_microlab_scedemo <- read_csv2("cc1_microlab_scedemo.csv")
cc1_pcr <- cc1_microlab_scedemo %>% filter(Examination == "Molekylär diagnostik")
rm(cc1_microlab_scedemo)

cc1_pcr$specimen[str_detect(tolower(cc1_pcr$Specimen1), tolower("^Övrigt provmaterial, Bronksekret"))] <- "Broncheal secretion/aspirate"
cc1_pcr$specimen[str_detect(tolower(cc1_pcr$Specimen1), tolower("^Övrigt provmaterial, Bronker"))] <- "Bronchi"
cc1_pcr$specimen[str_detect(tolower(cc1_pcr$Specimen1), tolower("^Övrigt provmaterial, Näsa"))] <- "Nasal"
cc1_pcr$specimen[str_detect(tolower(cc1_pcr$Specimen1), tolower("^Övrigt provmaterial, BAL"))] <- "Bronchoalveolar Lavage"
cc1_pcr$specimen[str_detect(tolower(cc1_pcr$Specimen1), tolower("^Övrigt provmaterial, nasalt"))] <- "Nasal"
cc1_pcr$specimen[str_detect(tolower(cc1_pcr$Specimen1), tolower("^Övrigt provmaterial, nässekret"))] <- "Nasal secretion"
cc1_pcr$specimen[str_detect(tolower(cc1_pcr$Specimen1), tolower("^Övrigt provmaterial, nösa"))] <- "Nasal"
cc1_pcr$specimen[str_detect(tolower(cc1_pcr$Specimen1), tolower("^Övrigt provmaterial, näshåla"))] <- "Nasal cavity"
cc1_pcr$specimen[str_detect(tolower(cc1_pcr$Specimen1), tolower("^Övrigt provmaterial, nf-aspirat"))] <- "Nasopharyngeal aspirate"
cc1_pcr$specimen[str_detect(tolower(cc1_pcr$Specimen1), tolower("^Övrigt provmaterial, Näsprov"))] <- "Nasal sample"
cc1_pcr$specimen[str_detect(tolower(cc1_pcr$Specimen1), tolower("^Övrigt provmaterial, nph"))] <- "Nasopharyngeal"
cc1_pcr$specimen[str_detect(tolower(cc1_pcr$Specimen1), tolower("^Övrigt provmaterial, Svalj"))] <- "Throat"
cc1_pcr$specimen[str_detect(tolower(cc1_pcr$Specimen1), tolower("^Övrigt provmaterial, nasofarynx"))] <- "Nasopharyngeal"
cc1_pcr$specimen[str_detect(tolower(cc1_pcr$Specimen1), tolower("^Övrigt provmaterial, nf"))] <- "Nasopharyngeal"
cc1_pcr$specimen[str_detect(tolower(cc1_pcr$Specimen1), tolower("^Övrigt provmaterial, track"))] <- "Tracheal"
cc1_pcr$specimen[str_detect(tolower(cc1_pcr$Specimen1), tolower("^Övrigt provmaterial, Svalgsekret"))] <- "Pharyngeal secretion"
cc1_pcr$specimen[str_detect(tolower(cc1_pcr$Specimen1), tolower("^Övrigt provmaterial, bakre svalgvägg"))] <- "Pharyngeal swab"
cc1_pcr$specimen[str_detect(tolower(cc1_pcr$Specimen1), tolower("^Övrigt provmaterial, bakre svalgväggen"))] <- "Pharyngeal swab"
cc1_pcr$specimen[str_detect(tolower(cc1_pcr$Specimen1), tolower("^Övrigt provmaterial, sputum"))] <- "Throat"
cc1_pcr$specimen[str_detect(tolower(cc1_pcr$Specimen1), tolower("^Övrigt provmaterial, svalg"))] <- "Throat"
cc1_pcr$specimen[str_detect(tolower(cc1_pcr$Specimen1), tolower("^Övrigt provmaterial, bakre svalg"))] <- "Throat, deep"

respiratory_specimen <- "^Bronch|^Nasal|^Nasopharyngeal|^Respiratory|^Sputum|^Throat|^Trach"
respiratory_cc1_microlab <- cc1_pcr %>% filter(str_detect(specimen, regex(respiratory_specimen, ignore_case = TRUE)))
rm(cc1_pcr)
rm(respiratory_specimen)

respiratory_viruses <- "^Adenovirus|^Bocavirus|^Coronavirus|^Enterovirus|^Influenza|^Metapneumovirus|^Parainfluenzavirus|^Rhinovirus|^RSV|^SARS-CoV-2|^Cytomegalovirus|^Epstein-Barr|^Herpes|^Morbillivirus|^Varicella zoster"
respiratory_cc1_virus <- respiratory_cc1_microlab %>% filter(str_detect(organism, regex(respiratory_viruses, ignore_case = TRUE))) %>% filter(result == "Positive")
rm(respiratory_cc1_microlab)
respiratory_cc1_virus <- respiratory_cc1_virus %>% mutate(organism_inf_new = if_else(organism %in% c("Influenza A", "Influenza B"), "Influenza A/B", organism))
multi_virus <- respiratory_cc1_virus %>% 
  filter(duplicated(slv_table_id) | duplicated(slv_table_id, fromLast = TRUE))
rm(multi_virus)

#remove if same slv_table_id has same organism, keep one
##for cross-checking if mistakenly deleted values
#n_distinct(respiratory_cc1_virus$slv_table_id)
#respiratory_cc1_virus_dupes <- respiratory_cc1_virus %>% group_by(slv_table_id, organism_inf_new) %>% filter(n() > 1) %>% arrange(slv_table_id, organism_inf_new)
respiratory_cc1_virus <- respiratory_cc1_virus %>% distinct(slv_table_id, organism, .keep_all = TRUE)
n_distinct(respiratory_cc1_virus$slv_table_id)
#remove both if slv is duplicated
cc1_virus_unq <- respiratory_cc1_virus %>% group_by(slv_table_id) %>% filter(n_distinct(organism) == 1) %>% ungroup()

#filter study viruses
std_viruses <- "^Influenza|^RSV|^SARS-CoV-2"
std_cc1_virus <- cc1_virus_unq %>% filter(str_detect(organism, regex(std_viruses, ignore_case = TRUE))) 
fwrite(std_cc1_virus, file= "std_cc1_virus_unq.csv", sep = ";", quote = TRUE, bom = TRUE)


####-----#######
#Pandemic

rm(list=ls())
setwd('C:/Data/Shared Folder/People/Rubyath/Data/Working files/Intermediate data/Period 2')
cc2_microlab_scedemo <- read_csv2("cc2_microlab_scedemo.csv")
cc2_pcr <- cc2_microlab_scedemo %>% filter(Examination %in% c("Molekylär diagnostik", "Molekylär diagnostik, öppenvård"))
rm(cc2_microlab_scedemo)

cc2_pcr$specimen[str_detect(tolower(cc2_pcr$Specimen1), tolower("^Övrigt provmaterial, Bronksekret"))] <- "Broncheal secretion/aspirate"
cc2_pcr$specimen[str_detect(tolower(cc2_pcr$Specimen1), tolower("^Övrigt provmaterial, Bronker"))] <- "Bronchi"
cc2_pcr$specimen[str_detect(tolower(cc2_pcr$Specimen1), tolower("^Övrigt provmaterial, Näsa"))] <- "Nasal"
cc2_pcr$specimen[str_detect(tolower(cc2_pcr$Specimen1), tolower("^Övrigt provmaterial, BAL"))] <- "Bronchoalveolar Lavage"
cc2_pcr$specimen[str_detect(tolower(cc2_pcr$Specimen1), tolower("^Övrigt provmaterial, nasalt"))] <- "Nasal"
cc2_pcr$specimen[str_detect(tolower(cc2_pcr$Specimen1), tolower("^Övrigt provmaterial, nässekret"))] <- "Nasal secretion"
cc2_pcr$specimen[str_detect(tolower(cc2_pcr$Specimen1), tolower("^Övrigt provmaterial, nösa"))] <- "Nasal"
cc2_pcr$specimen[str_detect(tolower(cc2_pcr$Specimen1), tolower("^Övrigt provmaterial, näshåla"))] <- "Nasal cavity"
cc2_pcr$specimen[str_detect(tolower(cc2_pcr$Specimen1), tolower("^Övrigt provmaterial, nf-aspirat"))] <- "Nasopharyngeal aspirate"
cc2_pcr$specimen[str_detect(tolower(cc2_pcr$Specimen1), tolower("^Övrigt provmaterial, Näsprov"))] <- "Nasal sample"
cc2_pcr$specimen[str_detect(tolower(cc2_pcr$Specimen1), tolower("^Övrigt provmaterial, nph"))] <- "Nasopharyngeal"
cc2_pcr$specimen[str_detect(tolower(cc2_pcr$Specimen1), tolower("^Övrigt provmaterial, Svalj"))] <- "Throat"
cc2_pcr$specimen[str_detect(tolower(cc2_pcr$Specimen1), tolower("^Övrigt provmaterial, nasofarynx"))] <- "Nasopharyngeal"
cc2_pcr$specimen[str_detect(tolower(cc2_pcr$Specimen1), tolower("^Övrigt provmaterial, nf"))] <- "Nasopharyngeal"
cc2_pcr$specimen[str_detect(tolower(cc2_pcr$Specimen1), tolower("^Övrigt provmaterial, track"))] <- "Tracheal"
cc2_pcr$specimen[str_detect(tolower(cc2_pcr$Specimen1), tolower("^Övrigt provmaterial, Svalgsekret"))] <- "Pharyngeal secretion"
cc2_pcr$specimen[str_detect(tolower(cc2_pcr$Specimen1), tolower("^Övrigt provmaterial, bakre svalgvägg"))] <- "Pharyngeal swab"
cc2_pcr$specimen[str_detect(tolower(cc2_pcr$Specimen1), tolower("^Övrigt provmaterial, bakre svalgväggen"))] <- "Pharyngeal swab"
cc2_pcr$specimen[str_detect(tolower(cc2_pcr$Specimen1), tolower("^Övrigt provmaterial, sputum"))] <- "Throat"
cc2_pcr$specimen[str_detect(tolower(cc2_pcr$Specimen1), tolower("^Övrigt provmaterial, svalg"))] <- "Throat"
cc2_pcr$specimen[str_detect(tolower(cc2_pcr$Specimen1), tolower("^Övrigt provmaterial, bakre svalg"))] <- "Throat, deep"
cc2_pcr$specimen[str_detect(tolower(cc2_pcr$Specimen1), tolower("^Näsa,svalg,saliv"))] <- "Nasal, throat, saliva"                     
cc2_pcr$specimen[str_detect(tolower(cc2_pcr$Specimen1), tolower("^Övrigt provmaterial, baktset, nasofarynx"))] <- "Nasopharyngeal culture set"
cc2_pcr$specimen[str_detect(tolower(cc2_pcr$Specimen1), tolower("^Övrigt provmaterial, Blind borste"))] <- "Respiratory, blind brush"
cc2_pcr$specimen[str_detect(tolower(cc2_pcr$Specimen1), tolower("^Övrigt provmaterial, Borste"))] <- "Respiratory, brush"            
cc2_pcr$specimen[str_detect(tolower(cc2_pcr$Specimen1), tolower("^Övrigt provmaterial, bronk"))] <- "Bronchi"              
cc2_pcr$specimen[str_detect(tolower(cc2_pcr$Specimen1), tolower("^Övrigt provmaterial, Bronksekret, borste"))] <- "Broncheal secretion"
cc2_pcr$specimen[str_detect(tolower(cc2_pcr$Specimen1), tolower("^Övrigt provmaterial, näsan"))] <- "Nasal"                
cc2_pcr$specimen[str_detect(tolower(cc2_pcr$Specimen1), tolower("^Övrigt provmaterial, Nasofarynxsekret"))] <- "Nasopharyngeal secretion"
cc2_pcr$specimen[str_detect(tolower(cc2_pcr$Specimen1), tolower("^Övrigt provmaterial, nasopharynx"))] <- "Nasopharyngeal"
cc2_pcr$specimen[str_detect(tolower(cc2_pcr$Specimen1), tolower("^Övrigt provmaterial, NPH-prov"))] <- "Nasopharyngeal sample"
cc2_pcr$specimen[str_detect(tolower(cc2_pcr$Specimen1), tolower("^Övrigt provmaterial, pleuravätska"))] <- "Pleural fluid"   
cc2_pcr$specimen[str_detect(tolower(cc2_pcr$Specimen1), tolower("^Övrigt provmaterial, sputa"))] <- "Sputum"
cc2_pcr$specimen[str_detect(tolower(cc2_pcr$Specimen1), tolower("^Övrigt provmaterial, trakealsuget"))] <- "Tracheal secretion"
cc2_pcr$specimen[str_detect(tolower(cc2_pcr$Specimen1), tolower("^Övrigt provmaterial, vä näsborre"))] <- "Nasal, left"

respiratory_specimen <- "^Bronch|^Nasal|^Nasopharyngeal|^Respiratory|^Sputum|^Throat|^Trach"
respiratory_cc2_microlab <- cc2_pcr %>% filter(str_detect(specimen, regex(respiratory_specimen, ignore_case = TRUE)))
rm(cc2_pcr)
rm(respiratory_specimen)

respiratory_viruses <- "^Adenovirus|^Bocavirus|^Coronavirus|^Enterovirus|^Influenza|^Metapneumovirus|^Parainfluenzavirus|^Rhinovirus|^RSV|^SARS-CoV-2|^SARS-CoV-2, Influenza and RSV|^Cytomegalovirus|^Epstein-Barr|^Herpes|^Morbillivirus|^Varicella zoster"
respiratory_cc2_virus <- respiratory_cc2_microlab %>% filter(str_detect(organism, regex(respiratory_viruses, ignore_case = TRUE))) %>% filter(result == "Positive")
rm(respiratory_cc2_microlab)
respiratory_cc2_virus <- respiratory_cc2_virus %>% mutate(organism_inf_new = if_else(organism %in% c("Influenza A", "Influenza B"), "Influenza A/B", organism))
multi_virus <- respiratory_cc2_virus %>% 
  filter(duplicated(slv_table_id) | duplicated(slv_table_id, fromLast = TRUE))
rm(multi_virus)

#remove if same slv_table_id has same organism, keep one
##for cross-checking if mistakenly deleted values
n_distinct(respiratory_cc2_virus$slv_table_id)
#respiratory_cc2_virus_dupes <- respiratory_cc2_virus %>% group_by(slv_table_id, organism_inf_new) %>% filter(n() > 1) %>% arrange(slv_table_id, organism_inf_new)
respiratory_cc2_virus <- respiratory_cc2_virus %>% distinct(slv_table_id, organism, .keep_all = TRUE)
n_distinct(respiratory_cc2_virus$slv_table_id)
#remove both if slv is duplicated
cc2_virus_unq <- respiratory_cc2_virus %>% group_by(slv_table_id) %>% filter(n_distinct(organism) == 1) %>% ungroup()

#filter study viruses
std_viruses <- "^Influenza|^RSV|^SARS-CoV-2"
std_cc2_virus <- cc2_virus_unq %>% filter(str_detect(organism, regex(std_viruses, ignore_case = TRUE))) 
fwrite(std_cc2_virus, file= "std_cc2_virus_unq.csv", sep = ";", quote = TRUE, bom = TRUE)

####----#####
#Post-pandemic

rm(list=ls())
setwd('C:/Data/Shared Folder/People/Rubyath/Data/Working files/Intermediate data/Period 3')
cc3_microlab_scedemo <- read_csv2("cc3_microlab_scedemo.csv")
cc3_pcr <- cc3_microlab_scedemo %>% filter(Examination == "Molekylär diagnostik")
rm(cc3_microlab_scedemo)

cc3_pcr$specimen[str_detect(tolower(cc3_pcr$Specimen1), tolower("^Övrigt provmaterial, Bronksekret"))] <- "Broncheal secretion/aspirate"
cc3_pcr$specimen[str_detect(tolower(cc3_pcr$Specimen1), tolower("^Övrigt provmaterial, Bronker"))] <- "Bronchi"
cc3_pcr$specimen[str_detect(tolower(cc3_pcr$Specimen1), tolower("^Övrigt provmaterial, Näsa"))] <- "Nasal"
cc3_pcr$specimen[str_detect(tolower(cc3_pcr$Specimen1), tolower("^Övrigt provmaterial, BAL"))] <- "Bronchoalveolar Lavage"
cc3_pcr$specimen[str_detect(tolower(cc3_pcr$Specimen1), tolower("^Övrigt provmaterial, nasalt"))] <- "Nasal"
cc3_pcr$specimen[str_detect(tolower(cc3_pcr$Specimen1), tolower("^Övrigt provmaterial, nässekret"))] <- "Nasal secretion"
cc3_pcr$specimen[str_detect(tolower(cc3_pcr$Specimen1), tolower("^Övrigt provmaterial, nösa"))] <- "Nasal"
cc3_pcr$specimen[str_detect(tolower(cc3_pcr$Specimen1), tolower("^Övrigt provmaterial, näshåla"))] <- "Nasal cavity"
cc3_pcr$specimen[str_detect(tolower(cc3_pcr$Specimen1), tolower("^Övrigt provmaterial, nf-aspirat"))] <- "Nasopharyngeal aspirate"
cc3_pcr$specimen[str_detect(tolower(cc3_pcr$Specimen1), tolower("^Övrigt provmaterial, Näsprov"))] <- "Nasal sample"
cc3_pcr$specimen[str_detect(tolower(cc3_pcr$Specimen1), tolower("^Övrigt provmaterial, nph"))] <- "Nasopharyngeal"
cc3_pcr$specimen[str_detect(tolower(cc3_pcr$Specimen1), tolower("^Övrigt provmaterial, Svalj"))] <- "Throat"
cc3_pcr$specimen[str_detect(tolower(cc3_pcr$Specimen1), tolower("^Övrigt provmaterial, nasofarynx"))] <- "Nasopharyngeal"
cc3_pcr$specimen[str_detect(tolower(cc3_pcr$Specimen1), tolower("^Övrigt provmaterial, nf"))] <- "Nasopharyngeal"
cc3_pcr$specimen[str_detect(tolower(cc3_pcr$Specimen1), tolower("^Övrigt provmaterial, track"))] <- "Tracheal"
cc3_pcr$specimen[str_detect(tolower(cc3_pcr$Specimen1), tolower("^Övrigt provmaterial, Svalgsekret"))] <- "Pharyngeal secretion"
cc3_pcr$specimen[str_detect(tolower(cc3_pcr$Specimen1), tolower("^Övrigt provmaterial, bakre svalgvägg"))] <- "Pharyngeal swab"
cc3_pcr$specimen[str_detect(tolower(cc3_pcr$Specimen1), tolower("^Övrigt provmaterial, bakre svalgväggen"))] <- "Pharyngeal swab"
cc3_pcr$specimen[str_detect(tolower(cc3_pcr$Specimen1), tolower("^Övrigt provmaterial, sputum"))] <- "Throat"
cc3_pcr$specimen[str_detect(tolower(cc3_pcr$Specimen1), tolower("^Övrigt provmaterial, svalg"))] <- "Throat"
cc3_pcr$specimen[str_detect(tolower(cc3_pcr$Specimen1), tolower("^Övrigt provmaterial, bakre svalg"))] <- "Throat, deep"
cc3_pcr$specimen[str_detect(tolower(cc3_pcr$Specimen1), tolower("^Näsa,svalg,saliv"))] <- "Nasal, throat, saliva"                     
cc3_pcr$specimen[str_detect(tolower(cc3_pcr$Specimen1), tolower("^Övrigt provmaterial, baktset, nasofarynx"))] <- "Nasopharyngeal culture set"
cc3_pcr$specimen[str_detect(tolower(cc3_pcr$Specimen1), tolower("^Övrigt provmaterial, Blind borste"))] <- "Respiratory, blind brush"
cc3_pcr$specimen[str_detect(tolower(cc3_pcr$Specimen1), tolower("^Övrigt provmaterial, Borste"))] <- "Respiratory, brush"            
cc3_pcr$specimen[str_detect(tolower(cc3_pcr$Specimen1), tolower("^Övrigt provmaterial, bronk"))] <- "Bronchi"              
cc3_pcr$specimen[str_detect(tolower(cc3_pcr$Specimen1), tolower("^Övrigt provmaterial, Bronksekret, borste"))] <- "Broncheal secretion"
cc3_pcr$specimen[str_detect(tolower(cc3_pcr$Specimen1), tolower("^Övrigt provmaterial, näsan"))] <- "Nasal"                
cc3_pcr$specimen[str_detect(tolower(cc3_pcr$Specimen1), tolower("^Övrigt provmaterial, Nasofarynxsekret"))] <- "Nasopharyngeal secretion"
cc3_pcr$specimen[str_detect(tolower(cc3_pcr$Specimen1), tolower("^Övrigt provmaterial, nasopharynx"))] <- "Nasopharyngeal"
cc3_pcr$specimen[str_detect(tolower(cc3_pcr$Specimen1), tolower("^Övrigt provmaterial, NPH-prov"))] <- "Nasopharyngeal sample"
cc3_pcr$specimen[str_detect(tolower(cc3_pcr$Specimen1), tolower("^Övrigt provmaterial, pleuravätska"))] <- "Pleural fluid"   
cc3_pcr$specimen[str_detect(tolower(cc3_pcr$Specimen1), tolower("^Övrigt provmaterial, sputa"))] <- "Sputum"
cc3_pcr$specimen[str_detect(tolower(cc3_pcr$Specimen1), tolower("^Övrigt provmaterial, trakealsuget"))] <- "Tracheal secretion"
cc3_pcr$specimen[str_detect(tolower(cc3_pcr$Specimen1), tolower("^Övrigt provmaterial, vä näsborre"))] <- "Nasal, left"
cc3_pcr$specimen[str_detect(tolower(cc3_pcr$Specimen1), tolower("^Övrigt provmaterial, hals"))] <- "Throat"
cc3_pcr$specimen[str_detect(tolower(cc3_pcr$Specimen1), tolower("^Övrigt provmaterial, luftväg"))] <-	"Respiratory tract"    
cc3_pcr$specimen[str_detect(tolower(cc3_pcr$Specimen1), tolower("^Övrigt provmaterial, luftväg, näsa"))] <-	"Respiratory tract, nasal"
cc3_pcr$specimen[str_detect(tolower(cc3_pcr$Specimen1), tolower("^Övrigt provmaterial, naso"))] <-	"Nasopharyngeal"          
cc3_pcr$specimen[str_detect(tolower(cc3_pcr$Specimen1), tolower("^Övrigt provmaterial, nasof"))] <-	"Nasopharyngeal"
cc3_pcr$specimen[str_detect(tolower(cc3_pcr$Specimen1), tolower("^Övrigt provmaterial, npha"))] <-	"Nasopharyngeal aspirate"
cc3_pcr$specimen[str_detect(tolower(cc3_pcr$Specimen1), tolower("^Övrigt provmaterial, nedre luftväg"))] <-	"Respiratory tract, lower"
cc3_pcr$specimen[str_detect(tolower(cc3_pcr$Specimen1), tolower("^Övrigt provmaterial, övre luftväg"))] <-	"Respiratory tract, upper"

respiratory_specimen <- "^Bronch|^Nasal|^Nasopharyngeal|^Respiratory|^Sputum|^Throat|^Trach"
respiratory_cc3_microlab <- cc3_pcr %>% filter(str_detect(specimen, regex(respiratory_specimen, ignore_case = TRUE)))
rm(cc3_pcr)
rm(respiratory_specimen)

respiratory_viruses <- "^Adenovirus|^Bocavirus|^Coronavirus|^Enterovirus|^Influenza|^Metapneumovirus|^Parainfluenzavirus|^Rhino-/Enterovirus|^Rhinovirus|^RSV|^SARS-CoV-2|^SARS-CoV-2, Influenza and RSV|^Cytomegalovirus|^Epstein-Barr|^Herpes|^Morbillivirus|^Varicella zoster"
respiratory_cc3_virus <- respiratory_cc3_microlab %>% filter(str_detect(organism, regex(respiratory_viruses, ignore_case = TRUE))) %>% filter(result == "Positive")
rm(respiratory_cc3_microlab)
respiratory_cc3_virus <- respiratory_cc3_virus %>% mutate(organism_inf_new = if_else(organism %in% c("Influenza A", "Influenza B"), "Influenza A/B", organism))
multi_virus <- respiratory_cc3_virus %>% 
  filter(duplicated(slv_table_id) | duplicated(slv_table_id, fromLast = TRUE))
rm(multi_virus)

#remove if same slv_table_id has same organism, keep one
##for cross-checking if mistakenly deleted values
n_distinct(respiratory_cc3_virus$slv_table_id)
#respiratory_cc3_virus_dupes <- respiratory_cc3_virus %>% group_by(slv_table_id, organism_inf_new) %>% filter(n() > 1) %>% arrange(slv_table_id, organism_inf_new)
respiratory_cc3_virus <- respiratory_cc3_virus %>% distinct(slv_table_id, organism, .keep_all = TRUE)
n_distinct(respiratory_cc3_virus$slv_table_id)
#remove both if slv is duplicated
cc3_virus_unq <- respiratory_cc3_virus %>% group_by(slv_table_id) %>% filter(n_distinct(organism) == 1) %>% ungroup()

#filter study viruses
std_viruses <- "^Influenza|^RSV|^SARS-CoV-2"
std_cc3_virus <- cc3_virus_unq %>% filter(str_detect(organism, regex(std_viruses, ignore_case = TRUE))) 
fwrite(std_cc3_virus, file= "std_cc3_virus_unq.csv", sep = ";", quote = TRUE, bom = TRUE)
