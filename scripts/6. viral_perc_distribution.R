#Respiratory viruses in respiratory specimen with PCR

rm(list=ls())
pacman::p_load(dplyr, readr, data.table, stringr, lubridate)

setwd('C:/Data/Shared Folder/People/Rubyath/Data/Working files/Intermediate data/Period 1')
cc1_microlab_scedemo <- read_csv2("cc1_microlab_scedemo.csv")
sort(unique(cc1_microlab_scedemo$Examination))
cc1_pcr <- cc1_microlab_scedemo %>% filter(Examination == "Molekylär diagnostik")
rm(cc1_microlab_scedemo)
specimen_cc1_microlab <- cc1_pcr %>% filter(is.na(specimen))
sort(unique(specimen_cc1_microlab$Specimen1))
rm(specimen_cc1_microlab)

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
cc1_pcr$specimen[str_detect(tolower(cc1_pcr$Specimen1), tolower("^Övrigt provmaterial, sputum"))] <- "Sputum"
cc1_pcr$specimen[str_detect(tolower(cc1_pcr$Specimen1), tolower("^Övrigt provmaterial, svalg"))] <- "Throat"
cc1_pcr$specimen[str_detect(tolower(cc1_pcr$Specimen1), tolower("^Övrigt provmaterial, bakre svalg"))] <- "Throat, deep"

specimen_organism_cc1_microlab <- cc1_pcr %>% filter(is.na(specimen)) %>% group_by(specimen, Specimen1,organism) %>% tally()
#cc1_pcr$specimen[str_detect(tolower(cc1_pcr$Specimen1), tolower("^Övrigt provmaterial$"))] <- "Respiratory, other sample"
sort(unique(cc1_pcr$specimen))
respiratory_specimen <- "^Bronch|^Nasal|^Nasopharyngeal|^Respiratory|^Sputum|^Throat|^Trach"
respiratory_cc1_microlab <- cc1_pcr %>% filter(str_detect(specimen, regex(respiratory_specimen, ignore_case = TRUE)))
sort(unique(respiratory_cc1_microlab$specimen))
sum_respiratory <- respiratory_cc1_microlab %>% group_by(specimen, organism, result) %>% tally()

rm(cc1_pcr)
rm(sum_respiratory)
rm(specimen_organism_cc1_microlab)

null_result <- respiratory_cc1_microlab %>% filter(is.na(result))
#View(respiratory_cc1_microlab)
sum_respiratory <- respiratory_cc1_microlab %>% group_by(organism) %>% tally() 
sort(unique(respiratory_cc1_microlab$organism))
rm(null_result)

#respiratory_viruses <- "^Adenovirus|^Bocavirus|^Coronavirus|^Enterovirus|^Influenza|^Metapneumovirus|^Parainfluenzavirus|^Rhinovirus|^RSV|^SARS-CoV-2"
respiratory_viruses <- "^Adenovirus|^Bocavirus|^Coronavirus|^Enterovirus|^Influenza|^Metapneumovirus|^Parainfluenzavirus|^Rhinovirus|^RSV|^SARS-CoV-2|^Cytomegalovirus|^Epstein-Barr|^Herpes|^Morbillivirus|^Varicella zoster"
respiratory_cc1_virus <- respiratory_cc1_microlab %>% filter(str_detect(organism, regex(respiratory_viruses, ignore_case = TRUE)))
total_pcr_tested <- n_distinct(respiratory_cc1_virus$slv_table_id)
cc1_virus_pos <- respiratory_cc1_virus %>% filter(result == "Positive")
rm(respiratory_cc1_microlab)
rm(respiratory_specimen)
rm(respiratory_viruses)
rm(sum_respiratory)

final_cc1_microlab <- read_csv2("final_cc1_demo.csv")

viral_etiology <- cc1_virus_pos %>% summarize(any_viral_pos = n_distinct(slv_table_id)) %>% pull(any_viral_pos)/(final_cc1_microlab %>% summarise(n_epi = n_distinct(slv_table_id)) %>% pull(n_epi)) *100
viral_etiology_monthly <- cc1_virus_pos %>% mutate(year_month = floor_date(first_admission_datetime, "month")) %>% group_by(year_month) %>% summarize(any_viral_monthly = n_distinct(slv_table_id), .groups = 'drop')
arti_monthly <- final_cc1_microlab %>% mutate(year_month = floor_date(first_admission_datetime, "month")) %>% group_by(year_month) %>% summarize(all_epi_monthly = n_distinct(slv_table_id), .groups = 'drop')
viral_perc_monthly <- arti_monthly %>% full_join(viral_etiology_monthly, by = "year_month") %>% mutate(viral_percentage_monthly = round((any_viral_monthly/all_epi_monthly)*100, 2))

total_episodes <- n_distinct(final_cc1_microlab$slv_table_id)
total_viral <- n_distinct(cc1_virus_pos$slv_table_id)
#virus_distribution <- cc1_virus_pos %>% group_by(organism) %>% summarize(n_viruses = n_distinct(slv_table_id), percentage = round((n_viruses/total_episodes)*100,2), .groups = 'drop') %>% arrange(desc(percentage))
sort(unique(cc1_virus_pos$organism))
cc1_virus_pos$organism[str_detect(tolower(cc1_virus_pos$organism), tolower("^Parainfluenzavirus"))] <- "Parainfluenzavirus"
cc1_virus_pos$organism[str_detect(tolower(cc1_virus_pos$organism), tolower("^Coronavirus"))] <- "Seasonal Coronavirus"
cc1_virus_pos$organism[str_detect(tolower(cc1_virus_pos$organism), tolower("^Bocavirus|Cytomegalovirus"))] <- "Other virus"
cc1_virus_pos$organism[str_detect(tolower(cc1_virus_pos$organism), tolower("^Herpes|Epstein-Barr"))] <- "Other virus"
virus_distribution <- cc1_virus_pos %>% group_by(organism) %>% summarize(n_viruses = n_distinct(slv_table_id), percentage = round((n_viruses/total_episodes)*100,2), .groups = 'drop') %>% arrange(desc(percentage))

#fwrite(virus_distribution, file="virus_dis_pre.csv", sep = ";", quote = TRUE, bom = TRUE)
#fwrite(viral_perc_monthly, file="viral_monthly_pre.csv", sep = ";", quote = TRUE, bom = TRUE)

#Demographic
cc1_virus_age <- cc1_virus_pos %>% group_by(age_group) %>% summarize(unique_episodes = n_distinct(slv_table_id)) %>% ungroup()
class(cc1_virus_pos$age_year)
summary(cc1_virus_pos$age_year)
cc1_virus_sex <- cc1_virus_pos %>% group_by(sex) %>% summarize(unique_episodes = n_distinct(slv_table_id)) %>% ungroup()
cc1_virus_mig <- cc1_virus_pos %>% group_by(mig_background) %>% summarize(unique_episodes = n_distinct(slv_table_id)) %>% ungroup()
cc1_virus_edu <- cc1_virus_pos %>% group_by(education_level_recoded) %>% summarize(unique_episodes = n_distinct(slv_table_id)) %>% ungroup()

#Viral coinfection combination and plotting
#deduplication because same virus confirmed from different samples
cc1_virus_pos_dedup <- cc1_virus_pos %>% distinct(slv_table_id, organism, .keep_all = TRUE)
##cc1_virus_pos_dedup$organism[str_detect(tolower(cc1_virus_pos_dedup$organism), tolower("^Parainfluenzavirus"))] <- "Parainfluenzavirus"
##cc1_virus_pos_dedup$organism[str_detect(tolower(cc1_virus_pos_dedup$organism), tolower("^Coronavirus"))] <- "Seasonal Coronavirus"
##cc1_virus_pos_dedup$organism[str_detect(tolower(cc1_virus_pos_dedup$organism), tolower("^Bocavirus|Cytomegalovirus"))] <- "Other virus"
##cc1_virus_pos_dedup$organism[str_detect(tolower(cc1_virus_pos_dedup$organism), tolower("^Herpes|Epstein-Barr"))] <- "Other virus"
viral_coinf <- cc1_virus_pos_dedup %>% arrange(slv_table_id, organism) %>% group_by(slv_table_id) %>% summarise(infection_count = n(), combination = paste(organism, collapse = "+")) %>% ungroup()
viral_coinf <- viral_coinf %>% count(combination, name = "frequency") %>% arrange(desc(frequency))
viral_coinf
#fwrite(viral_coinf, file = "viral_coinf_pre.csv", sep = ";", quote = TRUE, bom = TRUE)
pacman::p_load(tidyr, ComplexUpset, ggplot2, patchwork)
plot_viral_coin <- cc1_virus_pos_dedup %>% group_by(slv_table_id) %>% mutate(inf_count = n()) %>% filter(inf_count > 1) %>% mutate(virus_category = organism) %>% ungroup()
valid_epi <- plot_viral_coin %>% group_by(slv_table_id) %>% summarise(is.single = max(inf_count) == 1, combination = paste(sort(organism), collapse = "+")) %>% group_by(combination) %>% mutate(combo_freq = n()) %>% ungroup() %>% filter(combo_freq >= 1) %>% pull(slv_table_id)
plot_viral_coin <- plot_viral_coin %>% filter(slv_table_id %in% valid_epi) %>% mutate(present = 1) %>% select(slv_table_id, virus_category, present) %>% pivot_wider(names_from = virus_category, values_from = present, values_fill = list(present = 0)) %>% select(-slv_table_id) %>% as.data.frame()
viral_coin <- upset(plot_viral_coin, intersect = colnames(plot_viral_coin), set_sizes = FALSE, name = "Viral coinfection combinations", sort_intersections_by =  'cardinality', sort_intersections = 'descending', base_annotations = list('Number of episodes' = intersection_size() + scale_y_continuous(limits = c(0,10), breaks = seq(0, 10, by = 2)) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(color = "black")))) + patchwork::plot_annotation(title = "Combination of virus coinfections, pre-pandemic period", theme = theme(plot.title =  element_text(hjust = 0.5, size = 16, face = "bold")))
#ggsave("viral coinfection combinations_pre.png", plot = viral_coin, width = 10, height = 8, units = "in")

####---------########
#Pandemic

rm(list=ls())
pacman::p_load(dplyr, readr, data.table, stringr, lubridate)

setwd('C:/Data/Shared Folder/People/Rubyath/Data/Working files/Intermediate data/Period 2')
cc2_microlab_recoded <- read_csv2("cc2_microlab_scedemo.csv")
sort(unique(cc2_microlab_recoded$Examination))
cc2_pcr <- cc2_microlab_recoded %>% filter(Examination %in% c("Molekylär diagnostik", "Molekylär diagnostik, öppenvård"))
rm(cc2_microlab_recoded)
specimen_cc2_microlab <- cc2_pcr %>% filter(is.na(specimen))
sort(unique(specimen_cc2_microlab$Specimen1))
rm(specimen_cc2_microlab)

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
cc2_pcr$specimen[str_detect(tolower(cc2_pcr$Specimen1), tolower("^Övrigt provmaterial, sputum"))] <- "Sputum"
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

specimen_organism_cc2_microlab <- cc2_pcr %>% filter(is.na(specimen)) %>% group_by(specimen, Specimen1,organism) %>% tally()
#cc2_pcr$specimen[str_detect(tolower(cc2_pcr$Specimen1), tolower("^Övrigt provmaterial$"))] <- "Respiratory, other sample"
#cc2_pcr$specimen[str_detect(tolower(cc2_pcr$Specimen1), tolower("^Övrigt provmaterial, \\?"))] <- "Respiratory, other sample"
sort(unique(cc2_pcr$specimen))
respiratory_specimen <- "^Bronch|^Nasal|^Nasopharyngeal|^Respiratory|^Sputum|^Throat|^Trach"
respiratory_cc2_microlab <- cc2_pcr %>% filter(str_detect(specimen, regex(respiratory_specimen, ignore_case = TRUE)))
sort(unique(respiratory_cc2_microlab$specimen))
sum_respiratory <- respiratory_cc2_microlab %>% group_by(specimen, organism, result) %>% tally()

rm(cc2_pcr)
rm(sum_respiratory)
rm(specimen_organism_cc2_microlab)

null_result <- respiratory_cc2_microlab %>% filter(is.na(result))
#Möjligt falskt positiv. ##Misstänkt positiv. Nytt prov ska tas.#
#NA results were checked
sum_respiratory <- respiratory_cc2_microlab %>% group_by(organism) %>% tally() 
sort(unique(respiratory_cc2_microlab$organism))
rm(null_result)

#respiratory_viruses <- "^Adenovirus|^Bocavirus|^Coronavirus|^Enterovirus|^Influenza|^Metapneumovirus|^Parainfluenzavirus|^Rhinovirus|^RSV|^SARS-CoV-2|^SARS-CoV-2, Influenza and RSV"
respiratory_viruses <- "^Adenovirus|^Bocavirus|^Coronavirus|^Enterovirus|^Influenza|^Metapneumovirus|^Parainfluenzavirus|^Rhinovirus|^RSV|^SARS-CoV-2|^SARS-CoV-2, Influenza and RSV|^Cytomegalovirus|^Epstein-Barr|^Herpes|^Morbillivirus|^Varicella zoster"
respiratory_cc2_virus <- respiratory_cc2_microlab %>% filter(str_detect(organism, regex(respiratory_viruses, ignore_case = TRUE)))
total_pcr_tested <- n_distinct(respiratory_cc2_virus$slv_table_id)
cc2_virus_pos <- respiratory_cc2_virus %>% filter(result == "Positive")
rm(respiratory_cc2_microlab)
rm(respiratory_specimen)
rm(respiratory_viruses)
rm(sum_respiratory)

final_cc2_microlab <- read_csv2("final_cc2_demo.csv")

viral_etiology <- cc2_virus_pos %>% summarize(any_viral_pos = n_distinct(slv_table_id)) %>% pull(any_viral_pos)/(final_cc2_microlab %>% summarise(n_epi = n_distinct(slv_table_id)) %>% pull(n_epi)) *100
viral_etiology_monthly <- cc2_virus_pos %>% mutate(year_month = floor_date(first_admission_datetime, "month")) %>% group_by(year_month) %>% summarize(any_viral_monthly = n_distinct(slv_table_id), .groups = 'drop')
arti_monthly <- final_cc2_microlab %>% mutate(year_month = floor_date(first_admission_datetime, "month")) %>% group_by(year_month) %>% summarize(all_epi_monthly = n_distinct(slv_table_id), .groups = 'drop')
viral_perc_monthly <- arti_monthly %>% full_join(viral_etiology_monthly, by = "year_month") %>% mutate(viral_percentage_monthly = round((any_viral_monthly/all_epi_monthly)*100, 2))

total_episodes <- n_distinct(final_cc2_microlab$slv_table_id)
total_viral <- n_distinct(cc2_virus_pos$slv_table_id)
#virus_distribution <- cc2_virus_pos %>% group_by(organism) %>% summarize(n_viruses = n_distinct(slv_table_id), percentage = round((n_viruses/total_episodes)*100,2), .groups = 'drop') %>% arrange(desc(percentage))
sort(unique(cc2_virus_pos$organism))
cc2_virus_pos$organism[str_detect(tolower(cc2_virus_pos$organism), tolower("^Parainfluenzavirus"))] <- "Parainfluenzavirus"
cc2_virus_pos$organism[str_detect(tolower(cc2_virus_pos$organism), tolower("^Coronavirus"))] <- "Seasonal Coronavirus"
cc2_virus_pos$organism[str_detect(tolower(cc2_virus_pos$organism), tolower("^Bocavirus|Cytomegalovirus"))] <- "Other virus"
cc2_virus_pos$organism[str_detect(tolower(cc2_virus_pos$organism), tolower("^Herpes|Epstein-Barr"))] <- "Other virus"
cc2_virus_pos$organism[str_detect(tolower(cc2_virus_pos$organism), tolower("^Varicella"))] <- "Other virus"
virus_distribution <- cc2_virus_pos %>% group_by(organism) %>% summarize(n_viruses = n_distinct(slv_table_id), percentage = round((n_viruses/total_episodes)*100,2), .groups = 'drop') %>% arrange(desc(percentage))

#fwrite(virus_distribution, file="virus_dis_pand.csv", sep = ";", quote = TRUE, bom = TRUE)
#fwrite(viral_perc_monthly, file="viral_monthly_pand.csv", sep = ";", quote = TRUE, bom = TRUE)

#Demographic
cc2_virus_age <- cc2_virus_pos %>% group_by(age_group) %>% summarize(unique_episodes = n_distinct(slv_table_id)) %>% ungroup()
class(cc2_virus_pos$age_year)
summary(cc2_virus_pos$age_year)
cc2_virus_sex <- cc2_virus_pos %>% group_by(sex) %>% summarize(unique_episodes = n_distinct(slv_table_id)) %>% ungroup()
cc2_virus_mig <- cc2_virus_pos %>% group_by(mig_background) %>% summarize(unique_episodes = n_distinct(slv_table_id)) %>% ungroup()
cc2_virus_edu <- cc2_virus_pos %>% group_by(education_level_recoded) %>% summarize(unique_episodes = n_distinct(slv_table_id)) %>% ungroup()

#Viral coinfection combination and plotting
cc2_virus_pos_dedup <- cc2_virus_pos %>% distinct(slv_table_id, organism, .keep_all = TRUE)
viral_coinf <- cc2_virus_pos_dedup %>% arrange(slv_table_id, organism) %>% group_by(slv_table_id) %>% summarise(infection_count = n(), combination = paste(organism, collapse = "+")) %>% ungroup()
viral_coinf <- viral_coinf %>% count(combination, name = "frequency") %>% arrange(desc(frequency))
viral_coinf
#fwrite(viral_coinf, file = "viral_coinf_pand.csv", sep = ";", quote = TRUE, bom = TRUE)
pacman::p_load(tidyr, ComplexUpset, ggplot2)
plot_viral_coin <- cc2_virus_pos_dedup %>% group_by(slv_table_id) %>% mutate(inf_count = n()) %>% filter(inf_count > 1) %>% mutate(virus_category = organism) %>% ungroup()
valid_epi <- plot_viral_coin %>% group_by(slv_table_id) %>% summarise(is.single = max(inf_count) == 1, combination = paste(sort(organism), collapse = "+")) %>% group_by(combination) %>% mutate(combo_freq = n()) %>% ungroup() %>% filter(combo_freq >= 1) %>% pull(slv_table_id)
plot_viral_coin <- plot_viral_coin %>% filter(slv_table_id %in% valid_epi) %>% mutate(present = 1) %>% select(slv_table_id, virus_category, present) %>% pivot_wider(names_from = virus_category, values_from = present, values_fill = list(present = 0)) %>% select(-slv_table_id) %>% as.data.frame()
viral_coin <- upset(plot_viral_coin, intersect = colnames(plot_viral_coin), set_sizes = FALSE, name = "Viral coinfection combinations", sort_intersections_by =  'cardinality', sort_intersections = 'descending', base_annotations = list('Number of episodes' = intersection_size() + scale_y_continuous(limits = c(0,10), breaks = seq(0, 10, by = 2)) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(color = "black")))) + patchwork::plot_annotation(title = "Combination of virus coinfections, pandemic period", theme = theme(plot.title =  element_text(hjust = 0.5, size = 16, face = "bold")))
#ggsave("viral coinfection combinations_pand.png", plot = viral_coin, width = 10, height = 8, units = "in")

####-------#####
#Post-pandemic

rm(list=ls())
pacman::p_load(dplyr, readr, data.table, stringr, lubridate)

setwd('C:/Data/Shared Folder/People/Rubyath/Data/Working files/Intermediate data/Period 3')
cc3_microlab_recoded <- read_csv2("cc3_microlab_scedemo.csv")

#Identified a encoding problem in two columns
#cc3_microlab_recoded$fodelselandgrupp <- iconv(cc3_microlab_recoded$fodelselandgrupp, from = "latin1", to = "UTF-8")
#problem_columns <- sapply(cc3_microlab_recoded, function(col) {if(is.character(col) || is.factor(col)) {any(!validUTF8(as.character(col)), na.rm = TRUE)} else {FALSE}})
#problem_rows <- which(!validUTF8(cc3_microlab_recoded$fodelselandgrupp))
#table(Encoding(cc3_microlab_recoded$fodelselandgrupp))
#sort(unique(cc3_microlab_recoded$fodelselandgrupp))

sort(unique(cc3_microlab_recoded$Examination))
cc3_pcr <- cc3_microlab_recoded %>% filter(Examination == "Molekylär diagnostik")
rm(cc3_microlab_recoded)
specimen_cc3_microlab <- cc3_pcr %>% filter(is.na(specimen))
sort(unique(specimen_cc3_microlab$Specimen1))
rm(specimen_cc3_microlab)

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
cc3_pcr$specimen[str_detect(tolower(cc3_pcr$Specimen1), tolower("^Övrigt provmaterial, sputum"))] <- "Sputum"
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

specimen_organism_cc3_microlab <- cc3_pcr %>% filter(is.na(specimen)) %>% group_by(specimen, Specimen1,organism) %>% tally()
#cc3_pcr$specimen[str_detect(tolower(cc3_pcr$Specimen1), tolower("^Övrigt provmaterial$"))] <- "Respiratory, other sample"
sort(unique(cc3_pcr$specimen))
respiratory_specimen <- "^Bronch|^Nasal|^Nasopharyngeal|^Respiratory|^Sputum|^Throat|^Trach"
respiratory_cc3_microlab <- cc3_pcr %>% filter(str_detect(specimen, regex(respiratory_specimen, ignore_case = TRUE)))
sort(unique(respiratory_cc3_microlab$specimen))
sum_respiratory <- respiratory_cc3_microlab %>% group_by(specimen, organism, result) %>% tally()

rm(cc3_pcr)
rm(sum_respiratory)
rm(specimen_organism_cc3_microlab)

null_result <- respiratory_cc3_microlab %>% filter(is.na(result))
#Möjligt falskt positiv. ##Misstänkt positiv. Nytt prov ska tas.# all are checked and found the updated result
#NA results were checked
sum_respiratory <- respiratory_cc3_microlab %>% group_by(organism) %>% tally() 
sort(unique(respiratory_cc3_microlab$organism))
rm(null_result)

#respiratory_viruses <- "^Adenovirus|^Bocavirus|^Coronavirus|^Enterovirus|^Influenza|^Metapneumovirus|^Parainfluenzavirus|^Rhino-/Enterovirus|^Rhinovirus|^RSV|^SARS-CoV-2|^SARS-CoV-2, Influenza and RSV"
respiratory_viruses <- "^Adenovirus|^Bocavirus|^Coronavirus|^Enterovirus|^Influenza|^Metapneumovirus|^Parainfluenzavirus|^Rhino-/Enterovirus|^Rhinovirus|^RSV|^SARS-CoV-2|^SARS-CoV-2, Influenza and RSV|^Cytomegalovirus|^Epstein-Barr|^Herpes|^Morbillivirus|^Varicella zoster"
respiratory_cc3_virus <- respiratory_cc3_microlab %>% filter(str_detect(organism, regex(respiratory_viruses, ignore_case = TRUE)))
total_pcr_tested <- n_distinct(respiratory_cc3_virus$slv_table_id)
cc3_virus_pos <- respiratory_cc3_virus %>% filter(result == "Positive")
rm(respiratory_cc3_microlab)
rm(respiratory_specimen)
rm(respiratory_viruses)
rm(sum_respiratory)

final_cc3_microlab <- read_csv2("final_cc3_demo.csv")

viral_etiology <- cc3_virus_pos %>% summarize(any_viral_pos = n_distinct(slv_table_id)) %>% pull(any_viral_pos)/(final_cc3_microlab %>% summarise(n_epi = n_distinct(slv_table_id)) %>% pull(n_epi)) *100
viral_etiology_monthly <- cc3_virus_pos %>% mutate(year_month = floor_date(first_admission_datetime, "month")) %>% group_by(year_month) %>% summarize(any_viral_monthly = n_distinct(slv_table_id), .groups = 'drop')
arti_monthly <- final_cc3_microlab %>% mutate(year_month = floor_date(first_admission_datetime, "month")) %>% group_by(year_month) %>% summarize(all_epi_monthly = n_distinct(slv_table_id), .groups = 'drop')
viral_perc_monthly <- arti_monthly %>% full_join(viral_etiology_monthly, by = "year_month") %>% mutate(viral_percentage_monthly = round((any_viral_monthly/all_epi_monthly)*100, 2))

total_episodes <- n_distinct(final_cc3_microlab$slv_table_id)
total_viral <- n_distinct(cc3_virus_pos$slv_table_id)
#virus_distribution <- cc3_virus_pos %>% group_by(organism) %>% summarize(n_viruses = n_distinct(slv_table_id), percentage = round((n_viruses/total_episodes)*100,2), .groups = 'drop') %>% arrange(desc(percentage))
sort(unique(cc3_virus_pos$organism))
cc3_virus_pos <- cc3_virus_pos %>% filter(organism != "Enterovirus D68")
cc3_virus_pos$organism[str_detect(tolower(cc3_virus_pos$organism), tolower("^Parainfluenzavirus"))] <- "Parainfluenzavirus"
cc3_virus_pos$organism[str_detect(tolower(cc3_virus_pos$organism), tolower("^Coronavirus"))] <- "Seasonal Coronavirus"
cc3_virus_pos$organism[str_detect(tolower(cc3_virus_pos$organism), tolower("^Bocavirus|Cytomegalovirus"))] <- "Other virus"
cc3_virus_pos$organism[str_detect(tolower(cc3_virus_pos$organism), tolower("^Herpes|Epstein-Barr"))] <- "Other virus"
virus_distribution <- cc3_virus_pos %>% group_by(organism) %>% summarize(n_viruses = n_distinct(slv_table_id), percentage = round((n_viruses/total_episodes)*100,2), .groups = 'drop') %>% arrange(desc(percentage))

#fwrite(virus_distribution, file="virus_dis_post.csv", sep = ";", quote = TRUE, bom = TRUE)
#fwrite(viral_perc_monthly, file="viral_monthly_post.csv", sep = ";", quote = TRUE, bom = TRUE)

#Demographic
cc3_virus_age <- cc3_virus_pos %>% group_by(age_group) %>% summarize(unique_episodes = n_distinct(slv_table_id)) %>% ungroup()
class(cc3_virus_pos$age_year)
summary(cc3_virus_pos$age_year)
cc3_virus_sex <- cc3_virus_pos %>% group_by(sex) %>% summarize(unique_episodes = n_distinct(slv_table_id)) %>% ungroup()
cc3_virus_mig <- cc3_virus_pos %>% group_by(mig_background) %>% summarize(unique_episodes = n_distinct(slv_table_id)) %>% ungroup()
cc3_virus_edu <- cc3_virus_pos %>% group_by(education_level_recoded) %>% summarize(unique_episodes = n_distinct(slv_table_id)) %>% ungroup()

#Viral coinfection combination and plotting
cc3_virus_pos_dedup <- cc3_virus_pos %>% distinct(slv_table_id, organism, .keep_all = TRUE)
viral_coinf <- cc3_virus_pos_dedup %>% arrange(slv_table_id, organism) %>% group_by(slv_table_id) %>% summarise(infection_count = n(), combination = paste(organism, collapse = "+")) %>% ungroup()
viral_coinf <- viral_coinf %>% count(combination, name = "frequency") %>% arrange(desc(frequency))
viral_coinf
#fwrite(viral_coinf, file = "viral_coinf_post.csv", sep = ";", quote = TRUE, bom = TRUE)
pacman::p_load(tidyr, ComplexUpset, ggplot2)
plot_viral_coin <- cc3_virus_pos_dedup %>% group_by(slv_table_id) %>% mutate(inf_count = n()) %>% filter(inf_count > 1) %>% mutate(virus_category = organism) %>% ungroup()
valid_epi <- plot_viral_coin %>% group_by(slv_table_id) %>% summarise(is.single = max(inf_count) == 1, combination = paste(sort(organism), collapse = "+")) %>% group_by(combination) %>% mutate(combo_freq = n()) %>% ungroup() %>% filter(combo_freq >= 1) %>% pull(slv_table_id)
plot_viral_coin <- plot_viral_coin %>% filter(slv_table_id %in% valid_epi) %>% mutate(present = 1) %>% select(slv_table_id, virus_category, present) %>% pivot_wider(names_from = virus_category, values_from = present, values_fill = list(present = 0)) %>% select(-slv_table_id) %>% as.data.frame()
viral_coin <- upset(plot_viral_coin, intersect = colnames(plot_viral_coin), set_sizes = FALSE, name = "Viral coinfection combinations", sort_intersections_by =  'cardinality', sort_intersections = 'descending', base_annotations = list('Number of episodes' = intersection_size() + scale_y_continuous(limits = c(0,10), breaks = seq(0, 10, by = 2)) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(color = "black")))) + patchwork::plot_annotation(title = "Combination of virus coinfections, post-pandemic period", theme = theme(plot.title =  element_text(hjust = 0.5, size = 16, face = "bold")))
#ggsave("viral coinfection combinations_post.png", plot = viral_coin, width = 10, height = 8, units = "in")


#Joining the trends for three periods

rm(list=ls())
pacman::p_load(dplyr, readr, data.table, stringr, lubridate)

setwd('C:/Data/Shared Folder/People/Rubyath/Data/Working files/Viral_etio_dis')
viral_perc_monthly_cc1 <- read_delim("viral_monthly_pre.csv", delim = ";", locale = locale(decimal_mark = "."))
viral_perc_monthly_cc2 <- read_delim("viral_monthly_pand.csv", delim = ";", locale = locale(decimal_mark = "."))
viral_perc_monthly_cc3 <- read_delim("viral_monthly_post.csv", delim = ";", locale = locale(decimal_mark = "."))

viral_perc_monthly <- bind_rows(viral_perc_monthly_cc1, viral_perc_monthly_cc2, viral_perc_monthly_cc3)
#fwrite(viral_perc_monthly, file = "viral_perc_monthly.csv", sep = ";", bom = TRUE)

#Plotting the trend
rm(list = ls())
gc()
pacman::p_load(readr, ggplot2, scales, patchwork)

setwd('C:/Data/Shared Folder/People/Rubyath/Data/Working files/Viral_etio_dis')
viral_perc_monthly <- read_delim("viral_perc_monthly.csv", delim = ";")
viral_perc_monthly$year_month <- as.Date(viral_perc_monthly$year_month)

Pand_start <- as.Date("2020-03-01")
Pand_end <- as.Date("2022-04-01")

format_dates <- function(dates){month_num <- format(dates, "%m"); ifelse(month_num == "01", format(dates, "%B %Y"), format(dates, "%B"))}
date_limits <- as.Date(c("2016-12-01", "2025-01-01"))

plot_bars <- ggplot(viral_perc_monthly, aes(x=year_month, y=all_epi_monthly)) + 
  annotate("rect", xmin = Pand_start, xmax = Pand_end, ymin = -Inf, ymax = Inf, fill= "gray80", alpha= 0.5) +
  annotate("text", x = as.Date("2021-02-01"), y = Inf, vjust = 1.5, label = "Pandemic period", size = 3, color = "black", fontface = "italic") +
  geom_col(fill="gray50", width = 15) + 
  scale_y_continuous(labels = scales::comma, limits = c(0, 2500), expand = c(0,0))+ 
  scale_x_date(date_breaks = "2 months", labels =  format_dates, expand = c(0.01,0), limits = date_limits) +
  labs(y = "No. of total hospitalization episodes") +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5, size = 14, face = "plain", color= "black"), 
  axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 9, color = "black"), 
  axis.text.y = element_text(color = "black"), 
  axis.title.y = element_text(color = "black", size = 10), 
  panel.grid = element_blank(), panel.border = element_blank(),
  axis.line = element_line(color = "black"),
  axis.title.x = element_blank())
plot_bars

plot_lines <- ggplot(viral_perc_monthly, aes(x = year_month, y = viral_percentage_monthly)) +
  annotate("rect", xmin = Pand_start, xmax = Pand_end, ymin = -Inf, ymax = Inf, fill= "gray80", alpha= 0.5) +
  annotate("text", x = as.Date("2021-02-01"), y = Inf, vjust = 1.5, label = "Pandemic period", size = 3, color = "black", fontface = "italic") +
  geom_line(color = "black", linewidth = 1.5) +
  scale_y_continuous(labels = scales::label_percent(scale = 1), limits = c(0, 100), expand = c(0,0)) +
  scale_x_date(date_breaks = "2 months", labels =  format_dates, expand = c(0.01,0), limits = date_limits) +
  labs(x = "Year-month") +
  labs(y = "% of episodes with viral etiology") +
  theme_bw() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 9, color = "black"),
  axis.text.y = element_text(color = "black"), 
  axis.title.y = element_text(color = "black", size = 10),
  panel.grid = element_blank(),
  panel.border = element_blank(),
  axis.line = element_line(color = "black"),
  axis.title.x = element_text(colour = "black", size = 10, margin = margin(t = 10)))
plot_lines

Viral_prev_combined <- (plot_bars/plot_lines) + plot_annotation(title = "Trends in acute RTI hospitalization and proportion of viral episodes, 2017-24", tag_levels = 'a', tag_suffix = ')', theme = theme(plot.title = element_text(hjust = 0.5, size = 12, face = "bold", colour = "black"), plot.tag = element_text(face = "plain", margin = margin(r = 25, b = 25))))
Viral_prev_combined
#ggsave("trend of hospitalization and viral episodes.png", plot = Viral_prev_combined, width = 10, height = 8, units = "in")

##------#####
#episodes with any microbial testing info

setwd('C:/Data/Shared Folder/People/Rubyath/Data/Working files/Intermediate data/Period 1/New folder')
final_cc1_microlab <- read_csv2("cc1_micro_lab_without_NA_NULL_dtf.csv")
n_distinct(final_cc1_microlab$slv_table_id)

setwd('C:/Data/Shared Folder/People/Rubyath/Data/Working files/Intermediate data/Period 2/New folder')
final_cc2_microlab <- read_csv2("cc2_micro_lab_without_NA_NULL_dtf.csv")
n_distinct(final_cc2_microlab$slv_table_id)

setwd('C:/Data/Shared Folder/People/Rubyath/Data/Working files/Intermediate data/Period 3/New folder')
final_cc3_microlab <- read_csv2("cc3_micro_lab_without_NA_NULL_dtf.csv")
n_distinct(final_cc3_microlab$slv_table_id)
