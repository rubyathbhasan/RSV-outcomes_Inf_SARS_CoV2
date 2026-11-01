rm(list=ls())
setwd('C:/Data/Shared Folder/People/Rubyath/Data/Working files/R_Codes/Completed codes/ICU_adm')
library(rmarkdown)

render("LR_icuad_prepand_inf.Rmd")
render("LR_icuad_pand_inf.Rmd")
render("LR_icuad_pand_SARS-Cov-2.Rmd")
render("LR_icuad_postpand_inf.Rmd")
render("LR_icuad_postpand_SARS-Cov-2.Rmd")