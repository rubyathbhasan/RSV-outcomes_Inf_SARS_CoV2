rm(list=ls())
setwd('C:/Data/Shared Folder/People/Rubyath/Data/Working files/R_Codes/Completed codes/Mec_vent')
library(rmarkdown)

render("LR_mecvent_prepand_inf.Rmd")
render("LR_mecvent_pand_inf.Rmd")
render("LR_mecvent_pand_SARS-Cov-2.Rmd")
render("LR_mecvent_postpand_inf.Rmd")
render("LR_mecvent_postpand_SARS-Cov-2.Rmd")