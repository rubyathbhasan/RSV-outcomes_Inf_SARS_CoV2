rm(list=ls())
setwd('C:/Data/Shared Folder/People/Rubyath/Data/Working files/R_Codes/Completed codes/LOS')
library(rmarkdown)

render("LOS_modelling_ZTNB_prepand_inf.Rmd")
render("LOS_modelling_ZTNB_pand_inf.Rmd")
render("LOS_modelling_ZTNB_pand_SARS_cov.Rmd")
render("LOS_modelling_ZTNB_postpand_inf.Rmd")
render("LOS_modelling_ZTNB_postpand_SARS_cov.Rmd")