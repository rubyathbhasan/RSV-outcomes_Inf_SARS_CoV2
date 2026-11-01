rm(list=ls())
setwd('C:/Data/Shared Folder/People/Rubyath/Data/Working files/R_Codes/Completed codes/Mort_30')
library(rmarkdown)

render("LR_mort_prepand_inf.Rmd")
render("LR_mort_pand_inf.Rmd")
render("LR_mort_pand_SARS-Cov-2.Rmd")
render("LR_mort_postpand_inf.Rmd")
render("LR_mort_postpand_SARS-Cov-2.Rmd")