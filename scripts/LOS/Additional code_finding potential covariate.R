#Finding any other covariate
sum(is.na(std_cc2$dementia))
std_cc2 <- std_cc2 %>% mutate(dementia = if_else(is.na(dementia), 0, dementia))
suspect_var <- std_cc2$dementia
hunt <- data.frame(Residuals = pearson_res, Svar = suspect_var)
library(ggplot2)
ggplot(hunt, aes(x = as.factor(Svar), y = Residuals)) + geom_boxplot(outlier.alpha = 0.5) + geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 1) + labs(title = "hunting", x = "Suspected var", y = "pearson res") + theme_minimal()
#for continuous variable, ggplot(hunt, aes(x = Svar, y = Residuals)) + geom_point(alpha = 0.4, color = "gray") + geom_smooth(method = "loess", color = "blue", linewidth = 1, se = FALSE) + geom_hline(yintercept = 0, linetype = "dashed", color = "red") + labs(title = "hunting", x = "Suspected var", y = "pearson res") + theme_minimal()

std_cc2$dementia <- as.factor(std_cc2$dementia)
cc2_ztnb_advanced4 <- vglm(days_of_stay ~ organism_inf_new + ns(age_year, 4) + sex + education_level_recoded + mig_background + dementia + cci_cat + bac_coinf, family = posnegbinomial(zero = NULL), data = std_cc2)
c(Advanced2 = AIC(cc2_ztnb_advanced2), Advanced3 = AIC(cc2_ztnb_advanced3), Advanced4 = AIC(cc2_ztnb_advanced4))