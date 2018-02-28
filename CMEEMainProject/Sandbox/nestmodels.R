

#select best wood and do a GLM, may be extend to all 103 and look at differences is models?
#best wood = site 83
#spec_rich, calculated in MainProjRich is nest level richnesses
#spec_rich[[i]] = site i, df of richness for 16 plots, 5 nests
#cum_rich_all[[i]] = = site i, cumulative richness
library(nlme)
library(reshape)
library(glmm)
f_bestsite = spec_rich[[83]]
cf_bestsite = cum_rich_all[[83]]
f_bestsite[f_bestsite==0]=NA
cf_bestsite[cf_bestsite==0]=NA
f_bestsite = f_bestsite[!is.na]
melted_f = melt(f_bestsite)
melted_cf = melt(cf_bestsite)

ggplot(melted_f, (aes_string(x='variable', y='value', na.rm = TRUE)) )+
  geom_boxplot(na.rm = TRUE, outlier.shape = 7, outlier.size = 3) +
  geom_point(aes(x=variable,y=value,na.rm =TRUE), data = melted_cf, colour = "red") 

plots = c("plot1","plot2","plot3","plot4","plot5","plot6","plot7",
          "plot8","plot9","plot10","plot11","plot12","plot13","plot14","plot15")
cf_bestsite = cf_bestsite[-16,]
cf_bestsite$plot = plots
melted_cf = melt(cf_bestsite)


model_lme = lme(value~variable,random = ~1|plot, data = melted_cf, na.action = na.omit)

fit = predict(model_lme)
melted_cf$fit = fit


ggplot(melted_f, (aes_string(x='variable', y='value', na.rm = TRUE)) )+
  geom_boxplot(na.rm = TRUE, outlier.shape = 7, outlier.size = 3) +
  geom_point(aes(x=variable,y=value,na.rm =TRUE), data = melted_cf, colour = "black") +
  geom_line(aes(x=variable, y = fit, group = plot), colour = "red", data = melted_cf)


################################

#lme not very good fit anyway, lets tranform


model_nlme = lme(log(value)~variable,random = ~1|plot, data = melted_cf, na.action = na.omit)




