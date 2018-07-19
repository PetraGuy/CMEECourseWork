

#this was to get the standard deviation of the random intercepts in the nest fits
setwd("~/Documents/CMEECourseWork/CMEEMainProject/Code")
rm(list = ls())
cat("\014")
library(reshape)
library(nlme)


plots = c("plot1","plot2","plot3","plot4","plot5","plot6","plot7",
          "plot8","plot9","plot10","plot11","plot12","plot13","plot14","plot15","plot16")
area = c("4","25","50","100","200") 
areas = sort(as.numeric(rep(area,16)), decreasing = FALSE)
plot_cum_richess = readRDS("CumulateveRichness.RDS")

fits_all_woods = function(){
  coef_df = data.frame()
  for (i in 1:103){
    #browser()
    cf_site = plot_cum_richess[[i]]
    cf_site$plot = plots
    melted_cf = melt(cf_site)
    melted_cf[melted_cf == 0] = NA # if a site has missing plots they will be zeros
    melted_cf$area = areas
    model = lme(log(value)~log(area),random = ~1|plot, data = melted_cf, na.action = na.omit)
    #get coefficients
    int = round(model$coefficients$fixed[[1]],digits = 2)
    slope = round(model$coefficients$fixed[[2]],digits = 2)
    p_slope = round(summary(model)$tTable[2,5],digits=4)
    sd = round(as.numeric(VarCorr(model)[1,2]),digits = 2)
    row = as.numeric(c(i,int,slope, p_slope,sd))
    coef_df = as.data.frame(rbind(coef_df,row))
  }
  colnames(coef_df) = c("Site","intercept","slope","p_slope","sd_intercept")
  return(coef_df)
}
MixedModelNests = fits_all_woods()

saveRDS(MixedModelNests,"nest_mixed_model_fits.RDS")