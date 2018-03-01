

#select best wood and do a GLM, may be extend to all 103 and look at differences is models?
#best wood = site 83
#spec_rich, calculated in MainProjRich is nest level richnesses
#spec_rich[[i]] = site i, df of richness for 16 plots, 5 nests
#cum_rich_all[[i]] = = site i, cumulative richness
library(nlme)
library(reshape)
#library(glmm)


# data prep
f_bestsite = spec_rich[[83]]
cf_bestsite = cum_rich_all[[83]]
f_bestsite[f_bestsite==0]=NA
cf_bestsite[cf_bestsite==0]=NA
f_bestsite = f_bestsite[!is.na]
melted_f = melt(f_bestsite)
melted_cf = melt(cf_bestsite)


# particula prep adding plots, removing plot 16 - for plot 83 empty - and remelting
plots = c("plot1","plot2","plot3","plot4","plot5","plot6","plot7",
          "plot8","plot9","plot10","plot11","plot12","plot13","plot14","plot15")
cf_bestsite = cf_bestsite[-16,]
cf_bestsite$plot = plots
melted_cf = melt(cf_bestsite)
f_bestsite = f_bestsite[-16,]
melted_f = melt(f_bestsite)

#lets add areas to data.

area = c("4","25","50","100","200") 
areas = sort(as.numeric(rep(area,15)), decreasing = FALSE)
melted_cf$area = areas
melted_f$area = areas



 model_lme = lme(value~variable,random = ~1|plot, data = melted_cf, na.action = na.omit)
# fit1 = predict(model_lme)
# melted_cf$fit = fit1

# plot of raw f data, fits to each plot, box plots of raw f

# ggplot(melted_f, (aes_string(x='variable', y='value', na.rm = TRUE)) )+
#   geom_boxplot(na.rm = TRUE, outlier.shape = 7, outlier.size = 3) +
#   geom_point(aes(x=variable,y=value,na.rm =TRUE), data = melted_cf, colour = "black") +
#   geom_line(aes(x=variable, y = fit1, group = plot), colour = "red", data = melted_cf)


################################

#lme not very good fit anyway, lets tranform - but really should x var = actual area.




#repeat plot and fits with area data because x scale will be different

# model_lme_areas = lme(value~area,random = ~1|plot, data = melted_cf, na.action = na.omit)
# 
# fit2 = predict(model_lme_areas)
# melted_cf$fit2 = fit2


# plot of raw f data, fits to each plot, box plots of raw f

melted_f$area = as.factor(melted_f$area) # ps, wont plot as separate plots if x continous
ggplot(melted_f, (aes_string(x='area', y='value', na.rm = TRUE)) )+
   geom_boxplot(na.rm = TRUE, outlier.shape = 7, outlier.size = 3)+
   ggtitle("Site 83, best wood, spread of richness per nest across all plots")

#box plots now just space out neatly, not to scale, so cant put on same graph

# ggplot(melted_cf)+
#   geom_point(aes(x=area,y=value,na.rm =TRUE), colour = "black") +
#   geom_line(aes(x=area, y = fit2, group = plot), colour = "red") +
#   ggtitle("Site 83, best wood, spread of richness per nest across all plots")
# 
# #fit still looks bad

# nlme log log models, NB log = ln

model_nlme_area_log = lme(log(value)~log(area),random = ~1|plot, data = melted_cf, na.action = na.omit)
fit3 = predict(model_nlme_area_log)
melted_cf$fit3 = fit3

#model plot
ggplot(melted_cf)+
  geom_point(aes(x=log(area),y=log(value),na.rm =TRUE), colour = "black") +
  geom_line(aes(x=log(area), y = fit3, group = plot), colour = "red")+
  geom_abline(intercept = 2.2497,slope = 0.2357, size = 2, colour = "blue")+
  ggtitle("Site 83, best wood, species area curve")+
  annotate("text",x = 2, y = 4, label  = "S = 9.9A^0.24")

#raw data cf scatter plot
ggplot(melted_cf)+
 geom_point(aes(x = area, y = value), colour = "black")+
  ggtitle("Raw data, site 83, cumulative richness across all plots with increasing area")+
  annotate("text", x = 75, y = 60, 
           label = "Change in richness due to area less than change in richness due to plot")


######


####
##fit every wood and look at coefficients
plots = c("plot1","plot2","plot3","plot4","plot5","plot6","plot7",
          "plot8","plot9","plot10","plot11","plot12","plot13","plot14","plot15","plot16")
area = c("4","25","50","100","200") 
areas = sort(as.numeric(rep(area,16)), decreasing = FALSE)

fits_all_woods = function(){
  coef_df = data.frame(matrix(ncol = 2, nrow = 0))
  
  #prepare data
  for (i in 1:103){
    #browser()
    cf_site = cum_rich_all[[i]]
    cf_site$plot = plots
    melted_cf = melt(cf_site)
    melted_cf$area = areas
    # zeros will freak this model out, make them NAs
    melted_cf[melted_cf==0]=NA
    #model
    model_nlme_area_log = lme(log(value)~log(area),random = ~1|plot, data = melted_cf, na.action = na.omit)
    
    #get coefficients
    int = model_nlme_area_log$coefficients$fixed[[1]]
    slope = model_nlme_area_log$coefficients$fixed[[2]]
    row = c(int,slope)
    coef_df = rbind(coef_df,row)
  }
  colnames(coef_df) = c("intercept","slope")
  return(coef_df)
}
#max and min slope
maxvars = fits[which(fits$slope == max(fits$slope)),]
minvars = fits[which(fits$slope == min(fits$slope)),]

ggplot()+
  geom_abline(data = fits,aes(intercept = intercept, slope = slope), colour = "pink")+
  geom_abline(data = maxvars,aes(intercept = intercept, slope = slope), colour = "red" )+
  geom_abline(data = minvars,aes(intercept = intercept, slope = slope), colour = "blue" )+
  
  scale_x_continuous(limits = c(0,5))+
  scale_y_continuous(limits = c(0,4))

max()

