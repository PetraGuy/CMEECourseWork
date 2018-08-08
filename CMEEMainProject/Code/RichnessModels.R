# Richness modelling

rm(list = ls())
cat("\014")
library(dplyr)
library(visreg) # for visreg plots 
library(ggplot2)
library(GGally) # for ggpairs
library(factoextra)
library(FactoMineR)# these two for PCA
library(corrplot)
library(MuMIn)
library(car) # for vif
library(arm)
library(reshape)

site_data =  read.csv("../Data/CompleteSiteLevelVars.csv")
site_data = site_data[,-1]
#mean impute the missing PHI
meanPHI = round(mean(site_data$Pos_Hetero_Index, na.rm = TRUE),2)
x = site_data$Pos_Hetero_Index
x[is.na(x)] = meanPHI
site_data$Pos_Hetero_Index = x


subset_all = site_data%>%select("Site","Richness","Area_ha",
                                "Northing", "Pos_Hetero_Index","Buffer3",
                                "no_MSG", "no_NVC","sd_pH","sd_SOM","sd_LBA",
                                "sd_meandbh","sd_treedensity","area_ratio",
                                "meandbh","meanph", "meanSOM","meanLBA",
                                "meantreedensity")



colnames(subset_all) = c("Site","Richness","Area",
                         "Northing", "PHI","Buffer",
                         "no_MSG", "no_NVC","sd_pH","sd_SOM","sd_LBA",
                         "sd_meandbh","sd_TD","area_ratio",
                         "meandbh","meanph", "meanSOM","meanLBA",
                         "meanTD")


#remove the wood with the largest area
largest_area = subset_all%>%filter(Area == max(Area))%>%select(Site)   
site_data_outlier = subset_all%>%filter(Site!=largest_area)
site_data_outier = site_data_outlier[,-3] # remove area column now



subset_sd = site_data_outlier%>%select("Site","Richness",
                               "Northing", "Pos_Hetero_Index","Buffer3",
                               "no_MSG", "no_NVC","sd_pH","sd_SOM","sd_LBA",
                               "sd_meandbh","sd_treedensity","area_ratio")

colnames(subset_sd) = c("Site","Richness",
                       "Northing", "PHI","Buffer",
                       "no_MSG", "no_NVC","sd_pH",
                       "sd_SOM","sd_LBA","sd_meandbh",
                       "sd_TD","area_ratio")

subset_mean = site_data_outlier%>%select("Site","Richness",
                              "Northing", "Pos_Hetero_Index", 
                              "meandbh","meanph", "Buffer3", "meanSOM","meanLBA",
                              "meantreedensity","area_ratio", "no_NVC", "no_MSG")

colnames(subset_mean) = c("Site","Richness",
                         "Northing", "PHI", "meandbh",
                         "meanph", "Buffer", "meanSOM", "meanLBA",
                         "meanTD", "area_ratio", "no_NVC", "no_MSG")





#look at correaltions between explanatory variables

vars = site_data_outlier[,-c(1,2)]

mcor = round(cor(vars, method = "pearson", use = "na.or.complete"),2)
corrplot(mcor, type = "upper", tl.pos = "td",
         method = "number", tl.cex = 0.5, tl.col = 'black',tl.srt=45,
         order = "hclust", diag = FALSE,
         title = "pearson correlation",
         mar=c(0,0,1,0))

mcor = round(cor(vars, method = "spearman", use = "na.or.complete"),2)
corrplot(mcor, type = "upper", tl.pos = "td",
         method = "number", tl.cex = 0.5, tl.col = 'black',tl.srt=45,
         order = "hclust", diag = FALSE,
         title = "spearman correlation",
         mar=c(0,0,1,0))

# mean and standarddeviation variabes show correlations (spearman)
#above 0.5 - we know from previous plots that these are correlated, so data
# WILL be split in two.

###Means correlations...

vars = subset_mean[,-c(1,2)]

mcor = round(cor(vars, method = "pearson", use = "na.or.complete"),2)
corrplot(mcor, type = "upper", tl.pos = "td",
         method = "number", tl.cex = 0.5, tl.col = 'black',tl.srt=45,
         order = "hclust", diag = FALSE,
         title = "pearson correlation",
         mar=c(0,0,1,0))

mcor = round(cor(vars, method = "spearman", use = "na.or.complete"),2)
corrplot(mcor, type = "upper", tl.pos = "td",
         method = "number", tl.cex = 0.5, tl.col = 'black',tl.srt=45,
         order = "hclust", diag = FALSE,
         title = "spearman correlation",
         mar=c(0,0,1,0))

#correlations all <0.5, except pearson for northing and buffer = 0.55 
#and spearman for meanTD and meanDBH = -.055


##Models for means#############
# we know richness vs ph usually unimodal around .5, therefore fit to meanpH^2
data = subset_mean[,-1]
richness = subset_mean[,2]
data$meanph = (data$meanph)^2

#rescale the data
rescaled_mean_data = apply(data[,-1],2, rescale)
rescaled_mean_data = as.data.frame(cbind(richness, rescaled_mean_data))

#create the model
mod_mean = lm(richness~., data=rescaled_mean_data, na.action = "na.fail")

#have a look at the linear model
par(mfrow =c(2,2))
plot(mod_mean)

# look at vifs
vif(mod_mean)

#residuals look random, points 35,43,63 are all woods with the highest richness
#vif low 
 #get top models
models = dredge(mod_mean)
model_set = get.models(models, subset = delta<2)


#do model averaging
mean_avg_models = model.avg(model_set)

#select output data
summary = summary(mean_avg_models)
coefs =  mean_avg_models$coefficients
importance =  c(NA,(as.vector(mean_avg_models$importance[1:8])))

coef_matrix = summary$coefmat.full
coefs = coef_matrix[,1]
adj_se = coef_matrix[,3]
CI_lower =  coefs - 1.96*adj_se
CI_upper = coefs + 1.96*adj_se

output = round(as.data.frame(cbind(coefs,adj_se, CI_lower, CI_upper, importance)),2)

# make plot of the variables and CI
data = output[c(2:9),]
data = data[,c(1,3,4)]
data = t(data)
data = as.data.frame(data)
melted = melt(data)

fun_mean <- function(x){
  data = data.frame(y=mean(x),label=mean(x,na.rm=T))
  data = round(data,2)
  return(data)}


melted$variable = as.factor(melted$variable) # ps, wont plot as separate plots if x continous
ggplot(melted, (aes_string(x='variable', y='value', na.rm = TRUE)) )+
  geom_boxplot(na.rm = TRUE, width = 0)+
  stat_summary(fun.y = mean, geom="point",colour="darkred", size=3) +
  stat_summary(fun.data = fun_mean, geom="text", vjust=-0.7)+
  geom_abline(intercept = 0, slope = 0)+
  labs(y = "effect size and 95%CI",x = "effect")+
  ggtitle("model averaged results for delta <2")

