#!/usr/bin/env Rscript

# Starting again with Sparrows in a more logical order
rm(list = ls())
library(ggplot2)
library(gridExtra) # for multiple plots
library(dplyr) #not used yet
library(moments) # for skewness
library(MASS)

###   Import and normalise Data #####
SparrowsData = as.data.frame(read.table("../Data/SparrowSize.txt", header = TRUE))
NumericCols = sapply(SparrowsData, is.numeric)
ScaledData = sapply(SparrowsData[,3:6], scale)
SparrowsNormalised = cbind(SparrowsData[,1:2], ScaledData, SparrowsData[,7:8])

### function to calcualte the mode ####
estimate_mode <- function(x) {
  
  d <- density(x, na.rm = TRUE)
  d$x[which.max(d$y)]
}
#Fuction to caluclate the standard error

StandError =  function(data){
  data = na.omit(data)
  SE = sd(data, na.rm = TRUE)/sqrt(length(data))
}

###     Sats for unnormalised data   ####

SparrowMeans = round(colMeans(as.data.frame(SparrowsData[,4:6]), na.rm = TRUE), digits = 2)


Variances =  sapply(SparrowsData[,NumericCols], var, na.rm = TRUE)
SparrowVars = round(Variances, digits = 2)[4:6]

Sparrowsd = round(sqrt(Variances[4:6]), digits = 2)

SparrowMedians = sapply(SparrowsData[,4:6], median, na.rm = TRUE)

SparrowModes = sapply(SparrowsData[,4:6], estimate_mode)
 
Skews = sapply(SparrowsData[,NumericCols], skewness, na.rm = TRUE)

SparrowSkews = round(Skews[4:6], digits = 2)


  
SparrowBillSE = round(with(SparrowsData, tapply(Bill, Year, StandError)), digits = 2)
SparrowWingSE = round(with(SparrowsData, tapply(Wing, Year, StandError)), digits =2)
SparrowMassSE = round(with(SparrowsData, tapply(Mass, Year, StandError)), digits = 2)
SparrowSE = rbind(SparrowBillSE,SparrowWingSE,SparrowMassSE)
rownames(SparrowSE) = c("Bill", "Wing", "Mass")

NormTests = sapply(SparrowsData[,4:6], shapiro.test)

SparrowStats = rbind(SparrowMeans, SparrowMedians, SparrowModes, SparrowVars, Sparrowsd, SparrowSkews, NormTests)
                 
colnames(SparrowStats) = c("Bill", "Wing", "Mass")
#rownames(SparrowStats) = c("Mean", "Variance", "StandDev", "Skewness")

###### Plots for un normalised data  ########################
dev.off()
plot1 <- ggplot(data=subset(SparrowsData, !is.na(Wing)), aes(Wing))+
  geom_histogram(bins = 25)+
  geom_vline(data=SparrowsData, aes(xintercept =  SparrowMeans[[2]]))+
  geom_density()
  
plot1a <- ggplot(data=subset(SparrowsData, !is.na(Wing)), aes(x=1, y = Wing))+
  geom_boxplot()+
  coord_flip()

plot2 <- ggplot(data = subset(SparrowsData, !is.na(Mass)) , aes(Mass))+
  geom_histogram(bins = 25)+
  geom_vline(data=SparrowsData, aes(xintercept =  SparrowMeans[[3]]))
geom_density()

plot2a <- ggplot(data=subset(SparrowsData, !is.na(Mass)), aes(x=1, y = Mass))+
  geom_boxplot()+
  coord_flip()

plot3 <- ggplot(data=subset(SparrowsData, !is.na(Bill)), aes(Bill))+
  geom_histogram(bins = 25)+
  geom_vline(data=SparrowsData, aes(xintercept =  SparrowMeans[[1]]))
geom_density()

plot3a <-   ggplot(data=subset(SparrowsData, !is.na(Bill)), aes(x=1, y = Bill))+
  geom_boxplot()+
  coord_flip()
grid.arrange(plot1,plot1a, plot2,plot2a, plot3, plot3a)


### More plots to examine fits etc ####


Wing_Sex1 = ggplot(data=subset(SparrowsData, !is.na(Wing)), aes(Sex.1, y = Wing))+
  geom_boxplot()

Wing_Sex2 = ggplot(data=subset(SparrowsData, !is.na(Wing)), aes(Sex.1, y = Wing))+
  geom_smooth()




print(SparrowStats)

#gives max-liklihod estimtors

fitdistr(c(na.exclude(SparrowsData$Bill)), "normal")

#### t tests. using var1~var2 in t.test takes mean of each and performs test

ttest = t.test(SparrowsData$Mass~ SparrowsData$Sex.1)
Sparrows2001 =  subset(SparrowsData, Year == 2001)    #2001 only
t.test2001 = t.test(Sparrows2001$Wing~Sparrows2001$Sex.1) # calcs mean of wing for make and female and does t test


