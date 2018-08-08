#!/usr/bin/env Rscript

#Looking at linear plots and residuals - are they normally distributed etc.
rm(list = ls())
library(ggplot2)
library(gridExtra) # for multiple plots
l

###   Import and normalise Data #####
SparrowsData = as.data.frame(read.table("../Data/SparrowSize.txt", header = TRUE))

Sparrows2001 = subset(SparrowsData, SparrowsData$Year==2001)

lm_Mass_Bill = lm(Mass ~ Bill, SparrowsData)
par(mfrow =c(2,2))
plot(lm_Mass_Bill)

##How many NA in Bill and Mass

Bill_NA = sum(is.na(SparrowsData$Bill))
Mass_NA = sum(is.na(SparrowsData$Mass))


#####################

#A linear model of Mass and Bill
plot_Mass_Bill = ggplot(data=subset(SparrowsData, !is.na(Bill)), aes(x = Bill, y= Mass))+
                geom_point()+
              geom_smooth(method='lm',formula=y~x)
# Look at some boxplots of mass and bill with sex to see if they have the same mean. If not
# you might need to separate them in the linear model

Wing_Sex1 = ggplot(data=subset(SparrowsData, !is.na(Mass)), aes(Sex.1, y = Mass))+
  geom_boxplot()

Wing_Sex2 = ggplot(data=subset(SparrowsData, !is.na(Mass)), aes(Sex.1, y = Mass))+
  geom_point()

grid.arrange(Wing_Sex1, Wing_Sex2)

#################################

lm_WingSex = lm(Wing ~ as.numeric(Sex), SparrowsData)
lm_MassSex = lm(Mass ~ as.numeric(Sex), SparrowsData)
lm_BillSex = lm(Bill ~ as.numeric(Sex), SparrrowsData)
lm_TarsusSex = lm(Tarsus ~ as.numeric(Sex), SparrowsData)
#ps. summary(linearmodel) gives you the rsquared etc. 
# simply plot(linearmodel) plots 4 residual plots
par(mfrow = c(2,2))
plot(lm_BillSex, main = "BillSex")
plot(lm_MassSex, main = "MassSex")
plot(lm_TarsusSex, main = "TarsusSex")
plot(lm_WingSex, main = "WingSex")
