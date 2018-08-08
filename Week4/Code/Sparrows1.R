
# Stats with Sparrows SW2, Exercise 1
library(ggplot2)
library(gridExtra)
library(dplyr)
library(moments) # for skewness
SparrowsData = read.table("../Data/SparrowSize.txt", header = TRUE)

dev.off()



plot1 <- ggplot(data=subset(SparrowsData, !is.na(Wing)), aes(Wing))+
  geom_histogram(bins = 25)+
  geom_vline(data=SparrowsData, aes(xintercept =  Means[[2]]))+
  geom_density()+
  stat_function(fun = dnorm, args = list(mean = 77.4 , sd = 2.42))
  

  
plot1a <- ggplot(data=subset(SparrowsData, !is.na(Wing)), aes(x=1, y = Wing))+
  geom_boxplot()+
  coord_flip()

plot2 <- ggplot(data = subset(SparrowsData, !is.na(Mass)) , aes(Mass))+
  geom_histogram(bins = 25)+
  geom_vline(data=SparrowsData, aes(xintercept =  Means[[3]]))
  geom_density()

plot2a <- ggplot(data=subset(SparrowsData, !is.na(Mass)), aes(x=1, y = Mass))+
  geom_boxplot()+
  coord_flip()

plot3 <- ggplot(data=subset(SparrowsData, !is.na(Bill)), aes(Bill))+
  geom_histogram(bins = 25)+
  geom_vline(data=SparrowsData, aes(xintercept =  Means[[1]]))
  geom_density()
  
plot3a <-   ggplot(data=subset(SparrowsData, !is.na(Bill)), aes(x=1, y = Bill))+
  geom_boxplot()+
  coord_flip()
grid.arrange(plot1,plot1a, plot2,plot2a, plot3, plot3a)


dev.off()

######## Statistics#########
# Doing whole lot in one go
SparrowMeans = round(colMeans(as.data.frame(SparrowsData[,4:6]), na.rm = TRUE), digits = 2)


# There isnt a colVar function so remove non numeric cols otherwise goes mental
NumericCols = sapply(SparrowsData, is.numeric)

Variances =  sapply(SparrowsData[,NumericCols], var, na.rm = TRUE)
SparrowVars = round(Variances, digits = 2)[4:6]

Sparrowsd = round(sqrt(Variances[4:6]), digits = 2)

Skews = sapply(SparrowsData[,NumericCols], skewness, na.rm = TRUE)

SparrowSkews = round(Skews[4:6], digits = 2)


SparrowStats = rbind(SparrowMeans, SparrowVars, Sparrowsd, SparrowSkews)
colnames(SparrowStats) = c("Bill", "Wing", "Mass")
rownames(SparrowStats) = c("Mean", "Variance", "StandDev", "Skewness")
##############################"

estimate_mode <- function(x) {
  
  d <- density(x, na.rm = TRUE)
  d$x[which.max(d$y)]
}




