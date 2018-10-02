setwd("C:/dev/code/CMEECourseWork/CMEEMainProject/Code")
library(dplyr)
library(ggplot2)
library(reshape)

allplotvars = read.csv("../Data/AllPlotsVarsRichness.csv")

site = allplotvars%>%filter(Site==50)%>%
  select(pHYr2, SOMYr2, LiveBasalAreaYr2, mean_dbh, tree.density)
scaledsite= scale(site)
melted = melt(scaledsite)
ggplot(melted,aes(x = value, fill = X2))+
   geom_density(alpha = 0.2, adjust = 3)+
  xlim(c(-3,3))
 

shapiro.test(site1$pHYr2)
shapiro.test(site1$SOMYr2)
shapiro.test(site1$mean_dbh)
shapiro.test(site1$tree.density)

test = shapiro.test(site1$pHYr2)

Site = c(1:103)

scaledallplotvars = as.data.frame(scale(allplotvars%>%select(pHYr2,SOMYr2,LiveBasalAreaYr2,mean_dbh, tree.density)))
scaledallplotvars = cbind(Site,scaledallplotvars)
pvaldf = data.frame()
for (i in 1:103){
  thissite = scaledallplotvars%>%filter(Site==i)%>%
    select(pHYr2, SOMYr2, LiveBasalAreaYr2, mean_dbh, tree.density)
  pvals = apply(thissite,2,shapiro.test)
  thisrow = vector()
    for (j in 1:5){
     p = round(pvals[[j]]$p.value,2)
     thisrow = c(thisrow,p)
    }
  pvaldf = rbind(pvaldf,thisrow)
  
}
colnames(pvaldf) = c("pH","SOM","LBA","DBH","TD")

valuescountpH = nrow(pvaldf[pvaldf$pH<0.05,])
valuescountSOM = nrow(pvaldf[pvaldf$SOM<0.05,])
valuescountLBA = nrow(pvaldf[pvaldf$LBA<0.05,])
valuescountDBH = nrow(pvaldf[pvaldf$DBH<0.05,])
valuescountTD = nrow(pvaldf[pvaldf$TD<0.05,])






