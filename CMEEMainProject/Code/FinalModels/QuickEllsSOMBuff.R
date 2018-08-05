


library(dplyr)
require(ggplot2)
require(reshape)
library(gridExtra)
rm(list = ls())
cat("\014")

sitedata = read.csv("../Data/CompleteSiteLevelVars.csv")

df = as.data.frame(cbind(sitedata$meanSOM, sitedata$mean_R, sitedata$mean_N))
colnames(df) = c("meanSOM", "mean_R","mean_N")
melted = melt(df, id.vars = "meanSOM")

lmN = lm(mean_N~meanSOM,df)
lmN = lm(mean_R~meanSOM,df)
lmBuff = lm(Buffer1~meanSOM,sitedata)
pN = summary(lmN)$coefficients[2,4]
pR = summary(lmR)$coefficients[2,4]
pBuff =summary(lmBuff)$coefficients[2,4]


g1 = ggplot(melted, aes(x = meanSOM, y = value, colour = variable))+
  geom_point()+
  geom_smooth(method = "lm")+
  ylab("Mean Ellenberg Value")+
  theme(legend.position = c(.8,.7))

g2 = ggplot(sitedata, aes(x = meanSOM, y = Buffer1))+
  geom_point()+
  geom_smooth(method = "lm")+
 ylab("1.5km Buffer")

g3 = ggplot(sitedata, aes(x = mean_N, y = Buffer1))+
  geom_point()+
  ylab("1.5km Buffer")+
  geom_smooth(method = "lm")

grid.arrange(g2,g3,ncol=2)



