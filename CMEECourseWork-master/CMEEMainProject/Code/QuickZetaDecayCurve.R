
library(ggplot2)
library(dplyr)
library(zetadiv)
library(grid)
library(gridExtra)
library(tidyr)
library(reshape)
rm(list = ls())
cat("\014")

empzeta = readRDS("empirical_zetas.RDS")

sites = c("Site56","Site67")
zeta = empzeta[,c(56,67,71,94)]
x = seq(1,16,by = 1)

data = cbind(x,zeta)
colnames(data) = c("order","Site56","Site67","Site71","Site94")

df = melt(data, id.vars = "order")

ggplot(data, aes(x = order, y = value), colour = variable)+
  geom_point(aes(y = Site56, col = "Site56"), size = 2)+
  geom_point(aes(y = Site67,col = "Site67"), size = 2)+
  geom_point(aes(y = Site71,col = "Site71"), size = 2)+
  geom_point(aes(y = Site94,col = "Site94"), size = 2)+
  geom_line(aes(y = Site56, col = "Site56"), size = 1)+
  geom_line(aes(y = Site67,col = "Site67"), size = 1)+
  geom_line(aes(y = Site71,col = "Site71"), size = 1)+
  geom_line(aes(y = Site94,col = "Site94"), size = 1)+
  scale_x_continuous(breaks = scales::pretty_breaks(n=16))+
  ylab("zeta")+
  xlab("zeta order")+
ggtitle("Zeta decay curves")+
   theme(legend.position = c(0.9, 0.8))

+
 annotate("segment", x = 2, xend = 4, y = 2, yend = 12, 
          colour = "black", size=2, alpha=0.6, arrow=arrow())+
  annotate("text", x = 4, y = 13, label = "decreasing heterogeneity")
  
  

                                              