
setwd("C:/dev/code/CMEECourseWork/CMEEMainProject/Code")

library(dplyr)
library(ggplot2)
library(gridExtra)
library(reshape)
rm(list = ls())
cat("\014")
plotdata = read.csv("../Data/AllPlotsVarsRichness.csv")

scotswoods = c(38,40,42,47,50,53,56,59,68,74,76,77,98)

allplotdata_scots = plotdata%>%filter(Site%in%scotswoods)



allplotdatae_england = plotdata%>%filter(!Site%in%scotswoods)

englandNVCs = as.data.frame(table(allplotdatae_england$ShortNVC))
englandNVCs$normfengland = (englandNVCs$Freq/sum(englandNVCs$Freq))
england = rep("england",44)
englandNVCs$england = england
englandNVCs = englandNVCs[-1,]
colnames(englandNVCs) = c("NVCcode","FreqE","normFE","countryE")

scotsNVCs = as.data.frame(table(allplotdata_scots$ShortNVC))
scotsNVCs$normfscotland = (scotsNVCs$Freq/sum(scotsNVCs$Freq))
scotland = rep("scotland",44)
scotsNVCs$scotland = scotland
scotsNVCs = scotsNVCs[-1,]
colnames(scotsNVCs) = c("NVCcode","FreqS","normFS","countryS")

NVCsdf = cbind(scotsNVCs,englandNVCs)
NVCsdf = NVCsdf[,-5]
NVCsdf$diffF = NVCsdf$normFS-NVCsdf$normFE


diffNVCs =c("W21","W8","OV27","W13","W15","W4","W6","W16","W10","M15")

NVCdiff = NVCsdf%>%filter(NVCcode%in%diffNVCs)

ggplot(data = NVCdiff, aes(x = NVCcode, y = diffF))+
  geom_bar(stat = "identity", fill = "blue", alpha = 0.5)+
  annotate(geom="text", x=3, y=0.1, label="More in Scotland",
           color="red")+
  annotate(geom="text", x=3, y=-0.15, label="More in England",
           color="red")+
  geom_text(aes(label = NVCcode), position = position_dodge(1))+
  ylab("Difference in normalised frequency of NVC codes")+
  annotate(geom = "text",x=1.5,y=0.05,label = "Erica tetralix wet heath")+
  annotate(geom = "text",x=2,y=-0.05,label = "Epilobium")+
  annotate(geom = "text",x=3,y=0.05,label = "Quercus")+
  annotate(geom = "text",x=4,y=-0.05,label = "Taxus baccata")+
  annotate(geom = "text",x=5,y=-0.05,label = "Fagus")+
  annotate(geom = "text",x=6,y=0.05,label = "Quercus")+
  annotate(geom = "text",x=7,y=-0.13,label = "Scrub")+
  annotate(geom = "text",x=8,y=.06,label = "Betula,Molina")+
  annotate(geom = "text",x=9,y=.07,label = "Alnus,Urtica")+
  annotate(geom = "text",x=10,y=-0.1,label = "Fraxinus, Acer")
  

scotsNVCsNotNorm = as.data.frame(table(allplotdata_scots$ShortNVC))
englandNVcsNotNorm = as.data.frame(table(allplotdatae_england$ShortNVC))
  
England80 = c("W10","OV27","W21","W16","W8","w6","w15","W13","MG7A","W14","W12")
Scotland80 = c("W10","W16","OV27","W6","W4","W8","M15","S4")

England80df = englandNVCs%>%filter(NVCcode%in%England80)
Scotland80df = scotsNVCs%>%filter(NVCcode%in%Scotland80)

g1 = ggplot(England80df, aes(x = reorder(NVCcode,-normFE), y = normFE))+
  geom_bar(stat = "identity")+
  ylab("normalised frequency")+
xlab("NVCcode")+
ggtitle("NVC codes giving 80% of English and Welsh woodlands")

g2 = ggplot(Scotland80df, aes(x = reorder(NVCcode,-normFS), y = normFS))+
  geom_bar(stat = "identity")+
  xlab("NVCcode")+
  ylab("normalised frequency")+
  ggtitle("NVC codes giving 80% of Scottish woodlands")


grid.arrange(g1,g2, ncol = 2)

 ######
#W10 scotland vs W10 england

W10scots = allplotdata_scots%>%filter(ShortNVC == "W10")%>%select(plot_richness)

W10england = as.vector(allplotdatae_england%>%filter(ShortNVC == "W10")%>%select(plot_richness))

df = as.data.frame(cbind(W10england$plot_richness,W10scots$plot_richness))
colnames(df) = c("England/Wales","Scotland")
melted = melt(df)

g1 = ggplot(melted, aes(x = value, fill=variable))+
  geom_density(alpha=0.25,adjust = 1, show.legend = FALSE)+
  scale_x_continuous(limits = c(-10,60))+
  annotate("label", x = 0, y = 0.042, 
           label = "Density of plot richnesses in W10 plots", fontface=2)+
  theme(legend.position = c(0.8, 0.2))+
  xlab("plot richness")+
  theme(text = element_text(size = 14, face = "bold"))
#####

OV27scots = allplotdata_scots%>%filter(ShortNVC == "OV27")%>%select(plot_richness)

OV27england = as.vector(allplotdatae_england%>%filter(ShortNVC == "OV27")%>%select(plot_richness))

df = as.data.frame(cbind(OV27england$plot_richness,OV27scots$plot_richness))
colnames(df) = c("England/Wales","Scotland")
melted = melt(df)

g2 = ggplot(melted, aes(x = value, fill=variable))+
  geom_density(alpha=0.25,adjust = 1, show.legend = FALSE)+
  scale_x_continuous(limits = c(-10,60))+
  annotate("label", x = 0, y = 0.042, 
           label = "Density of plot richnesses in OV27 plots", fontface = 2)+
  theme(legend.position = c(0.8, 0.2))+
  xlab("plot richness")+
  theme(text = element_text(size = 14, face = "bold"))
####
W8scots = allplotdata_scots%>%filter(ShortNVC == "W8")%>%select(plot_richness)

W8england = as.vector(allplotdatae_england%>%filter(ShortNVC == "W8")%>%select(plot_richness))

df = as.data.frame(cbind(W8england$plot_richness,W8scots$plot_richness))
colnames(df) = c("England/Wales","Scotland")
melted = melt(df)

g3 = ggplot(melted, aes(x = value, fill=variable))+
  geom_density(alpha=0.25,adjust = 1)+
  scale_x_continuous(limits = c(-10,60))+
  annotate("label", x = 0, y = 0.042, 
           label = "Density of plot richnesses in W8 plots")+
  theme(legend.position = c(0.9, 0.4))+
  xlab("plot richness")+
  theme(text = element_text(size = 14, face = "bold"))

grid.arrange(g1,g2, nrow =2)
