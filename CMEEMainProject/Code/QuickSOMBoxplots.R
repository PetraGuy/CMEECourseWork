
library(dplyr)
require(ggplot2)
require(reshape)
library(gridExtra)
rm(list = ls())
cat("\014")
site_data =  read.csv("../Data/CompleteSiteLevelVars.csv")
AllPlotsvars = read.csv("../Data/AllPlotsVarsRichness.csv")

W4 = AllPlotsvars%>%filter(ShortNVC == "W4")%>%select(ShortNVC,SOMYr2,pHYr2,plot_richness)
W8 = AllPlotsvars%>%filter(ShortNVC == "W8")%>%select(ShortNVC,SOMYr2,pHYr2,plot_richness)
W10 = AllPlotsvars%>%filter(ShortNVC == "W10")%>%select(ShortNVC,SOMYr2,pHYr2,plot_richness)
W16 = AllPlotsvars%>%filter(ShortNVC == "W16")%>%select(ShortNVC,SOMYr2,pHYr2,plot_richness)
W21 = AllPlotsvars%>%filter(ShortNVC == "W21")%>%select(ShortNVC,SOMYr2,pHYr2,plot_richness)
M15 = AllPlotsvars%>%filter(ShortNVC == "M15")%>%select(ShortNVC,SOMYr2,pHYr2,plot_richness)
M16 = AllPlotsvars%>%filter(ShortNVC == "M16")%>%select(ShortNVC,SOMYr2,pHYr2,plot_richness)
OV27 = AllPlotsvars%>%filter(ShortNVC == "OV27")%>%select(ShortNVC,SOMYr2,pHYr2,plot_richness)
W6 = AllPlotsvars%>%filter(ShortNVC == "W6")%>%select(ShortNVC,SOMYr2,pHYr2,plot_richness)
W12 = AllPlotsvars%>%filter(ShortNVC == "W12")%>%select(ShortNVC,SOMYr2,pHYr2,plot_richness)

W13 = AllPlotsvars%>%filter(ShortNVC == "W13")%>%select(ShortNVC,SOMYr2,pHYr2,plot_richness)
W14 = AllPlotsvars%>%filter(ShortNVC == "W14")%>%select(ShortNVC,SOMYr2,pHYr2,plot_richness)
W15 = AllPlotsvars%>%filter(ShortNVC == "W15")%>%select(ShortNVC,SOMYr2,pHYr2,plot_richness)
W16 = AllPlotsvars%>%filter(ShortNVC == "W16")%>%select(ShortNVC,SOMYr2,pHYr2,plot_richness)
MG7 = AllPlotsvars%>%filter(ShortNVC == "MG7A")%>%select(ShortNVC,SOMYr2,pHYr2,plot_richness)



df = as.data.frame(rbind(W4,W8,W10,W16,W21,M15,M16))
df$SOM_div_10 = df$SOMYr2/10
df$pH_div_10 = df$pHYr2
df$PlotRichness = df$plot_richness/10

df_mod = df[-c(2,3,4)]

melted = melt(df_mod)


ggplot(data=melted, aes(y = value, x = ShortNVC,colour = variable))+
  geom_boxplot(varwidth = FALSE, outlier.colour = NULL)+
  scale_y_continuous(breaks = seq(0,10, by = 1))+
  geom_vline(xintercept = c(1.5,2.5,3.5,4.5,5.5,6.5))
  

ggplot(AllPlotsvars, aes(x = SOMYr2, y = plot_richness))+geom_point()

ggplot(AllPlotsvars, aes(x=mean_dbh, y=plot_richness), varwidth = TRUE) +
  geom_boxplot(fill="skyblue", aes(group = cut_width(mean_dbh, 5)), na.rm = TRUE)

  

ggplot(AllPlotsvars, aes(x=LiveBasalAreaYr2, y=plot_richness), varwidth = TRUE) +
  geom_boxplot(fill="skyblue", aes(group = cut_width(LiveBasalAreaYr2, 1)), na.rm = TRUE)


ggplot(AllPlotsvars, aes(x=SOMYr2, y=plot_richness), varwidth = TRUE) +
  geom_boxplot(fill="skyblue", aes(group = cut_width(SOMYr2, 5)), na.rm = TRUE)

ggplot(AllPlotsvars, aes(x=pHYr2, y=plot_richness), varwidth = TRUE) +
  geom_boxplot(fill="skyblue", aes(group = cut_width(pHYr2, 1)), na.rm = TRUE)


data =  AllPlotsvars%>%select(ShortNVC,SOMYr2,pHYr2,plot_richness)
codefreq = as.data.frame(table(AllPlotsvars$ShortNVC))
codefreq = codefreq[order(codefreq$Freq),]
bigNVC = codefreq%>%filter(Freq>10)
bigNVC = bigNVC[-3,]
codes = bigNVC$ShortNVC
data$ShortNVC = as.character(data$ShortNVC)
bigNVC$ShortNVC = as.character(bigNVC$ShortNVC)
colnames(bigNVC) = c("ShortNVC","Freq")

data = data[order(data$ShortNVC),]

AllNVC = unique(data$ShortNVC)


databigNVC = data%>%filter(ShortNVC %in% codes)

databigNVC$SOMYr2 = databigNVC$SOMYr2/10
databigNVC$plot_richness = databigNVC$plot_richness/10
melted = melt(databigNVC)

ggplot(data=melted, aes(y = value, x = ShortNVC,colour = variable))+
  geom_boxplot(varwidth = FALSE, outlier.colour = NULL, na.rm = TRUE)

gw4= ggplot(W4, aes(x=SOMYr2, y=plot_richness)) +
  geom_boxplot(fill="skyblue", aes(group = cut_width(SOMYr2, 10)), varwidth = FALSE, na.rm = TRUE)+
  annotate("label", x = 80, y = 80, label = "W4")+
  geom_point(alpha = 0.2, colour = "red")+
  scale_y_continuous(limits = c(0,100) )

gw6 = ggplot(W6, aes(x=SOMYr2, y=plot_richness), varwidth = FALSE) +
  geom_boxplot(fill="skyblue", aes(group = cut_width(SOMYr2, 10)), na.rm = TRUE)+
annotate("text", x = 80, y = 40, label = "W6")+
  geom_point(alpha = 0.2, colour = "red")+
  geom_smooth(method = "lm")
  scale_y_continuous(limits = c(0,100) )


gw8 = ggplot(W8, aes(x=SOMYr2, y=plot_richness), varwidth = FALSE) +
  geom_boxplot(fill="skyblue", aes(group = cut_width(SOMYr2, 10)), na.rm = TRUE)+
annotate("text", x = 80, y = 40, label = "W8")+
  geom_point(alpha = 0.2, colour = "red")+
  geom_smooth(method = "lm")
  scale_y_continuous(limits = c(0,100) )


gw10 = ggplot(W10, aes(x=SOMYr2, y=plot_richness), varwidth = FALSE) +
  geom_boxplot(fill="skyblue", aes(group = cut_width(SOMYr2, 10)), na.rm = TRUE)+
  annotate("text", x = 80, y = 40, label = "W10")+
  geom_point(alpha = 0.2, colour = "red")+
  geom_smooth(method = "lm")
  scale_y_continuous(limits = c(0,100) )

w10lm = lm(plot_richness~SOMYr2, w10)


gw12 = ggplot(W12, aes(x=SOMYr2, y=plot_richness), varwidth = FALSE) +
  geom_boxplot(fill="skyblue", aes(group = cut_width(SOMYr2, 10)), na.rm = TRUE)+
  annotate("text", x = 80, y = 40, label = "W12")+
  geom_point(alpha = 0.2, colour = "red")+
  geom_smooth(method = "lm")
  scale_y_continuous(limits = c(0,100) )


gw13 = ggplot(W13, aes(x=SOMYr2, y=plot_richness), varwidth = FALSE) +
  geom_boxplot(fill="skyblue", aes(group = cut_width(SOMYr2, 10)), na.rm = TRUE)+
  annotate("text", x = 80, y = 40, label = "W13")+
  geom_point(alpha = 0.2, colour = "red")+
  scale_y_continuous(limits = c(0,100) )

gw14 = ggplot(W14, aes(x=SOMYr2, y=plot_richness), varwidth = FALSE) +
  geom_boxplot(fill="skyblue", aes(group = cut_width(SOMYr2, 10)), na.rm = TRUE)+
  annotate("text", x = 80, y = 40, label = "W14")+
  geom_point(alpha = 0.2, colour = "red")+
  geom_smooth(method = "lm")
  scale_y_continuous(limits = c(0,100) )

gw15 = ggplot(W8, aes(x=SOMYr2, y=plot_richness), varwidth = FALSE) +
  geom_boxplot(fill="skyblue", aes(group = cut_width(SOMYr2, 10)), na.rm = TRUE)+
  annotate("text", x = 80, y = 40, label = "W15")+
  geom_point(alpha = 0.2, colour = "red")+
  scale_y_continuous(limits = c(0,100) )

gw16 = ggplot(W8, aes(x=SOMYr2, y=plot_richness), varwidth = FALSE) +
  geom_boxplot(fill="skyblue", aes(group = cut_width(SOMYr2, 10)), na.rm = TRUE)+
  annotate("text", x = 80, y = 40, label = "W16")+
  geom_point(alpha = 0.2, colour = "red")+
  geom_smooth(method = "lm")
  scale_y_continuous(limits = c(0,100) )

gw21 = ggplot(W8, aes(x=SOMYr2, y=plot_richness), varwidth = FALSE) +
  geom_boxplot(fill="skyblue", aes(group = cut_width(SOMYr2, 10)), na.rm = TRUE)+
  annotate("text", x = 80, y = 40, label = "W21")+
  geom_point(alpha = 0.2, colour = "red")+
  geom_smooth(method = "lm")
  scale_y_continuous(limits = c(0,100) )

gov27 = ggplot(W8, aes(x=SOMYr2, y=plot_richness), varwidth = FALSE) +
  geom_boxplot(fill="skyblue", aes(group = cut_width(SOMYr2, 10)), na.rm = TRUE)+
  annotate("text", x = 80, y = 40, label = "OV27")+
  geom_point(alpha = 0.2, colour = "red")+
  geom_smooth(method = "lm")
  scale_y_continuous(limits = c(0,100) )


gmg7 = ggplot(MG7, aes(x=SOMYr2, y=plot_richness), varwidth = FALSE) +
  geom_boxplot(fill="skyblue", aes(group = cut_width(SOMYr2, 10)), na.rm = TRUE)+
  annotate("text", x = 80, y = 40, label = "MG7")+
  geom_point(alpha = 0.2, colour = "red")+
  geom_smooth(method = "lm")
  scale_y_continuous(limits = c(0,100) )


grid.arrange(gw6,gw8,gw10,gw16,gw21,gov27,ncol = 2)

w10all = AllPlotsvars%>%filter(ShortNVC == "W10")
W8all = AllPlotsvars%>%filter(ShortNVC == "W8")
w16all = AllPlotsvars%>%filter(ShortNVC=="W16")

groundcover = read.csv("../data/GroundCover.csv")

w10high1= groundcover%>%filter(SITE==42)%>%filter(PLOT==16)
w10high2 = groundcover%>%filter(SITE==77)%>%filter(PLOT==13) 
w10low1 = groundcover%>%filter(SITE==98)%>%filter(PLOT==5) 
w10low2 = groundcover%>%filter(SITE==91)%>%filter(PLOT==12)

w8high1 = groundcover%>%filter(SITE==60)%>%filter(PLOT==4)
w8high2= groundcover%>%filter(SITE==55)%>%filter(PLOT==10)
w8low1= groundcover%>%filter(SITE==4)%>%filter(PLOT==8)
w8low2 =groundcover%>%filter(SITE==71)%>%filter(PLOT==9)

Ellenbergs = read.csv("../Data/Ellenbergs.csv")
colnames(Ellenbergs) = c("Amalgams","Taxon.name","L" , "F" ,  "R" , "N" ,  "S" )
Ellenbergs$Amalgams = gsub(" ", "", Ellenbergs$Amalgams, fixed = TRUE)
Ellenbergs$Amalgams = as.numeric(Ellenbergs$Amalgams)

vegcodes = read.csv("../Data/vegetation_codes.csv")
colnames(vegcodes) = c("Species","Amalgams")

w10lowveg1 = inner_join(vegcodes,w10low1)
w10lowveg1ellen = inner_join(w10lowveg1,Ellenbergs)


w10lowveg2 = inner_join(vegcodes,w10low2)
w10lowveg2ellen = inner_join(w10lowveg2,Ellenbergs)

w10highveg1 = inner_join(vegcodes,w10high1)
w10highveg1ellen = inner_join(w10highveg1,Ellenbergs)

w10highveg2 = inner_join(vegcodes,w10high2)
w10high2ellen = inner_join(w10highveg2,Ellenbergs)

diffW10 = setdiff(w10highveg1ellen,w10lowveg1ellen)

diffW10low = setdiff(w10lowveg1ellen$Amalgams,w10highveg1ellen$Amalgams)
w10lownothigh = vegcodes%>%filter(Amalgams %in% diffW10low)

diffW10high = setdiff(w10highveg1ellen$Amalgams,w10lowveg1ellen$Amalgams)
w10highnotlow = vegcodes%>%filter(Amalgams %in% diffW10high)
#################
w8lowveg1 = inner_join(vegcodes, w8low1)
w8lowveg1ellen = inner_join(w8lowveg1,Ellenbergs)

w8lowveg2 = inner_join(vegcodes,w8low2)
w8lowveg2ellen = inner_join(w8lowveg2, Ellenbergs)

w8highveg1 = inner_join(vegcodes, w8high1)
w8highveg1ellen = inner_join(w8highveg1, Ellenbergs)

w8highveg2 = inner_join(vegcodes, w8high2)
w8highveg2ellen = inner_join(w8highveg2, Ellenbergs)


get_ave_ellens = function(site,plot){
  ellens = vector()
  plot = groundcover%>%filter(SITE==site)%>%filter(PLOT==plot)
  plotveg =  inner_join(vegcodes,plot)
  plotvegellen = inner_join(plotveg,Ellenbergs)
  ave_N = mean(plotvegellen$N)
  ave_R = mean(plotvegellen$R)
  ellens[1] = ave_N
  ellens[2] = ave_R
  return(ellens)
}

w10low1 = get_ave_ellens(98,5)
w10low2 = get_ave_ellens(91,12)
w10high1 = get_ave_ellens(42,16)
w10high2 = get_ave_ellens(77,13)

w8low1 = get_ave_ellens(4,8)
w8low2 = get_ave_ellens(71,9)
w8high1 = get_ave_ellens(60,4)
w8high2 = get_ave_ellens(55,10)
