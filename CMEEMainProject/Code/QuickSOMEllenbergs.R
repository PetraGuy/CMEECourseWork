


library(dplyr)
require(ggplot2)
require(reshape)
library(gridExtra)
rm(list = ls())
cat("\014")
site_data =  read.csv("../Data/CompleteSiteLevelVars.csv")
AllPlotsvars = read.csv("../Data/AllPlotsVarsRichness.csv")

Ellenbergs = read.csv("../Data/Ellenbergs.csv")
colnames(Ellenbergs) = c("Amalgams","Taxon.name","L" , "F" ,  "R" , "N" ,  "S" )
Ellenbergs$Amalgams = gsub(" ", "", Ellenbergs$Amalgams, fixed = TRUE)
Ellenbergs$Amalgams = as.numeric(Ellenbergs$Amalgams)

vegcodes = read.csv("../Data/vegetation_codes.csv")
colnames(vegcodes) = c("Species","Amalgams")

groundcover = read.csv("../data/GroundCover.csv")

cover_ellenbergs = inner_join(groundcover,Ellenbergs)


get_all_plots = function(nvc){
  nvc =  as.character(nvc)
  allplots = AllPlotsvars%>%filter(ShortNVC == nvc)
  return(allplots)
}

get_ellens = function(allplots){
  rows = nrow(allplots)
  df = data.frame()
  for(row in 1:rows){
   site = allplots[row,2]
   plot = allplots[row,3]
   plants = cover_ellenbergs%>%filter(SITE == site)%>%filter(PLOT == plot)
   ave_N = round(mean(plants$N),2)
   ave_R = round(mean(plants$R),2)
   SOM = allplots[row,]$SOMYr2
   pH = allplots[row,]$pHYr2
   thisrow = c(SOM,pH,ave_N,ave_R)
   df = rbind(df,thisrow)
   
}
colnames(df) = c("SOM","pH", "ave_N","ave_R")
return(df)
}

get_som_ellens = function(nvc){
  allplots = get_all_plots(nvc)
  ellensdf = get_ellens(allplots)
  return(ellensdf)
}

get_p = function(data){
  vals = vector()
  lmN = lm(SOM~ave_N, data)
  lmR = lm(SOM~ave_R, data)
  lmpH = lm(SOM~pH,data)
  pN = summary(lmN)$coefficients[2,4]
  pR = summary(lmR)$coefficients[2,4]
  ppH = summary(lmpH)$coefficients[2,4]
  vals = c(pN,pR,ppH)
  return(vals)
  
}
###############
w10vals = get_som_ellens("W10")
melted = melt(w10vals, id.vars = "SOM")
ps = get_p(w10vals)
pvals = paste("p ave N =",round(ps[1],2),
              ", p ave R =",round(ps[2],2),
              ", p pH =",round(ps[3],2))

 w10plot = ggplot(melted, aes(x = SOM, y = value, colour = variable))+
    geom_point()+
   geom_smooth(method = "lm")+
  ylab("Average Ellenberg for plot")+
   annotate("text", x = 80, y = 6, label = "W10")+
   annotate("text", x = 40, y = 3, label = pvals)
###############
 w8vals = get_som_ellens("W8")
 melted = melt(w8vals, id.vars = "SOM")
 ps = get_p(w8vals)
 pvals = paste("p ave N =",round(ps[1],2),
               ", p ave R =",round(ps[2],2),
               ", p pH =",round(ps[3],2))
 
 w8plot = ggplot(melted, aes(x = SOM, y = value, colour = variable))+
   geom_point()+
   geom_smooth(method = "lm",linetype="dashed")+
   ylab("Average Ellenberg for plot")+
   annotate("text", x = 80, y = 6, label = "W8")+
   annotate("text", x = 40, y = 3, label = pvals)
###########
 
 w6vals = get_som_ellens("W6")
 melted = melt(w6vals, id.vars = "SOM")
 ps = get_p(w6vals)
 pvals = paste("p ave N =",round(ps[1],2),
               ", p ave R =",round(ps[2],2),
               ", p pH =",round(ps[3],2))
 
 w6plot = ggplot(melted, aes(x = SOM, y = value, colour = variable))+
   geom_point()+
   geom_smooth(method = "lm", linetype="dashed")+
   ylab("Average Ellenberg for plot")+
   annotate("text", x = 80, y = 6, label = "W6")+
   annotate("text", x = 40, y = 3, label = pvals)
 
#############
 
 w16vals = get_som_ellens("W16")
 melted = melt(w16vals, id.vars = "SOM")
 ps = get_p(w16vals)
 pvals = paste("p ave N =",round(ps[1],2),
               ", p ave R =",round(ps[2],2),
               ", p pH =",round(ps[3],2))
 
 w16plot = ggplot(melted, aes(x = SOM, y = value, colour = variable))+
   geom_point()+
   geom_smooth(method = "lm")+
   ylab("Average Ellenberg for plot")+
   annotate("text", x = 80, y = 6, label = "W16")+
   annotate("text", x = 40, y = 3, label = pvals)
 
###########

 w21vals = get_som_ellens("W21")
 melted = melt(w21vals, id.vars = "SOM")
 ps = get_p(w21vals)
 pvals = paste("p ave N =",round(ps[1],2),
               ", p ave R =",round(ps[2],2),
               ", p pH =",round(ps[3],2))
 
 w21plot = ggplot(melted, aes(x = SOM, y = value, colour = variable))+
   geom_point()+
   geom_smooth(method = "lm",linetype="dashed")+
   ylab("Average Ellenberg for plot")+
   annotate("text", x = 80, y = 6, label = "W21")+
   annotate("text", x = 40, y = 3, label = pvals)
 
######
 
 ov27vals = get_som_ellens("W8")
 melted = melt(ov27vals, id.vars = "SOM")
 ps = get_p(ov27vals)
 pvals = paste("p ave N =",round(ps[1],2),
               ", p ave R =",round(ps[2],2),
               ", p pH =",round(ps[3],2))
 
 ov27plot = ggplot(melted, aes(x = SOM, y = value, colour = variable))+
   geom_point()+
   geom_smooth(method = "lm",linetype="dashed")+
   ylab("Average Ellenberg for plot")+
   annotate("text", x = 80, y = 6, label = "OV27")+
   annotate("text", x = 40, y = 3, label = pvals)
 
###
 
 grid.arrange(w6plot,w8plot,w10plot,w16plot,w21plot,ov27plot, ncol = 2)
 