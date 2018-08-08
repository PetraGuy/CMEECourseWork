
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

nestZsdf = readRDS("nest_mixed_model_fits.RDS")
plotZsdf = exp_zs = read.csv("../Data/zexp.csv")

nestZs = nestZsdf$slope
plotZs = plotZsdf$x

df = as.data.frame(cbind(nestZs,plotZs))%>%gather(key,value)
                   

ggplot(df,aes(value, fill=key))+
  geom_density(show.legend = TRUE,alpha = 0.5)+
  xlab("z value")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 14))
   
