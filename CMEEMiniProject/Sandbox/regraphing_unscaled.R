




# data exploration - box plots

#ggsave("boxplot2.pdf", plot = boxplot2)


#separate data on scale
#1 = leaflength,width,widest,angl2
#2 = lobing,veins
#3 = fruit length and width
#4=fruit and leaf ratio widest percent
melted1 = melted%>%filter(variable == "LeafLength"|
                            variable ==  "LeafWidth"|
                            variable ==  "WidestPoint"|
                            variable == "Angle")
melted2 = melted%>%filter(variable == "Lobing"|
                            variable == "Veins")
melted3 = melted%>%filter(variable == "FruitLength"|
                           variable == "FruitWidth")
melted4 = melted%>%filter(variable == "FruitRatio"|
                            variable == "LeafRatio")

ggplot(data = melted1) +  
  geom_boxplot(aes(x=Species,y=value, fill = Species)) +  
  facet_wrap(~variable) +
  theme(axis.ticks = element_blank(), axis.text.x = element_blank())

ggplot(data = melted2) +  
  geom_boxplot(aes(x=Species,y=value, fill = Species)) +   
  facet_wrap(~variable) +
  theme(axis.ticks = element_blank(), axis.text.x = element_blank())


ggplot(data = melted3) +  
  geom_boxplot(aes(x=Species,y=value, fill = Species)) +  
  facet_wrap(~variable) +
  theme(axis.ticks = element_blank(), axis.text.x = element_blank())


ggplot(data = melted4) +  
  geom_boxplot(aes(x=Species,y=value, fill = Species)) +  
  facet_wrap(~variable) +
  theme(axis.ticks = element_blank(), axis.text.x = element_blank())



                         