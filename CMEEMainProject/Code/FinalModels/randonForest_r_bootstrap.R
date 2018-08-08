

rm(list = ls())
cat("\014")
library(dplyr)
library(rpart)
library(rpart.plot)
library(gbm)#bgm
library(caret)
library(Metrics) #rmse
library(randomForest)
library(gridExtra)


site_data =  read.csv("../../Data/CompleteSiteLevelVars.csv")
site_data = site_data[,-1]

zeta_r = readRDS("../Zeta/zeta_r")
Site = c(1:103)
zeta_r = as.data.frame(cbind(Site,zeta_r))
site_data = inner_join(zeta_r,site_data)

#mean impute the missing PHI
meanPHI = round(mean(site_data$Pos_Hetero_Index, na.rm = TRUE),2)
x = site_data$Pos_Hetero_Index
x[is.na(x)] = meanPHI
site_data$Pos_Hetero_Index = x


subset_all = site_data%>%select("Site","Richness","Area_ha",
                                "Northing", "Pos_Hetero_Index","Buffer3",
                                "no_MSG", "no_NVC","sd_pH","sd_SOM","sd_LBA",
                                "sd_meandbh","sd_treedensity","area_ratio",
                                "meandbh","meanph", "meanSOM","meanLBA",
                                "meantreedensity","zeta_r")



colnames(subset_all) = c("Site","Richness","Area",
                         "Northing", "PHI","Buffer",
                         "no_MSG", "no_NVC","sd_pH","sd_SOM","sd_LBA",
                         "sd_meandbh","sd_TD","area_ratio",
                         "meandbh","meanph", "meanSOM","meanLBA",
                         "meanTD","zeta_r")


subset_mean = subset_all%>%select("Richness",
                                  "Northing", "PHI","Buffer",
                                  "no_MSG", "no_NVC","area_ratio",
                                  "meandbh","meanph", "meanSOM","meanLBA",
                                  "meanTD","zeta_r")



subset_sd = subset_all%>%select("Richness",
                                "Northing", "PHI","Buffer",
                                "no_MSG", "no_NVC","sd_pH","sd_SOM","sd_LBA",
                                "sd_meandbh","sd_TD","area_ratio",
                                "zeta_r")










rmse_test = 0
rmse_train = 0
#varimp_o = data.frame(nrow = 8 )
vars = (colnames(subset_sd[-1]))
vis = c(0,0,0,0,0,0,0,0,0,0,0)

for (i in 1:100){
  
  assignment <- sample(1:2, size = nrow(subset_mean), prob = c(0.75,0.25), replace = TRUE)
  

  train <- subset_sd[assignment == 1, ]    # subset the grade data frame to training indices only
  test <- subset_sd[assignment == 2, ]
  forest = randomForest(formula = Richness~., data = train,importance = TRUE,
                        mtry = 4,
                        nodesize = 8,
                        sampsize = 61)  
  
  pred_test= predict(object = forest, newdata = test)
  pred_train = predict(object = forest, newdata= train)
  
  rmse_test = rmse_test + rmse(actual = test$Richness, predicted = pred_test)
  rmse_train = rmse_train + rmse(actual = train$Richness, predicted = pred_train)
  
  vis = vis + varImp(forest)$Overall
    
  
  
  #o = order(vi, decreasing = TRUE)
  #varimp_o = as.data.frame(cbind(varimp_o,o))
}

rmse_test = round(rmse_test/100,2)
rmse_train = round(rmse_train/100,2)
vis_rnd = (round(vis/100,2))
vis_df = data.frame(IncMSE = length(variables))
vis_df = as.data.frame(cbind(vars,vis_rnd))
vis_df$vis_rnd = as.numeric(levels(vis_df$vis_rnd))[vis_df$vis_rnd]


ggplot(data = vis_df , aes(x = reorder(vars, -vis_rnd), y = vis_rnd)) +  
  geom_bar(stat = "identity")+
  ylab("%IncMSE")+
  xlab("")+
  theme(axis.text.x=element_text(angle = 45, hjust = 1))+
  ggtitle("Inc MSE for 100 bootstrapped random forests")+
  annotate("text", x = 10, y = 15, label = paste("rmse train set = ", rmse_train))+
  annotate("text", x = 10, y = 14, label = paste("rmse test set = ", rmse_test))



####################
varimp_o = varimp_o[,-1]

First = unlist(varimp_o[1,])
First_table = table(First)
data = as.data.frame(First_table)
g1 = ggplot(data = data, aes(x = First, y=Freq))+
  geom_bar(stat = "identity", width = 0.25)

Second = unlist(varimp_o[2,])
Second_table = table(Second)
data = as.data.frame(Second_table)
g2 = ggplot(data = data, aes(x = Second, y=Freq))+
  geom_bar(stat = "identity",width = 0.25)

Third = unlist(varimp_o[3,])  
Third_table = table(Third)
data = as.data.frame(Third_table)
g3 = ggplot(data = data, aes(x = Third, y=Freq))+
  geom_bar(stat = "identity",width=0.25)

Fourth = unlist(varimp_o[4,])
Fourth_table = table(Fourth)
data = as.data.frame(Fourth_table)
g4 = ggplot(data = data, aes(x = Fourth, y=Freq))+
  geom_bar(stat = "identity",width=0.25)

Fifth = unlist(varimp_o[5,])
Fifth_table = table(Fifth)
data = as.data.frame(Fifth_table)
g5 = ggplot(data = data, aes(x = Fifth, y=Freq))+
  geom_bar(stat = "identity",width=0.25)

Sixth = unlist(varimp_o[6,])
Sixth_table = table(Sixth)
data = as.data.frame(Sixth_table)
g6 = ggplot(data = data, aes(x = Sixth, y=Freq))+
  geom_bar(stat = "identity",width=0.25)

Seventh = unlist(varimp_o[7,])
Seventh_table = table(Seventh)
data = as.data.frame(Seventh_table)
g7 = ggplot(data = data, aes(x = Seventh, y=Freq))+
  geom_bar(stat = "identity",width=0.25)

Eighth = unlist(varimp_o[8,])
Eighth_table = table(Eighth)
data = as.data.frame(Eighth_table)
g8 = ggplot(data = data, aes(x = Eighth, y=Freq))+
  geom_bar(stat = "identity",width=0.25)





grid.arrange(g1,g2,g3,g4,g5,g6,g7,g8,ncol =2)

par
