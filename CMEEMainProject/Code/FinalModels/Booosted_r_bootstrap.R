
setwd("C:/dev/code/CMEECourseWork/CMEEMainProject/Code/FinalModels")



rm(list = ls())
cat("\014")
library(dplyr)
library(rpart)
library(rpart.plot)
library(gbm)#bgm
library(caret)
library(Metrics) #rmse
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

subset_all = site_data%>%select(Site,Richness,Area_ha,
                                Northing, Pos_Hetero_Index,Buffer3,
                                no_MSG, no_NVC,sd_pH,sd_SOM,sd_LBA,
                                sd_meandbh,sd_treedensity,area_ratio,
                                meandbh,meanph, meanSOM,meanLBA,
                                meantreedensity,zeta_r)



colnames(subset_all) = c("Site","Richness","Area",
                         "Northing", "PHI","Buffer",
                         "no_MSG", "no_NVC","sd_pH","sd_SOM","sd_LBA",
                         "sd_meandbh","sd_TD","area_ratio",
                         "meandbh","meanph", "meanSOM","meanLBA",
                         "meanTD","zeta_r")


subset_sd = subset_all%>%select(Site,Richness,
                                       PHI,Buffer,Northing,
                                       no_MSG, no_NVC,sd_pH,sd_SOM,sd_LBA,
                                       sd_meandbh,sd_TD,area_ratio,zeta_r)



subset_mean = subset_all%>%select(Site,Richness,
                                  PHI, Northing, meandbh,Buffer,
                                  meanph,  meanSOM,meanTD,meanLBA, area_ratio,no_NVC, 
                                  no_MSG,zeta_r)




get_traintest = function(datasubset){
  #browser()
  traintest = list()
  assignment <- sample(1:2, size = nrow(datasubset), prob = c(0.7,0.3), replace = TRUE)
  
  traintest[[1]] <- datasubset[assignment == 1, ]   
  traintest[[2]]  <- datasubset[assignment == 2, ]  
  return(traintest)
}

get_model = function(traintest){
  train = traintest[[1]]
  model = gbm(formula = Richness~.,
              data = train,
              n.minobsinnode = 2,
              bag.fraction = 0.8 ,
              interaction.depth = 6,
              n.trees = 10000,
              distribution = "gaussian",
              cv.folds =5)
return(model)
}

get_rmse = function(model,traintest){
 rmses = list()
 trainset = traintest[[1]]
 testset = traintest[[2]]
 
 ntree_opt = gbm.perf(model, method = "cv",oobag.curve = FALSE)
 
  pred_train = predict(model,trainset, ntree_opt) 
  pred_test <- predict(model,testset,ntree_opt)
  
  rmses[[1]] = rmse(actual = trainset$Richness, predicted = pred_train)
  rmses[[2]] = rmse(actual = testset$Richness,   predicted = pred_test)
  return(rmses)
}

get_rel_infl = function(model){
  s = summary(model,n.trees=10000)
  x = as.character(s$var)
  y = s$rel.inf
  data = as.data.frame(cbind(x,y))
  data$y = as.numeric(levels(data$y))[data$y]
  data$x <- factor(data$x, levels = data$x[order(data$y)])
  return(data)
}


get_influence = function(datasubset,relinfl) {
  vars = colnames(datasubset[-1])
  ris = vector()
  for (var in vars){
    ris[var] = as.vector(filter(relinfl,relinfl$x == var)[2])
  }
  return(ris)
 }

add_list_elements = function(list1,list2){
  list_sum = list()
  length = length(list1)
  for (i in 1:length){
   list_sum[[i]] = list1[[i]]+list2[[i]]
  }
  return(list_sum)
}

##############################


##############################

## SOmehting has changed here - check the dataframe creations, orders etc, 
##this code is no loner correct

dataset = subset_mean
dataset = dataset[,-1] # remove site col


run_boosted_bootstrap = function(dataset){
  dataset = dataset[,-1]
  #browser()
  # initialise 
  data = list()
 # rmse_list = list()
  #rmse_list[[1]] = 0
 # rmse_list[[2]] = 0
  
  
  #initalise a list for rel imp
  rel_infl_list = list()
  for (i in 1:ncol(dataset[-1])){
    rel_infl_list[i] = 0
  }
  
  
  
  for (i in 1:100){
  
  data = get_traintest(dataset)
  model = get_model(data)
  #rmse_train = get_rmse(model,data)[[1]]
  #rmse_test = get_rmse(model,data)[[2]]
 # rmse_list[[1]] = rmse_list[[1]] + rmse_train
  #rmse_list[[2]] = rmse_list[[2]] + rmse_test
        
  rel_infl = get_rel_infl(model)
  rel_infl_ordered = get_influence(dataset,rel_infl)
  rel_infl_list = add_list_elements(rel_infl_list,rel_infl_ordered)
          
  }

 
variables = colnames(dataset[-1])
relinlf_vec = vector()
for (i in 1:12){
  relinlf_vec[i] = round((rel_infl_list[[i]]/100),2)
  
}

#relinlf_df = data.frame(rel_influence = length(variables))

#for (i in 1:ncol(dataset[-1])){
 # relinlf_df[i,1] = rel_infl_list[[i]]/100
#}

relinlf_df = as.data.frame(cbind(variables,relinlf_vec))
#relinlf_df$relinlf_vec = (relinlf_df$relinlf_vec)

#rmse_train_set = round(rmse_list[[1]]/100,2)
#rmse_test_set = round(rmse_list[[2]]/100,2)

#ggplot(data = relinlf_df , aes(x = reorder(variables, -relinlf_vec), y = relinlf_vec)) +  
  #geom_bar(stat = "identity")+
  #ylab("relative influence")+
  #xlab("")+
  #labs(title = "MEAN dataset")+
  #theme(axis.text.x=element_text(angle = 45, hjust = 1))+
  #ggtitle("Relative Influence for 100 bootstrapped GBMs")+
  #annotate("text", x = 10, y = 15, label = paste("rmse train set = ", rmse_train_set))+
  #annotate("text", x = 10, y = 14, label = paste("rmse test set = ", rmse_test_set))+
  #theme(text = element_text(size = 14, face = "bold"))
return(relinlf_df)
 }
  
meanrelinfl = run_boosted_bootstrap(subset_mean)
mean_data = relinlf_df
saveRDS(mean_data,"/gbm_mean_dataset")

dataset = subset_sd

sd_data = relinlf_df

saveRDS(sd_data,"/gbm_sd_dataset")

relinlf_df = mean_data
mean_data = readRDS("/gbm_mean_dataset")
sd_data = readRDS("/gbm_sd_dataset")

