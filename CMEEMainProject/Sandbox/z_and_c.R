

#Looking at relationship of the correlations of the
#the positive site descriptor codes withz and c


# the coefficients were dreated in NestLevelData
rm(list = ls())
cat("\014")

coefficients = read.csv("../Data/fits.csv")
sitevars = read.csv("../Data/SiteLevelVars.csv")

#make sure sites correspond from the two dataframes
orderedsitevars = sitevars[order(sitevars$Site),]

colnames(coefficients) = c("Site" , "intercept" ,"slope" ,"t_slope" , "p_slope" , "F_value"  )

model_df = as.data.frame(orderedsitevars$mean_L)
model_df$meanW =  orderedsitevars$mean_W
model_df$meandbh = orderedsitevars$meandbh
model_df$numnotrees = orderedsitevars$no_trees
model_df$numpostcodes = orderedsitevars$Pos_Hetero_Index
model_df$numNVC = orderedsitevars$no_NVC
model_df$northing = orderedsitevars$Northing
model_df$area = orderedsitevars$Area_ha
model_df$buffers = orderedsitevars$Buffer3
colnames(model_df) = c("meanL","meanW" ,"meandbh","numnotrees", 
                       "numpostcodes","numNVC" ,"northing" , "area" ,"buffers" )
model_df[is.na(model_df)] = 0
norm_model_df = apply(model_df,2, function(x) x/max(x))

melted = melt(norm_model_df)
Site = rep(c(1:103),9)
slope = rep(coefficients$slope,9)

melted = cbind(melted,Site)
melted = cbind(melted,slope)
colnames(melted) = c( "X1"    "X2"    "value" "Site"  "slope")
ggplot(melted, aes(value,slope))+geom_point(aes(colour = variable))+
  geom_smooth(method = "lm", aes(colour = variable))


