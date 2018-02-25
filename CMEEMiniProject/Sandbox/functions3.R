#CMEE Miniproject
#Petra Guy, Imperial College, pg5112@ic.ac.uk
#November 2017
#R version 1.1.383 Ubuntu 16.04

#Import data of morphological characteristics of sorbus, perform a clustering and a classification model
#and compare

dev.off()
rm(list = ls())
cat("\014")

Soraria_Data = read.csv('../Data/SorariaComplete.csv')

## median impute for the NAs ##
# if NA found, replace it with the median

median_replace1 = function(x){
  ifelse(is.na(x), median(x,na.rm = TRUE), x)
 }

median_replace2 = function(x){
  apply(x,2,median_replace1)
}

Imputed_df = lapply(split.data.frame(Soraria_Data[,2:12], Soraria_Data$Species), FUN = median_replace2)

#Now have  a list of dataframes, each element is a species - joint them back together
temp = do.call(rbind, Imputed_df)
Soraria_Imputed = cbind(Soraria_Data[1], temp)

##Now have a dataframe with NAs replaced by medians = Soraria_Imputed
#####################################################################
##Data Exploration
##Need to scale the data to compare characteristics

temp = scale(Imputed_df)
Soraria_Imputed_sc = cbind(Soraria_Data[1],temp)
##Pairs
pdf('../Results/')
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}
pairs(Soraria_Imputed_sc[-1], lower.panel = panel.smooth, upper.panel = panel.cor)

##scatterplots by species

ggplot(Soraria_Imputed, aes(x= LeafLength, y = LeafWidth, col = Species))+
  geom_point()
ggplot(Soraria_Imputed, aes(x= FruitLength, y = FruitWidth, col = Species))+
  geom_point()
ggplot(Soraria_Imputed, aes(x= Lobing, y = Veins, col = Species))+
  geom_point()

##Box plots for everything
Soraria_col_names = colnames(Soraria_Data)
Soraria_col_names = Soraria_col_names[-1]
plotlist = list()

library(grid)
for (i in seq_along(Soraria_col_names)){
  plot = ggplot(Soraria_Data, (aes_string(x='Species', y=Soraria_col_names[i])))+
                 geom_boxplot()
  plotlist = rbind(plotlist,  plot)
  grid.draw(plot)
}


####examine clustering 
ration_ss =rep(0,8)
for (k in 1:8){
  soraria_km = kmneans(Soraria_Imputed, k, nstarts = 20)
  ratio_ss[k] = soraria_km$tot.withinss/soraria_km$totss
}

plot(ration_ss, type = 'b', xlab = 'k')
  
# try on scaled data

ration_sc_ss =rep(0,8)
for (k in 1:8){
  soraria_sc_km = kmneans(Soraria_Imputed_sc, k, nstarts = 20)
  ratio_sc_ss[k] = soraria_sc_km$tot.withinss/soraria_sc_km$totss
}

plot(ration_ss, type = 'b', xlab = 'k')

####examine classifications in hclust

soraria_dist = dist(Soraria_Imputed_sc)

soraria_single = hclust(soraria_dist, method = 'single')
memb_single = cutree(soraria_single, k = 8)
plot(soraria_single)

soraria_complete = hclust(soraria_dist, method = 'complete')
memb_complete = cutree(soraria_single, k = 8)
plot(soraria_complete)
