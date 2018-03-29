
# INPUTS dbh live counts, direct from access database

# OUTPUTS ../Data/dbh_means.csv, ../Data/tree_density.csv


library(dplyr)
library(ggplot2)
library(gridExtra) # for the ggplots created in for loops



#clear the workspace
rm(list = ls())
cat("\014")

#Get the data , enter input CSV file name here, for data in data directory
inputfile = 'table_DBH_live_counts_71-03.csv'
fullfile = paste("../Data",inputfile, sep = "/")
DBH_measurements = as.tbl(read.csv(fullfile))

#Just year 2
Yr2_DBH = DBH_measurements %>% filter(Yr == 2)

# One line per dbh per plot, i.e. ignore species
DBH_Yr2_agg = aggregate(Count~DBH_class + PLOT + SITE, data = Yr2_DBH, sum) 
DBH_Yr2_agg = filter(DBH_Yr2_agg, PLOT !=60)
# Count is now the sum of counts in each DBH class.

# get the means of each plot but add in NAs for missing plots - these might have no tres or might be missing

Means_df = data.frame()
dbh_classes = c(1:32)
dbh_values = seq(from = 7.5, to = 162.5, length = 32)
for (i in 1:103){
  sitedata = DBH_Yr2_agg%>%filter(SITE == i)
  for (j in 1:16) {
    plotdata = sitedata%>%filter(PLOT == j)
    Means_df[i,j] = round(sum(plotdata$Count*(plotdata$DBH_class*5+2.5))/sum(plotdata$Count), digits = 2)
  }
}

rownames(Means_df) = c(1:103)
colnames(Means_df) = c(1:16)
#the Nan might be an issue, I think its Ok to replace them with zeros, no leave as Nan - sort later as appropriate
#sometimes need the nan to know nohing was there
#Means_df = replace(Means_df, is.na(Means_df),0)


write.csv(Means_df, "../Data/dbh_means.csv")

#calculate the density of trees per plot - 200m^2
tree_density= function(data){
  den = data.frame()
  for (i in 1:103){
    sitedata = data%>%filter(SITE == i)
    for (j in 1:16){
      plotdata = sitedata%>%filter(PLOT == j)
      if (length(plotdata$DBH_class) != 0){
        tmp = sum(plotdata$DBH_class * plotdata$Count)
        tmp = tmp/200
        den[i,j]  = tmp}
      else{
        den[i,j] = NA
      }
      
    }
  }
  return(den)
}
plot_density = tree_density(DBH_Yr2_agg)
write.csv(plot_density, "../Data/tree_density.csv")






