#new way of getting z's using the formula of Chiarucci

rm(list = ls())
cat("\014")

library(dplyr)

#plot level SAC


plotrichness = read.csv("../Data/plot_rich.csv")
#plot richness includes the missing plots as 0's, col 1 in random row numbers
plotrichness = plotrichness[, -1]

#Input all the ground flora data
Data = read.csv("../Data/GroundCover.csv")
Data_Yr2 = Data %>% filter(Yr_2 == 2)#%>%select(SITE,PLOT,NEST,COV,Amalgams)
colnames(Data_Yr2) = c("SITE", "PLOT", "NEST", "Cover", "BRC_number", "Year")
veg_codes = read.csv("../Data/vegetation_codes.csv")

# the  bryophytes, lichen etc have already been removed from veg codes csv so join this with ground cover to eliminate bryophytes from counts


colnames(veg_codes) = c("Species", "BRC_number")
Data_Yr2_veg = Data_Yr2 %>% inner_join(veg_codes)
# now using Dat_Yr2_veg means analysis is carried out without bryophytes

#for each site create a table of unique BRC, occurances of BRC.

#################################################################

get_site_brcs = function(sitenum){
  numplots = max(Data_Yr2_veg%>%filter(SITE == sitenum)%>%select(PLOT))
  BRClist= list()
  for (j in 1:numplots){
    BRClist[[j]] = unique(Data_Yr2_veg%>%filter(SITE==sitenum)%>%filter(PLOT==j)%>%select(BRC_number))
  }
  return(BRClist)
}
  
get_occurances = function(brclist){
  t = as.data.frame(table(unlist(brclist)))
  return(t)
}  
 
get_numplots = function(data,sitenum){
  numplots = max(Data_Yr2_veg%>%filter(SITE == sitenum)%>%select(PLOT))
  return(numplots)
}

get_Sn = function(occ){
  Sn = nrow(occ)
  return(Sn)
}

get_col = function(occ,colnum,numplots){
  end = nrow(occ)
  col = vector()
  for (i in 1:end){
    n = numplots - occ$Freq[i]
    r = colnum
    value = choose(n,r)
    col = c(col,value)
  }
  return(col)
}

get_site_df = function(occ,numplots,Sn){
  
  sitedf = data.frame(row.names = 1:Sn)
    for (i in 1:numplots){
      col = get_col(occ,i,numplots)
      sitedf = cbind(sitedf,col)
    }
  return(sitedf)
}
  
 
  
  
  