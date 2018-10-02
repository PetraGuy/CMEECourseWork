#new way of getting z's using the formula of Chiarucci
#Note - this is pointless because vegan has this option in its specacc function(method = exact)

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

# a list of unique brc in each plot/site
get_site_brcs = function(sitenum){
  numplots = max(Data_Yr2_veg%>%filter(SITE == sitenum)%>%select(PLOT))
  BRClist= list()
  for (j in 1:numplots){
    BRClist[[j]] = unique(Data_Yr2_veg%>%filter(SITE==sitenum)%>%filter(PLOT==j)%>%select(BRC_number))
  }
  return(BRClist)
}

#shows occurances/freq of each brc in a site
get_occurances = function(brclist){
  t = as.data.frame(table(unlist(brclist)))
  return(t)
}  

#gives number of plots in a site
get_numplots = function(data,sitenum){
  numplots = max(Data_Yr2_veg%>%filter(SITE == sitenum)%>%select(PLOT))
  return(numplots)
}

#number of species in a site
get_Sn = function(occ){
  Sn = nrow(occ)
  return(Sn)
}

#used by get_site_df
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

#gets the presence absence data frome using the site occurance data
get_site_df = function(occ,numplots,Sn){
  sitedf = data.frame(row.names = 1:Sn)
    for (i in 1:numplots){
      col = get_col(occ,i,numplots)
      sitedf = cbind(sitedf,col)
    }
  return(sitedf)
}

#this is the calculation 
get_final_row = function(sitedf,numplots,Sn){
  value1 = vector()
  for (i in 1:numplots){
   tmp = (choose(numplots,i))^-1
   value1 = c(value1,tmp)
  }
  value2 = colSums(sitedf)
  finalrow = Sn - value1*value2
  return(finalrow)
}
  
#now get the SAC for each site

get_SAC = function(sitenum,allspecieslists){
  brc = get_site_brcs(sitenum)
  occ = get_occurances(brc)
  numplots = get_numplots(allspecieslists,sitenum)
  Sn = get_Sn(occ)
  df = get_site_df(occ,numplots,Sn)
  SAC = get_final_row(df,numplots,Sn)
  return(SAC)
}
################

#get the SACs for each wood

SAC_list = list()
for (i in 1:103){
  SAC_list[[i]] = get_SAC(i,Data_Yr2_veg)
}

###############
#Now get the zs for those SACs

get_z = function(SAC){
  numplots = length(SAC)
  end = 200*numplots
  areas = seq(200,end,200)
  df = as.data.frame(cbind(log(areas),log(SAC)))
  colnames(df) = c("logarea","logfreq")
  rownames(df) = c()
  model = lm(logfreq~logarea,df)
  z = model$coefficients[2][[1]]
  return(z)
}

zexp = vector()
for(i in 1:103){
  zexp = c(zexp,get_z(SAC_list[[i]]))
}

write.csv(zexp,"../Data/zexp.csv")
