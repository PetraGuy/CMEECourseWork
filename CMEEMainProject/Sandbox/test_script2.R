

FindLowCounts = function(data, threshold){
  #browser()
  lcs = data.frame()
  for (i in 1:103){
    sitedata = data%>%filter(SITE == i)
    for (j in 1:16){
      plotdata = sitedata%>%filter(PLOT == j)
      fx = sum(plotdata$DBH_class*plotdata$Count)
      if (fx < threshold){
       lcs[i,j] = fx
      }else{
         lcs[i,j] = "Na"
       }
    }
  }
  locations = which(lcs < threshold, arr.ind = TRUE)
  colnames(locations) = c("SITE", "PLOT")
  return(locations)
}

d = FindLowCounts(DBH_Yr2_agg,3)

means2 = DBH_Yr2_agg%>%group_by(SITE,PLOT)%>%
  summarise(mean_DBH = round(sum(Count*(DBH_class*5+2.5))/sum(Count), digits = 2))

