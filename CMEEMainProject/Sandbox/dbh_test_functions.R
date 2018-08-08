

 data = DBH_Yr2_agg%>%filter(SITE==1)%>%filter(PLOT==1)
 sum(data["Count"])/nrow(data)
 
 data = list()
 means=list()
 medians=list()
 sds = list()
 skews=data.frame()
 for (i in 1:103){
      data[[i]] = DBH_Yr2_agg%>%filter(SITE==i)
      means[[i]]=aggregate(Count~PLOT, data = x, FUN = mean)
      #means for all plots by site. mean of site5 plot4 is means[[5]][[2]][4]
      medians[[i]]=aggregate(Count~PLOT, data = x, FUN = median)
      sds[[i]]= aggregate(Count~PLOT, data = x, FUN = sd)
      #as above for medians
 }
 
 for (i in 1:103){
   for (j in 1:16){
     skews[i,j] = 3*(means[[i]][[2]][j]- medians[[i]][[2]][j])/(sds[[i]][[2]][j])
   }
 }
 
 data[[1]]
 
 tapply(x$Count, x$PLOT, mean)
 aggregate(Count~PLOT, data = x, FUN = mean)