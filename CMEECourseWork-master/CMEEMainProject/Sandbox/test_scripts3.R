

richness = function(data){
  site_list = list()
  #browser()
    for (i in 1:103){
    sitedata = data%>%filter(SITE == i)
    plot_nest_df = data.frame()
    for (j in 1:16){
      plotdata = sitedata%>%filter(PLOT == j)
      tmp = vector()
       for (k in 1:5){
        nestdata = plotdata%>%filter(NEST ==k)
        tmp = c(tmp, nrow(nestdata))
        }
      plot_nest_df = rbind(plot_nest_df, tmp)
      colnames(plot_nest_df) = c("Nest1","Nest2","Nest3","Nest4","Nest5")
    }
   site_list[[i]]= plot_nest_df
  }
return(site_list)   
}
  
plot_richness = function(data){
  site_matrix = matrix(nrow = 103, ncol = 16)
  #browser()
  for (i in 1:103){
    sitedata = data%>%filter(SITE == i)
    plot_nest_df = data.frame()
    for (j in 1:16){
      plotdata = sitedata%>%filter(PLOT == j)
      site_matrix[i,j] = length(unique(plotdata$BRC_number))
      # for the seedlings there is sometimes record in specified plot, and NA record, therefore have 
      #used unique to capture those that are NAs, but not double count when same species listed in a nest
    }
  }
  return(site_matrix)
}
plot_rich = plot_richness(Data_Yr2_veg)

tmp = read.csv("../Data/plot_rich.csv")
  
pos = c("W","C","S","Aq","Co")
b3 = sitedata%>%filter(Buffer == 1500)
posb3 =b3%>% filter(Category %in% pos)
index_b3 = round(sum(posb3$Propn), digits = 2)
index_b3

