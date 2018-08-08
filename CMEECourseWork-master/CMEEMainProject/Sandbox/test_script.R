

means = data.frame()
dbh_classes = c(1:32)
dbh_values = seq(from = 7.5, to = 162.5, length = 32)
for (i in 1:103){
  sitedata = DBH_Yr2_agg%>%filter(SITE == i)
  for (j in 1:16) {
    plotdata = sitedata%>%filter(PLOT == j)
    means[i,j] = round(sum(plotdata$Count*(plotdata$DBH_class*5+2.5))/sum(plotdata$Count), digits = 2)
    }
}
rownames(means) = c(1:103)
colnames(means) = c(1:16)
columns = as.character(c(1:16))
mean_10 = subset(means, columns < 10)

for (i in 1:103){
  for (j in 1:16){
    if (means[i,j] < 10){
    Site = paste("Site",i, sep = "")
    Plot = paste("Plot",j, sep = "")
    print(paste(Site, Plot, sep = " " ))
    print(means[i,j])
  }
}
}
threshold = 10
GetSites = function(lower, upper){
  m_df = data.frame()
  for (i in 1:103){
    for (j in 1:16){
      if( (means[i,j] < upper) && (means[i,j] > lower) ){
        m_df = rbind(m_df,c(i,j,means[i,j]))
      }}}
  colnames(m_df) = c("Site", "Plot","Mean")
  
  plot_list = list()
 
  if(nrow(m_df) >= 4){
    for (i in 1:4){
    #browser()
    rand = sample(nrow(m_df),4)
    tmp = rand[1]
    rand_row = m_df[tmp,]
    site = rand_row[[1]]
    plot = rand_row[[2]]
    mu = rand_row[[3]]
    
  data = DBH_Yr2_agg%>%filter(SITE==site)%>%filter(PLOT==plot)
  Plot = ggplot(data = data, aes(x = DBH_class, y = Count)) +
    geom_bar(stat = "identity", col = "black", fill = "green") +
    scale_x_continuous("DBH class")+
    labs(title = mu)
  plot_list[[i]] = Plot
    }
    grid.arrange(grobs = plot_list, ncol = 2)
  }
}


data = DBH_Yr2_agg%>%filter(SITE==78)%>%filter(PLOT==2 )
Plot = ggplot(data = data, aes(x = DBH_class, y = Count)) +
  geom_bar(stat = "identity", position="dodge", width = 0.5,col = "black", fill = "green") +
  scale_x_continuous("DBH class", 
                     breaks = 1:15,
                     labels = c(1:15),
                     limits = c(0, 15))+
  labs(title = 7.98)
Plot



t1 = paste("site",1)
t2 = paste("plot",1)
t3 = paste("mean= ",5)
Title = paste(t1,t2,t3, sep = " ")

PlotPlotTest = function(i,j){
  data = DBH_Yr2_agg %>% filter(SITE == i)%>%filter(PLOT==j)
  t1 = paste("site",i)
  t2 = paste("plot",j)
  Title = paste(t1,t2, sep = " ")
  Plot = ggplot(data = data, aes(x = DBH_class, y = Count)) +
    geom_bar(stat = "identity", col = "black", fill = "green") +
    scale_x_continuous("DBH class",
                       breaks = brks)+
    stat_bin(breaks = brks)
    print(Plot)
}

data = DBH_Yr2_agg%>%filter(SITE == 1)%>%filter(PLOT == 1)
data = data[,c(1,4)]
Modes_df[i,j] = data%>%filter(DBH_class, Count == max(Count))
###############
brks = seq(0,16, by = 2)

test = DBH(1,2)

###############################
site = DBH_Yr2_agg%>% filter(SITE == 1)
plot = site%>%filter(PLOT == 1)

highest_freq_class= function(data){
  hfc = data.frame()
  for (i in 1:103){
    sitedata = data%>%filter(SITE == i)
    for (j in 1:16){
      plotdata = sitedata%>%filter(PLOT == j)
      if (length(plotdata$DBH_class) != 0){
        hfc[i,j]  = max(plotdata$Count)}
      else{
        hfc[i,j] = NA
      }
      
    }
  }
  return(hfc)
}
high_freq_df = highest_freq_class(DBH_Yr2_agg)
##############################
##Mode

getmode = function(DBH_Yr2_agg){
  modes_df = data.frame()
  for (i in 1:103){
    sitedata = data%>%filter(SITE == i)
    for (j in 1:16){
      plotdata = sitedata%>%filter(PLOT == j)
      uni_vals = unique()
        modes_df[i,j] = NA
      }
      
    }
  }
  return(hfc)
}
  uni_vals = unique()
}

Mode = vector()
for (n in length(u)){
  Mode[n] = 
  
}

