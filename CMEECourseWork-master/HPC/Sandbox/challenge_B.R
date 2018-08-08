
challenge_B = function(){
  
  v = 0.1
  t = 200
  size = 100
  repeats = 50
  num_communties = 10
  
  communities = data.frame()
  
  #generate some communites with random initial richnesses and store them
  #in a datframe called communties. Because expected richness for 100 selected
  #from 100 is 63.66, richness will always be around 64. To get a variety of richnesses
  # the number out of wich to select is altered
  for (i in 1:num_communties){
    set.seed(i)
    m = size/i
    community = generate_community(m,size)
    communities = cbind(communities, community,row.names = NULL)
   
  }
  
  #create a list of dataframes of richnesses for each community in communities
  #when is is processes through neutral_time_series_speciation repeat times
  
  richness_list = list()
  
  get_repeated_richnesses = function(a_comm,v,t){
    richness_df = data.frame()
    for (i in 1:repeats){
    richness = neutral_time_series_speciation(a_comm, v, t)
    richness_df = rbind(richness_df,richness)
    }
    return(richness_df)
  }
  
  for (i in 1:num_communties){
      richness_list[[i]] = get_repeated_richnesses(communities[,i],v,t)
  }
  
# we now have richness_list. Each element is a num repeats x t dataframe for each 
#initial community. Each row of the datafram is the richness value through time.
# There as many rows as we specified repeats to average over. As for challenge A
#we now take the  average for each column to give an average richness 
#over the time steps. No CI required this time
  ave_df =  data.frame(row.names = FALSE)
  for (i in 1:num_communties){
    ave = colSums(richness_list[[i]])/repeats
    ave_df = cbind(ave_df,ave,row.names = NULL )
  }
    
  x = (1:t)
  matplot (x, ave_df, pch = 19, cex = 0.3,
           xlab = "generations", ylab = "average richness", 
           main = "Average species richness for various starting communities",
           sub = "v = 0.1, community size = 100")
  
}