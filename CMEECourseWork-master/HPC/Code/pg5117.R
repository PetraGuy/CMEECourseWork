#!/usr/bin/env Rscript
#HPC Week

#Biological Computing Boot Camp
#R Studio Version 1.1.383 ubuntu 16.04 LTS 64bi
#Author Petra Guy November 2017

rm(list = ls())
graphics.off()

## generate a random community of size n, varies due to input of seed
generate_community = function(m,size) {
  comm = sample(c(1:m),
                size = size,
                replace = TRUE)
  return(comm)
}

############# QUESTION 1#########################
#Q1 get species richness
species_richness = function(x) {
  r = length(unique(x))
  return(r)
}
##################### QUESTION 2 ###############
#Q2 Get a maximally diverse community
initialise_max = function(x) {
  comm = seq(1:x)
  return(comm)
}
################### QUESTION 3 ##################
#Q3 get a minimally diverse community - everything is the same
initialise_min = function(x) {
  comm = rep(1, x)
  return(comm)
}

################# QUESTION 4 #################
#Q4 Need for neutral_step functions, just select two numbers from within the length of
#input vector which can then be used as indices for speciation or death replacement
choose_two = function(x) {
  two = sample(x, 2)
  return(two)
}
############### QUESTION 5 ######################
#Q5 Uses choose two and then replaces species at index 1 with that at index 2
#Simulates a species dieing and being repalced by another within community
neutral_step = function(x) {
  index = choose_two(length(x))
  x[index[1]] = x[index[2]]
  return(x)
}
############## QUESTION 6 ###################
#Q6 Simulates n steps of neutral step
neutral_generation = function(x) {
  n = round(length(x) / 2)
  for (i in 1:n) {
    comm = neutral_step(x)
  }
  return(comm)
}

################### QUESTION 7 ################
#Q7 Runs several generations and returns species richness a each step
neutral_time_series = function(x, t) {
  rich = c(species_richness(x))
  for (i in 1:t) {
    x = neutral_generation(x)
    rich = c(rich, species_richness(x))
    
  }
  return(rich)
}
############## QUESTION 8 #######################
#Q8 Uses neutral time series over 200 steps on a maximally diverse community and plots the richness
question_8 = function() {
  rich = neutral_time_series(initialise_max(100), 200)
  plot(rich,
       main = "Species richness starting with maximal diversity of 100 individuals",
       ylab = "species richness",
       sub = "Species richness will tend to 1 as all individuals eventually become the same")
}
############ QUESTION 9########################
#Q9 creates a neutral step with either a speciation or a replacement, depending on value of v
neutral_step_speciation = function(x, v) {
  p = runif(1)
  if (v < p) {
    index = choose_two(length(x))
    x[index[1]] = x[index[2]]
  }
  else {
    newspecies =  max(x) + 1
    index = sample((length(x)), size = 1, replace = TRUE)
    x[index] = newspecies
  }
  return(x)
}
############### QUESTION 10 #############################
#Q10 Takes a community and outputs new community after a few generations with speciation or replacement
neutral_generation_speciation = function(x, v) {
  n =  round(length(x) / 2)
  for (i in 1:n) {
    x = neutral_step_speciation(x, v)
  }
  return(x)
}
################ QUESTION 11 #########################
#Q11 Returns community richness at each generation
neutral_time_series_speciation = function(x, v, t) {
  rich = vector()
  for (i in 1:t) {
    x = neutral_generation_speciation(x, v)
    rich[i] = species_richness(x)
  }
  return(rich)
}
################ QUESTION 12 ########################
#Q12 Output graph for species richness over 200 generations for maximally and minimally diverse communites
question_12 = function() {
  t = 200
  v = 0.1
  comm_max = initialise_max(100)
  comm_min = initialise_min(100)
  rich_max = neutral_time_series_speciation(comm_max, v, t)
  rich_min = neutral_time_series_speciation(comm_min, v, t)
  
  x = (1:t)
  titles = c("Max", "Min")
  y =  cbind(rich_max, rich_min)
  colnames(y) = titles
  
  matplot (x, y, pch = 19, col = 1:2)
  legend(1,
         50,
         legend = colnames(y),
         col = 1:2,
         lty = 1:4)
}
################ QUESTION 13 ########################
#Q13 calcualte species abundance using table - which gives frequencies
species_abundance = function(x) {
  abundance = as.numeric(sort(table(x), decreasing = TRUE))
  return(abundance)
}

############ QUESTION 14 #######################
#Q14 Arrange the abundances into octets
octaves = function(x) {
  oct = tabulate(floor(log2(x)) + 1)
  return(oct)
}

########### QUESTION 15 ########################
#Q15 Need to add the octets produced by octaves() and average them, but each octet can be a different
#length, this function pads the shorter octet with zeros
sum_vect = function(x, y) {
  if (length(x) < length(y))    {
    short = x
    long = y
    newshort = c(x, rep(0, length(long) - length(short)))
    sum = newshort + long
  }   else if (length(x) > length(y)) {
    short = y
    long = x
    newshort = c(y, rep(0, length(long) - length(short)))
    sum = newshort + long
  }   else  {
    sum = x + y
  }
  return(sum)
}

####################### QUESTION 16 ###########################
#Q16, produce a barchart of octets after 200 generations burn-in, using another 2000 generations
#using v = 0.1, size = 100

question_16 = function() {
  #browser()
  octets = list()
  x = initialise_min(100)
  v = 0.1
  rich = vector()
  i = 1
  index = 1
  # burn in
  while (i < 2200) {
    if (i < 200) {
      x = neutral_generation_speciation(x, v)
      
    } else {
      # continue for 2000 cycles
      x = neutral_generation_speciation(x, v)
      if (i %% 20 == 0) {
        abundance = species_abundance(x)
        octets[[index]] = octaves(abundance)
        index = index + 1
      }
      
    }
    i = i + 1
  }
  
  #find the average of the octaves which are list elements of octets
  l = length(octets)
  sum = vector()
  for (a in 1:(l)) {
    sum = sum_vect(sum, octets[[a]])
    
    
  }

  ave = sum / l
  names = names = c("1-3", "4-7", "8-31", "32-63", "64-127", ">127")
  barplot(ave,
          names.arg = names,
          main = "Average abundances in octets",
          xlab = "abundances")
  print(ave)
}
get_series_average = function(richness_vector){
  avg = vector()
  avg = c(avg,richness_vector[1])
  for ( i in 1:(length(richness_vector)-1)){
    i = i + 1
    tmpvect = richness_vector[1:i]
    total = sum(tmpvect)
    avgerage = total/length(tmpvect)
    avg= c(avg, avgerage)
  }
  return(avg)
}

######################  Challenge A #######################################

#Calculate species ricchness at each time interval and store the values.
#Repeat.
#Take average richness at each time step.

challenge_A = function(){
t = 200
v = 0.1
repeats = 500

#get two starting communities
comm_max = initialise_max(100)
comm_min = initialise_min(100)

#initialise richness vector
richness_max_vect = vector(length = t)
richness_min_vect = vector(length = t)

#initialise df for storing output for laer calculations
richness_df_max = data.frame()
richness_df_min = data.frame()


for (i in 1:repeats) {
  
  #get times series of richness for them both - repeat this and store values for each repeat
  rich_max = neutral_time_series_speciation(comm_max, v, t)
  rich_min = neutral_time_series_speciation(comm_min, v, t)
  
  richness_df_max = rbind(richness_df_max,rich_max)
  richness_df_min = rbind(richness_df_min,rich_min)
  
  
}
#can be useful to name cols - otherwise first row can become a header
cols = c(1:t)
colnames(richness_df_max) = cols
colnames(richness_df_min) = cols
#calcualte the standard deviations and averages using the dataframe of stored richnesses
#because each run is a row the cols are used for sd and ave.
sd_max = sqrt(apply(richness_df_max,2,var))
sd_min = sqrt(apply(richness_df_min,2,var))
avg_max = colSums(richness_df_max)/repeats
avg_min = colSums(richness_df_min)/repeats

#for 97.2% CI we need 98.6th percentile = z of 2.2
CI_upper_max = avg_max + 2.2*sd_max
CI_lower_max = avg_max - 2.2*sd_max

CI_upper_min = avg_min + 2.2*sd_min
CI_lower_min = avg_min - 2.2*sd_min

#plot the graphs

x = (1:t)
titles = c("Maximum initial richness", "Minimum initial richness")
y =  cbind(avg_max, avg_min)

matplot (x, y, pch = 20, col = 1:2, xlim = c(0,t), ylim = c(0,100),
         xlab = "time", ylab = "Average species richness",
         main = "Average species richness for 500 repeats of neutral_time_series_speciation")
legend(50,
       100,
       legend = titles,
       col = 1:2,
       lty = 1:4)

lines(CI_upper_max)
lines(CI_lower_max)
lines(CI_upper_min, col = "red")
lines(CI_lower_min, col = "red")

}
################ CHALLENGE B #############################
# We want to repeat challenge A, but use different startting communities
#not just min and max. The function generates a community with a random richness.
challenge_B = function(){
  
  v = 0.1
  t = 200
  size = 100
  repeats = 50
  num_communties = 10
  
  communities = data.frame(row.names = FALSE)
  
  #generate some communites with random initial richnesses and store them
  #in a datframe called communties. Because expected richness for 100 selected
  #from 100 is 63.66, richness will always be around 64. To get a variety of richnesses
  # the number out of wich to select is altered
  for (i in 1:num_communties){
    set.seed(i)
    m = size/i
    community = generate_community(m,size)
    communities = cbind(communities, community)
    
  }
  
  #create a list of dataframes of richnesses for each community in communities
  #when is is processes through neutral_time_series_speciation repeat times
  
  richness_list = list()
  
  get_repeated_richnesses = function(a_comm,v,t){
    richness_df = data.frame()
    for (i in 1:repeats){
      richness = neutral_time_series_speciation(a_comm, v, t)
      richness_df = rbind(richness_df,richness, row.names = NULL)
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
    ave_df = cbind(ave_df,ave,row.names = NULL)
  }
  
  x = (1:t)
  matplot (x, ave_df, pch = 19, cex = 0.3,
           xlab = "generations", ylab = "average richness", 
           main = "Average species richness for various starting communities",
           sub = "v = 0.1, community size = 100")
  
}
################# QUESTION 17 AND 18 ####################################
# THE FULL PROGRAMME AS IT WAS LOADED ONTO THE HPC SAVED SEPARATWLY

################ QUESTION 19 #######################################
#BASH SCRIPT, 
#R CODE
#OUTPUT FILES
# ALL IN ZIP

# SEE /RESULTS/Cluster_1207 for zip and /RESULTS/Cluster_1207/pg5117 for files


############# Question 20 ##########################################
# Read in the files from the HPC
#couldnt work out easy wy of specifying files in each batch, so this first function

get_quartile = function(i) {
  if (i == 1) {
    quartile = c(1:25)
  }
  else if (i == 2) {
    quartile = c(26:50)
  }
  else if (i == 3) {
    quartile = c(51:75)
  }
  else {
    quartile = c(76:100)
  }
  return(quartile)
}
# This function returns the sum and number of the octets in single file, to be found in path specified
get_sum_and_length_of_octet = function(i) {
  #browser()
  infile = paste("../Results/Unzipped/pg5117_cluster_", i, ".rda", sep = "")
  load(infile)
  len = length(octets)
  sum = vector()
  sum_and_length = list()
  for (a in 1:len) {
    sum = sum_vect(sum, octets[[a]])
  }
  sum_and_length[[1]] = sum
  sum_and_length[[2]] = len
  return(sum_and_length)
  
}


get_results = function() {
  #browser()
  ave_for_file_batch = list()
  for (i in 1:4) {
    j = get_quartile(i)    # this return vector of file numbers in each batch, eg if i = 2, 26:50
    cum_sum = vector()
    cum_len = 0
    results = list()
    for (counts in j) {
      results =  get_sum_and_length_of_octet(counts)
      cum_sum = sum_vect(cum_sum, results[[1]])
      cum_len =  results[[2]] + cum_len
    }
    ave_for_file_batch[[i]] = cum_sum / cum_len
    
  }
  return(ave_for_file_batch)
}

plot_results = function(results) {
  par(mfrow = c(2, 2))
  ave1 = results[[1]]
  plot1 = barplot(ave1,
                  main = paste(round(results[[1]], 3), collapse = ","),
                  xlab = "abundances, size = 500")
  
  ave2 = results[[2]]
  plot2 = barplot(ave2,
                  main = paste(round(results[[2]], 3), collapse = ","),
                  xlab = "abundances, size = 1000")
  
  ave3 = results[[3]]
  plot3 = barplot(ave3,
                  main = paste(round(results[[3]], 3), collapse = ","),
                  xlab = "abundances, size = 2500")
  
  ave4 = results[[4]]
  barplot(ave4,
          main = paste(round(results[[4]], 3), collapse = ","),
          xlab = "abundances, size = 5000")
}
#un-commentto run this funtion
 
#plot_results(get_results())


##########CHALLENGE C ##########################

######################## CHALLENGE D #################


challenge_D = function() {
  sizes  = c(500, 1000, 2500, 5000)
  v = 0.002125
  reps = 500
  octets = rep(list(rep(0,6)),4)
  
  for (j in 1:reps){
    #browser()
    octet = list()
    
    for (i in 1:4){
      
      size = sizes[i]     # set size to 500,1000 etc
      theta = v * (size - 1) / (1 - v)
      abundance = vector()
      lineages = initialise_min(size)  # initialise lineagse to min of size
      N = size
      
      while (N > 1) {
        p = theta / (theta + N - 1)
        r = runif(1, 0:1)
        index = sample(N, 2)
        
        if (r < p) {
          abundance = c(abundance, lineages[index[2]])
        }
        else {
          lineages[index[1]]=lineages[index[1]] + lineages[index[2]]
          
        }
        lineages = lineages[-index[2]]
        N = length(lineages)
      }
      
      abundance = sort(c(abundance, lineages), decreasing = TRUE)
      octet[[i]] = octaves(abundance)
      x = octet[[i]]
      y = octets[[i]]
      sum = sum_vect(x,y)
      octets[[i]] = sum
      
    }
    
  }
  
  par(mfrow = c(2, 2))
  y1 = octets[[1]]/reps
  
  plot1 = barplot(y1,
                  main = paste(round(y1, 3), collapse = ","),
                  xlab = "abundances, size = 500")
  y2 = octets[[2]]/reps
  
  plot2 = barplot(y2,
                  main = paste(round(y2, 3), collapse = ","),
                  xlab = "abundances, size = 1000")
  y3 = octets[[3]]/reps
  
  plot3 = barplot(y3,
                  main = paste(round(y3, 3), collapse = ","),
                  xlab = "abundances, size = 2500")
  y4 = octets[[4]]/reps
  
   plot4 = barplot(y4,
                  main = paste(round(y4, 3), collapse = ","),
                  xlab = "abundances, size = 5000")

}
############## Fractals ###################################

########## Question 19 Fractals ###########################



chaos_game = function(){
  graphics.off()
  #browser()
  x = vector()
  y = vector()
  X <- list(c(0,0),c(3,4),c(4,1))
  coord = X[[1]]
  x1 = coord[[1]]
  y1 = coord[[2]]
  plot(NA, xlim=c(0,5), ylim=c(0,5), xlab="X", ylab="Y")
  points(x1,y1, cex = 0.2)
  for (i in 1:1000){
    index = sample((1:3),1)
    coord = X[[index]]
    x2 = coord[[1]]
    y2 = coord[[2]]
    x1 = (0.5*x2 + 0.5*x1)
    y1 = (0.5*y2 + 0.5*y1)
    x = c(x, x1)
    y = c(y, y1)
    
  }
  plot(x , y, cex = 0.2)
}

########## Question 20 Fractals #############

turtle = function(start, distance, direction){
  x1 = start[1]
  y1 = start[2]
  x2 = x1 + distance*cos(direction)
  y2 = y1 + distance*sin(direction)
  segments(x1,y1,x2,y2)
  coords = c(x2,y2)
  return(coords)
}

################ Question 21 Fractals ###########

elbow = function(start, distance, direction){
  coords = turtle(start, distance, direction)
  direction = (direction  - pi/4)
  distance = 0.95*distance
  coords = turtle(coords, distance, direction)
  
}

########### Question 22 fractals ############

spiral = function(start, distance, direction){
  coords = turtle(start, distance, direction)
  direction = (direction - pi/4)
  distance = 0.95*distance
  spiral(coords,distance,direction)
  
}

~############## Question 23 Fractals ############

plot(NA, xlim=c(0,2.5), ylim=c(0,3.5), xlab="X", ylab="Y")
spiral_2 = function(start, distance, direction){
  coords = turtle(start, distance, direction)
  direction = (direction - pi/4)
  distance = 0.95*distance
  if (distance > 0.01){
    spiral_2(coords,distance,direction)
  }
}
start = c(0,2)
spiral_2(start,1,pi/4)

############# Question 24 Fractals ##############

#plot(NA, xlim=c(0,10), ylim=c(0,10), xlab="X", ylab="Y")
tree = function(start,distance,direction){
  coords = turtle(start, distance, direction)
  direction1 = (direction - pi/4)
  direction2 = (direction + pi/4)
  distance = 0.65*distance
  if (distance > 0.1){
    tree(coords,distance,direction1)
    tree(coords,distance,direction2)
  }
}
#start = c(5,0)
#tree(start,3,pi/2)

###########Question 25 Fractals #########################

#plot(NA, xlim=c(1,9), ylim=c(0,7), xlab="X", ylab="Y")
fern = function(start,distance,direction){
  coords = turtle(start, distance, direction)
  direction1 = direction + pi/4
  direction2 = pi/2
  if (distance > 0.1){
    fern(coords,0.87*distance, direction1)
    fern(coords,0.38*distance, direction2)
  }
}
#start = c(8,0)
#fern(start,2,pi/2)

############## Question 26 Fractals #######

#plot(NA, xlim=c(0,10), ylim=c(0,10), xlab="X", ylab="Y")
fern_2 = function(start,distance,direction,dir){
  
  coords = turtle(start, distance, direction)
  direction1 = direction - (pi/4)*dir
  dir = dir*-1
  if (distance > 0.1){
    fern_2(coords,0.38*distance,direction1,dir)
    fern_2(coords,0.87*distance,direction,dir)
  }
}
#start = c(5,0)
#fern_2(start,2,pi/2,-1)

########## Additional Fractals Challeeg F ##############

#Colourful Tree
#plot(NA, xlim=c(0,20), ylim=c(0,12), xlab="X", ylab="Y")

turtle1 = function(start, distance, direction){
  x1 = start[1]
  y1 = start[2]
  x2 = x1 + distance*cos(direction)
  y2 = y1 + distance*sin(direction)
  if (distance > 0.5){
    col = "brown"
    lwd = 3
  }else {
    col = "green"
    lwd = 0.5
  }
  segments(x1,y1,x2,y2,col = col,lwd =lwd)
  coords = c(x2,y2)
  return(coords)
}

tree_mod = function(start,distance,direction){
  coords = turtle1(start, distance, direction)
  direction1 = (direction - pi/5)
  direction2 = (direction + pi/5)
  distance = 0.75*distance
  if (distance > 0.05){
    tree_mod(coords,distance,direction1)
    tree_mod(coords,distance,direction2)
  }
}
#start = c(10,0)
#tree_mod(start,3,pi/2)

################################

# curly fern

#plot(NA, xlim=c(1,9), ylim=c(0,7), xlab="X", ylab="Y")
fern_mod = function(start,distance,direction){
  coords = turtle(start, distance, direction)
  direction1 = (direction + pi/8)
  direction2 = 0.9*direction
  if (distance > 0.1){
    fern_mod(coords,0.87*distance,direction1)
    fern_mod(coords,0.5*distance,direction2)
  }
}
#start = c(8,0)
#fern_mod(start,2,pi/2)

##################################

#bushy fern
#factor = 0.38
#plot(NA, xlim=c(0,40), ylim=c(0,40), xlab="X", ylab="Y")
fern_2_mod = function(start,distance,direction,dir){
  #browser()
  coords = turtle(start, distance, direction)
  factor = factor*1.2
  direction1 = direction - (pi/4)*dir
  dir = dir*-1
  if (distance > 0.05){
    fern_2_mod(coords,factor*distance,direction1,dir)
    fern_2_mod(coords,0.87*distance,direction,dir)
  }
}
#start = c(20,0)
#fern_2_mod(start,5,pi/2,-1)

############ Challenge G #########

#plot(NA, xlim=c(0,30), ylim=c(0,30), xlab="X", ylab="Y")
fern_2 = function(start,distance,direction,dir){
  
  coords = turtle(start, distance, direction)
  if (distance > 0.1){
    fern_2(coords,0.38*distance,direction - (pi/4)*dir,dir*-1)
    fern_2(coords,0.87*distance,direction,dir*-1)
  }
}
#fern_2(c(15,0),5,pi/2,-1)
# Not very small

############### THE END #########################



