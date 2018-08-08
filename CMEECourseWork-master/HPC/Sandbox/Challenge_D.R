

#Using coalescence, ChallengeD
rm(list = ls())
sizes  = c(500, 1000, 2500, 5000)
v = 0.002125
reps = 500
octets = rep(list(rep(0,6)),4)

initialise_min = function(x) {
  comm = rep(1, x)
  return(comm)
}
octaves = function(x) {
  oct = tabulate(floor(log2(x)) + 1)
  return(oct)
}

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
                 
                  main = "Average abundances in octets",
                  xlab = "abundances")
  y2 = octets[[2]]/reps
 
  plot2 = barplot(y2,
                 
                  main = "Average abundances in octets",
                  xlab = "abundances")
  y3 = octets[[3]]/reps
  
  plot3 = barplot(y3,
                  
                  main = "Average abundances in octets",
                  xlab = "abundances")
  y4 = octets[[4]]/reps
  
  barplot(y4,
          
          main = "Average abundances in octets",
          xlab = "abundances")

  