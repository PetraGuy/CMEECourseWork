
#question 20 redone for the files back from the hpc - i.e. 100 files
#files 25,26,50,51,75,76 are missing, so adjusted for those



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
#couldnt work out easy wy of specifying files in each batch, so this
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
results =  get_results()
plot_results(results)


#files = Sys.glob(file.path("/home/petra/Documents/CMEECourseWork/HPC/Results/LocalResults/", "*.rda"))