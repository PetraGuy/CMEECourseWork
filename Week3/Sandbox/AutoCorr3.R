#!/usr/bin/env Rscript
#Chapter 8.8 Final Practial


prepare_workspace = function(){
  rm(list = ls())
   graphics.off()
   library(dplyr)
}

# get the data and make two vectors coz easier for loops


  load("../Data/KeyWestAnnualMeanTemperature.RData")
  Years = ats[[1]]  
  Temps = ats[[2]]
  MyData = as.data.frame(cbind(Years,Temps))


  # plot simple time series

  autocorr = acf(Temps,1)[[1]][2]
  
  print("autoccorrelation coefficient for lag of 1 is ")
  print(autocorr)
  
  # repeating with the 1000 samples in a for loop
  acfs = vector("numeric",1000)
  for (i in 1:1000){
    acfs[i] = acf(sample(Temps,100))[[1]][2]
  }
 
  
  # reapeting using piping - but since acf gives a list, the answer is messh=y, hence unlist and select out alternate values
  acfs2 =vector("numeric",1000)
  for (i in 1:1000){ 
    acfs2[i] <- Temps %>% sample(.,100) %>% acf(.,1)
  }
acfs2 = unlist(acfs2[c(FALSE,TRUE)])

hist(acfs2, main = paste("distribution of random acfs. Our acf was 0.309", xlab = "autocorrelation coefficients"))

