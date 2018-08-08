# 9.4.2 Debugging using browser

#Biological Computing Boot Camp
#R Studio Version 1.1.383 ubuntu 16.04 LTS 64bi
#Author Petra Guy 21th October 2017

Exponential <- function (N0 = 1, r =1, generations = 10){
  N <- rep(NA, generations)
  
  N[1] <-N0
  for (t in 2:generations) {
    N[t] <- N[t-1]*exp(r)
    browser()
    }
  return (N)
}

plot(Exponential(), type = "l", main = "Exponential growth")