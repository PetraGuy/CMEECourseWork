#!/usr/bin/env Rscript

loadRData <- function(fileName){
  #loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}
d <- loadRData("../Data/KeyWestAnnualMeanTemperature.RData")