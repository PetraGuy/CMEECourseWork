###Try to fit the z's from the ave path to some variables

rm(list = ls())
cat("\014")
library(dplyr)
library(ggplot2)


path_min = readRDS("path_min.RDS")
path_max = readRDS("path_max.RDS")
ave_data_fits = read.csv("../Data/z_ave_fits")
AllplotVars =  read.csv("../Data/AllPlotsVarsRichness.csv")


