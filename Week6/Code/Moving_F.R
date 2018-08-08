#!/usr/bin/env Rscript
#Bioinformatics Week6

#R Studio Version 1.1.383 ubuntu 16.04 LTS 64bi
#Author Petra Guy 7th November 2017

library(dplyr)
library(ggplot2)
library(reshape2)
library(stats)
graphics.off()
rm(list = ls())

# read in data and make AA,Aa, frequency rows

args <- commandArgs(TRUE)
InputFileName <- as.character(args[1])
Outputfilename <- as.character(args[2])
pathnameIn = paste("../Data",InputFileName, sep = "/")
pathnameOut = paste("../Results",Outputfilename, sep = '/')
MyData = read.table(file = pathnameIn, header = TRUE)
#MyData <- read.table(file="../Data/H938_chr15.geno", header=TRUE) 


#do the frequencies thing
MyData <- mutate(MyData, nObs = nA1A1 + nA1A2 + nA2A2) 
MyData <- mutate(MyData, p11 = nA1A1/nObs , p12 = nA1A2/nObs, p22 = nA2A2/nObs )
MyData <- mutate(MyData, p1 = p11 + 0.5*p12, p2 = p22 + 0.5*p12)
MyData <- mutate(MyData, X2 = (nA1A1-nObs*p1^2)^2 /(nObs*p1^2) + (nA1A2-nObs*2*p1*p2)^2 / (nObs*2*p1*p2) + (nA2A2-nObs*p2^2)^2 / (nObs*p2^2))
MyData <- mutate(MyData,pval = 1-pchisq(X2,1))
MyData = mutate(MyData, F = (2*p1*(1-p1)-p12)/ (2*p1*(1-p1)))

#OMoving average
movingavg <- function(x, n=5){stats::filter(x, rep(1/n,n), sides = 2)} 
pdf(pathnameOut,11.7, 8.3)
Myplot = plot(movingavg(MyData$F), xlab="SNP number") 
print(Myplot)

