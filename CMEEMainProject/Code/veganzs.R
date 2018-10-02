
rm(list = ls())
cat("\014")
library(vegan)

mu = 0
minmax = 0
for (i in 1:1000){
 r = rnorm(10, 20, 5)
  mu = mu +mean(r)
 minmax = minmax + (min(r)+((max(r) - min(r))/2))
}
mu = mu/1000
minmax = minmax/1000
mu
minmax

#test the vegan method..

data = readRDS("petras_presence_absence.RDS")
areas = seq(200,3200,200)
x = log(areas)

veganzs = vector()
for (i in 1:103){
  site = data[[i]]
  specacc= specaccum(site,method="exact")
  r = specacc$richness
  l = length(r)
  area = areas[1:l]
  model = lm(log(r)~log(area))
  veganzs[i] = model$coefficients[[2]]
}

veganzs = saveRDS(veganzs,"veganzs.RDS")

randzsdf = data.frame(matrix(nrow = 103, ncol = 100))
for (j in  i:100){
veganzsrandom = vector()
for (i in 1:103){
  site = data[[i]]
  specacc= specaccum(site,method="random")
  r = specacc$richness
  l = length(r)
  area = areas[1:l]
  model = lm(log(r)~log(area))
  veganzsrandom[i] = model$coefficients[[2]]
}
randzsdf[,13] = veganzsrandom
}

z_ave = read.csv("../Data/z_ave_fits.csv")

veganrandzs = saveRDS(veganzsrandom,"veganrandomz.RDS")

ranges = apply(randzsdf[,c(1:10)],1,range)

getdiff = function(df){
  max = apply(df,1,max)
  min = apply(df,1,min)
  diffs = ((max-min)/max)*100
  return(diffs)
}

diffs = getdiff(randzsdf[,c(1:10)])
diffs

mean = apply(randzsdf,1,mean,na.rm =TRUE)
df = as.data.frame(cbind(mean,z_ave$slope))
