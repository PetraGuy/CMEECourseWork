
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
