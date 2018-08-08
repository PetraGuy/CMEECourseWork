
# trying to understand F!!


cf_site83 = cum_rich_all[[83]]
cf_site83 = cf_site83[-16,]
nest1 = cf_site83[,1]
nest2 = cf_site83[,2]
nest3 = cf_site83[,3]
nest4 = cf_site83[,4]
nest5 = cf_site83[,5]
mean_nest1 = mean(nest1)
mean_nest2 = mean(nest2)
mean_nest3 = mean(nest3)
mean_nest4 = mean(nest4)
mean_nest5 = mean(nest5)

SSW1 = sum((nest1 - mean_nest1)^2)
SSW2 = sum((nest2 - mean_nest2)^2)
SSW3 = sum((nest3 - mean_nest3)^2)
SSW4 = sum((nest4 - mean_nest4)^2)
SSW5 = sum((nest5 - mean_nest5)^2)
SSW = SSW1+SSW2+SSW3+SSW4+SSW5


grand_mean = (sum(nest1)+sum(nest2)+sum(nest3)+sum(nest4)+sum(nest5))/(75)

SSB1 = 15*((mean_nest1 -grand_mean)^2)
SSB2 = 15*((mean_nest2 -grand_mean)^2)
SSB3 = 15*((mean_nest3 -grand_mean)^2)
SSB4 = 15*((mean_nest4 -grand_mean)^2)
SSB5 = 15*((mean_nest5 -grand_mean)^2)
SSB = SSB1+SSB2+SSB3+SSB4+SSB5

DFB = 4
DFW = 14*5

MSB = SSB/DFB
MSW =SSW/DFW

F_stat = MSB/MSW

y = log(melted_cf$value)
x = log(melted_cf$area)

site83lm = lm(y~x)
anova(site83lm)





