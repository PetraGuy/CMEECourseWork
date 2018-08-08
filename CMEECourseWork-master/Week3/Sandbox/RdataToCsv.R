

#Getting the Rdata into a csv file
Temperatures = load("../Data/KeyWestAnnualMeanTemperature.RData")
ats
Tempserature = cbind(ats[1],ats[2])

TempsFile = write.csv(Tempserature, "../Data/KeyWest.CSV")

data2 = (read.csv("../Data/KeyWest.CSV"))