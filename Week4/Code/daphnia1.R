# Stats with Sparrows - but daphni
#Week 4, stats in R

#R Studio Version 1.1.383 ubuntu 16.04 LTS 64bi
#Author Petra Guy 31st October 2017

library(dplyr)
rm(list = ls())
daphnia = read.delim("../Data/daphnia.txt")
summary(daphnia)

# plots to look at variation
par(mfrow = c(1,2))
plot(Growth.rate ~ Detergent, data = daphnia)
plot(Growth.rate~ Daphnia, data = daphnia)

#pipe into a sumamry of variances.The ratio of variances should be small
#e.g. less than 4. Thats not the case for clone1 to clone2 and 3
#as you can guess from the box plot above.
daphnia %>% group_by(Detergent)%>% summarise(variance=var(Growth.rate))
daphnia %>% group_by(Daphnia)%>% summarise(variance=var(Growth.rate))

# Need to bear that in mind as it bias bias least sqs estimators

#Looking at distribution of data

hist(daphnia$Growth.rate)
# regression assumes that variation in obs for each x is normal
# the residuaks, not the data

seFun = function(x){
  sqrt(var(x)/length(x))
}

detergentMean = with(daphnia, tapply(Growth.rate, INDEX = Detergent, FUN = mean))
detergentSEM = with(daphnia, tapply(Growth.rate, INDEX = Detergent, FUN = seFun))
cloneMean = with(daphnia, tapply(Growth.rate, INDEX = Daphnia, FUN = mean))
cloneSEM = with(daphnia, tapply(Growth.rate, INDEX = Daphnia, FUN = seFun))

par(mfrow = c(2,1), mar = c(4,4,1,1))
barMids = barplot(detergentMean, xlab = "Detergent Type", ylab = "Population growth rate", ylim = c(0,5))
arrows(barMids, detergentMean - detergentSEM, barMids, detergentMean + detergentSEM, code = 3, angle = 90)
barMids = barplot(cloneMean, xlab = "Daphnia clone", ylab = "Population growth rate",ylim = c(0,5) )
arrows(barMids, cloneMean - cloneSEM, barMids, cloneMean + cloneSEM, code = 3, angle = 90)

# the above plots the means for each group as a bar, with the variance of the mean.
#then you can look to see how df=ifferent each mean really is. run a lm with
#Detergent and Daphnia and look at ANOVA

daphniaMod = lm(Growth.rate ~ Detergent + Daphnia, data = daphnia)
anova(daphniaMod)

# The meansq detergent is small with large p - so not doing much explaining and not significant
#Sp the Daphnia variable is the important one. 

summary(daphniaMod)

# this shows that the means of the detergents are not very different (col 1)

# use the Tukey HSD test to look at all pairwise differences

daphniaAnovaMod = aov(Growth.rate ~ Detergent + Daphnia, data = daphnia)
summary(daphniaAnovaMod)
daphniaModHSD = TukeyHSD(daphniaAnovaMod)

par(mfrow = c(2,1), mar = c(10,10,2,2))
plot(daphniaModHSD)

par(mfrow = c(2,2))
plot(daphniaMod)

### from Intro to Statistics booklet. Look at scatterplots

plot(Growth.rate ~ Daphnia, data = daphnia)

