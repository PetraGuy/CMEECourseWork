# Stats with Sparrows - but timber
#Week 4, stats in R

#R Studio Version 1.1.383 ubuntu 16.04 LTS 64bi
#Author Petra Guy 31st October 2017

rm(list = ls())
timber = read.delim("../Data/timber.txt")
summary(timber)

par(mfrow =c(2,2))
boxplot(timber$volume)
boxplot(timber$girth)
boxplot(timber$height)

var(timber$volume)
var(timber$girth)
var(timber$height)

t2 = as.data.frame(subset(timber, timber$volume!="NA"))
t2$z.girth = scale(timber$girth)
t2$z.height = scale(timber$height)

par(mfrow = c(2,2))
hist(t2$volume)
hist(t2$girth)
hist(t2$height)

pairs(timber) # plot the variates next to each other to examine colinearity

cor(timber) #this gives the correlation

# Look at the Variance Inflation Factor. If one covariate is set as the
#y and the others are modelled against it...

s = summary(lm(girth ~ height, data = timber))
model = lm(girth ~ height, data = timber)
VIF = 1/(1-(s$r.squared^2))

timberMod = lm(volume ~ girth + height, data = timber)
anova(timberMod) # apparently this shows height is needed, but I'm not sure why

#I want to look at differet linear models

mod1 = lm(volume~girth, data = timber)
mod2 = lm(volume~height, data = timber)
anova(mod1)
anova(mod2)

#I dont understand the statement in the handout that ht and girth are needed. The lm for
#vol ~ girth is same as vol~ht+girth, so why put the ht in?

plot(timberMod)
timberplot  = ggplot(timber, aes(x = girth, y = volume))+
  geom_smooth()+
  geom_point()

#1. Remove the point with high leverage - no.31

timberNew = timber[-31,]

timberNewMod = lm(volume~girth, data = timberNew)
timberplot  = ggplot(timberNew, aes(x = girth, y = volume))+
  geom_smooth()+
  geom_point()+
  geom_abline(slope =  0.04321, intercept = -2.41213)
              
anova(timberNewMod)

# This doesnt look a lot better - but the data does look exponetiol. lets try log plot

timberlogMod = lm(log(volume)~girth, data = timberNew)
plot(timberlogMod)
anova(timberlogMod)
ggplot(timberNew, aes(x = girth, y = log(volume)))+
  geom_smooth()+
  geom_point()

# add an interaction term
timberMod2 = lm(volume ~ girth + height*girth, data = timberNew)
anova(timberMod2)
timberMod2
anova(timberMod)

# nope - the best model is just Vol~ height