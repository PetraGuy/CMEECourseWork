
#Sparrows ANOVA

SparrowsData = as.data.frame(read.table("../Data/SparrowSize.txt", header = TRUE))

# run a lm on wing and sex

Wing_lm = lm(Wing ~ as.numeric(Sex), SparrowsData)
Summary(Wing_lm) # gives the R2 and F stats etc
anova(Wing_lm) # as summary but only shows Sum Sq and df for residuals and expl var
#For this model R2  0.27, p = 2.26E-16, i.e not much variance explained and means equal 
t.test(SparrowsData$Wing~SparrowsData$Sex)
# t test shows that means also same, because CI overlaps the difference in means - confirms above ANOVA

# what about different birds - bird ID is the treatment/expl var

Wing_lm2 = lm(Wing ~ as.factor(BirdID), SparrowsData)
summary(Wing_lm2)
anova(Wing_lm2) 


length(unique(SparrowsData$BirdID)) # there are lots of birdID, so lots of groups, but some only have 1 bird
# So not sure how weird this makes anova?


plot_Mass_Year = ggplot(data=subset(SparrowsData, !is.na(Mass)), aes(as.factor(Year), Mass), SparrowsData)+
  geom_boxplot()
 # ps need as.factor year coz year is continuous var. 
#now use anova to see if there's any differences in mass between years

lm_Year_Mass = lm(Mass~as.factor(Year), SparrowsData)
summary(lm_Year_Mass)
anova(lm_Year_Mass)
# ss between years less than ss in each year. R2 small p small, so there is a difference, but lots of variation

#Exercies - run a lm excluding year 2000, because ALL other years are ~ 2000 -1, some error in 2000?

Sparrow_2000 = subset(SparrowsData, Year != 2000)
lm_SparrowMass_2000 = lm(Mass~ as.factor(Year), Sparrow_2000)
summary(lm_SparrowMass_2000)
anova(lm_SparrowMass_2000)

# now the p value is huge and the R2 small, so no difference

  

                             