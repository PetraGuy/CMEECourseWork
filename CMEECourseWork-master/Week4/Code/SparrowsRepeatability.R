
#Sparrows, repeatability, pdf14
library(plyr)
# for repeatability need means sum squares bewtween and mean sum squares between and number of things in each group.

# to caculate number of things in each group - e.g. how many birds of same ID were repeatably measured.

#we know
unique = length(unique(SparrowsData$BirdID)) # = 636. # but how many of sparrow no1, no2 are there?

BirdID_freq =  count (SparrowsData$BirdID) # this gives frequency table for each id

# need to square and sum each frequcency
sqsum = 0
for (i in 1:unique){
  sq = (BirdID_freq[i,2])^2 
  sqsum = sqsum + sq
}

# total number of things measured
tot = length(SparrowsData$BirdID)

# next term in calculation of no

term = tot - sqsum/tot

#finally divide by no.group1 -1 = unique

n0 = term/unique

#now to get repeatability, which needs n0

#say you were lookinhg at wing of different birds, Wing_lm2 was Wing ~ BirdID
#anova(Wing_lm2) give MeanWSq Bird = 13.207, MeanSq Residuals = 1.6156
anova_Wing_lm2 = anova(Wing_lm2)

MSamoung = anova_Wing_lm2$`Mean Sq`[1] # amoung birds of different groups
MSwithin = anova_Wing_lm2$`Mean Sq`[2] # within each group - residuals

Sa2 = (MSamoung - MSwithin)/n0
Sw2 = MSwithin

r = Sa2/(Sw2 + Sa2) # r = 0.72, 72% of winglength variation is due to between individual bird differences. 
# not within each bird - i.e. same bird has same wing length, does not vary and is repeatibly measured.
# different birds are more different than individual birds.



