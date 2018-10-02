setwd("C:/dev/code/CMEECourseWork/CMEEMainProject/Code")
library(dplyr)
rm(list = ls())
cat("\014")

#This to look at ave no species shared between "same" and "different" plot types.
#because Baselga said pairwise differences could not distinguish between nested and turnover
#nested = same NVC
#turnover = different NVC
#I used same and different NVC is same wood - to avoid additional differences in species 
#due to geographical ranges.

#In the end pairwise difference between same NVCs is on ave 12
#for different NVC is 7
#therefore using empirical data to supply simulated nested and turnover assemblages
#shows that pairwise difference applied to realistic species assemblages
#does differentiate between nested - ave pairwise mean is higher - and turnover - 
#average pairwise mean is lower.
#I.e. - less species are shared when there is turnover than in nested.

sites =  c(1:103)
zeta_r = readRDS("Zeta/zeta_r")
zeta_r = as.data.frame(cbind(sites,zeta_r))


ground_flora = read.csv("../Data/GroundCover.csv")
ground_flora = ground_flora%>%filter(Yr_2 == 2)
veg_codes = read.csv("../Data/vegetation_codes.csv")
colnames(ground_flora) = c("SITE", "PLOT","NEST","Cover","BRC_number","Year")
colnames(veg_codes) = c("Species", "BRC_number")
flora = ground_flora%>% inner_join(veg_codes)
Richness = read.csv("../Data/SiteRichness.csv")
CompleteSiteLevelvars = read.csv("../Data/CompleteSiteLevelVars.csv")
plotvars = read.csv("../Data/AllPlotsVarsRichness.csv")
colnames(flora) = c("Site","Plot","NEST" , "Cover","BRC_number","Year", "Species" )
allspecies = flora%>%select(Site,Plot,Species)



#A species list per code
#Returns list for all codes

getNVCspecies =  function(){
NVClist = as.character(unique(plotvars$ShortNVC))
Specieslist = list()
for (NVC in 1:length(NVClist)){
  code = NVClist[NVC]
  siteplots = plotvars%>%filter(ShortNVC == code)%>%select(Site,Plot)
  Specieslist[[code]] = inner_join(siteplots,allspecies)
  
}
names(Specieslist) = NVClist
return(Specieslist)
}


#Now Specieslist$W10 is a list of all species in W10 plots
#So we can look at overlap between each plot type and within each plot type
#But use most common codes, otherwise 44! ways of doing this
#common codes = w10, ov27, 

commonNVC = c("W10","OV27","W6","W8","W16","W21")

overlapdata = data.frame(matrix(ncol = 6,nrow = 6))
colnames(overlapdata) = commonNVC
rownames(overlapdata) = commonNVC

for (NVC in 1:length(commonNVC)){
  code = commonNVC[NVC]
  common = commonNVC[-match(code,commonNVC)]
  for (i in 1:length(common)){
    subcode = common[i]
    overlap = length(intersect(Specieslist[[code]][["Species"]],Specieslist[[subcode]][["Species"]]))
    overlapdata[code,subcode] = overlap
    
  }
}

#######Look at overlaps between plots of W10 and OV27 to see how much overlap there generaly is

#get all the species in an NVC code - will have repeats for different plots
getspecieslists = function(NVC){
  NVCplots = plotvars%>%filter(ShortNVC == NVC)
  NVClist = NVCplots%>%select(Site,Plot)
  NVCspecies = inner_join(NVClist,allspecies)
  return(NVCspecies) }

#split up into different plots

getseparatelists = function(NVCspecies) {
  sitenos = unique(NVCspecies$Site)
  Separatelist = list()
  
  for (i in 1:length(sitenos)) {
    #browser()
    site = sitenos[i]
    var = NVCspecies%>%filter(Site == site)
    plots = unique(var$Plot)
    count = 1
    for(j in 1:length(plots)) {
      plot = plots[j]
      Separatelist[[count]] = NVCspecies%>%filter(Site == site)%>%filter(Plot == plot)
      count=count+1
    } 
  }
  return(Separatelist)
}

  
# now get species list by NVC code = getspecieslist, then make separate lists
#eg w10species = getspecieslists("W10)
# w10list = getseparatelistss(w10species)


##################
#Now look at overlaps in the separate lists, which is zeta 2


getoverlaps = function(separatelist){
  nCr = choose(length(separatelist),2)
  #browser()
  totaloverlap = 0
  
  for (i in 1:length(separatelist)){
   thisplot = separatelist[[length(separatelist)]]
   separatelist[[length(separatelist)]] = NULL
   
   if (length(separatelist) == 0) {
     break
   }
   
   for (j in 1:length(separatelist)){
     
     if (j > length(separatelist)){
       break
     }
     overlap = length(intersect(thisplot$Species, separatelist[[j]]$Species))
     totaloverlap = totaloverlap + overlap
   }
  }
  
  aveoverlap = totaloverlap/nCr
  return(aveoverlap) 
  }

######
getNVCoverlap = function(NVC){
  specieslist = getspecieslists(NVC)
  seplist = getseparatelists(specieslist)
  overlap = getoverlaps(seplist)
  return(overlap)
}

########


############
#now get overlaps for two different NVC

getcut = function(l1,l2){
  if (l1 < l2){
    shortest = l1
    cut = l2 - l1
  } else {
    shortest = l2
    cut = l1 - l2
  }
  return(cut)
}

getfixedlengthlist = function(cut,list){
  noelements = cut - 1
  elements = sample(1:noelements,noelements)
  fixedlist = list()
  for (i in 1:noelements){
    n = elements[i]
    fixedlist[[i]] = list[[n]]
  }
  return(fixedlist)
}
  
getdiffoverlap = function(list1,list2){
  totaloverlap = 0
  l1 = length(list1)
  l2 = length(list2)
  times = l1*l2
  for (i in 1:l1){
    thisplot = list1[[i]]
    for (j in 1:l2){
      overlap =  length(intersect(thisplot$Species, list2[[j]]$Species))
      totaloverlap = totaloverlap + overlap
    }
  }
  aveoverlap = totaloverlap/times
  return(aveoverlap)
}
#########
getdiffcodeoverlap = function(list1,list2){
  l1 = length(list1)
  l2 = length(list2)
  toremove = getcut(l1,l2)
  fixed1 = getfixedlengthlist(cut,list1)
  fixed2 = getfixedlengthlist(cut,list2)
  overlap = getdiffoverlap(fixed1,fixed2)
}
################

##since you expect less overlap in distance locations
#what about overlap within the same wood for same/diff codes

getspeciesforplot = function(site,noplots){
  specieslist = list()
  for (i in 1:noplots){
    plot=i
    specieslist[[i]] = allspecies%>%filter(Site==site)%>%filter(Plot==plot)
  }
  return(specieslist)
}

getcodekey = function(thisite){
  noplots = length(thissite$Plot)
 
  
}
getplotkey = function(nocodes,codesinsite,thissite){
  plotkey = list()
  for (i in 1:nocodes){
    thiscode = codesinsite[i]
    thissiteplots = thissite%>%filter(ShortNVC == thiscode)%>%select(Plot)
    plotkey[[i]] = thissiteplots
  }
  names(plotkey) = codesinsite
  return(plotkey)
}

getplotoverlaps = function(plotset1,plotset2){
  
}

getoverlapinwood = function(site){
  thissite = plotvars%>%filter(Site == site)
  noplots = length(thissite$Plot)
  codesinsite =  as.character(unique(thissite$ShortNVC))
  nocodes = length(codesinsite)
  
  species = getspeciesforplot(site,noplots)
  plotkey = getplotkey(nocodes,codesinsite,thissite)
  
  bycodelist = list()
  for (i in 1:nocodes){
    set = plotkey[[i]]$Plot
    noinset = length(set)
    sublist = list()
    for (j in 1:noinset){
      thisplot = set[j]
      sublist[[j]] = species[[thisplot]]
    }
    bycodelist[[i]] = sublist
  } 
  #diffcodeoverlaps
  overlapdiff = vector()
  for (i in 1:nocodes){
    if (i == nocodes){
      break
    }
    set1 = bycodelist[[i]]
    set2 = bycodelist[[i+1]]
    overlapdiff = c(overlapdiff,getdiffoverlap(set1,set2))
    }
  #samecodeoverlap
  overlapsame = getdiffoverlap(bycodelist[[1]],bycodelist[[1]])
  overlaps = list()
  overlaps[[1]] = overlapsame
  overlaps[[2]] = mean(overlapdiff)
  return(overlaps)
} 


##lets do all woods

same = 0
diff = 0
for (i in 1:103){
  o = getoverlapinwood(i)
  same = same + o[[1]]
  diff = diff + o[[2]]
}
avesame = same/103
avediff = diff/103



#smallcodes = codesinsite[-which.max(lapply(plotkey,nrow))]