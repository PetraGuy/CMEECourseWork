
#Select brack, knotweed, rhodis, nettles and himalyan balsam
# what you would do from here is extract the sites/plots where cover is > say 50%
# then correlate that with plot richnesses

#So this analysis not complete
#just the sites/bolts extracted where the invasives occur


library(dplyr)

Data = read.csv("../Data/GroundCover.csv")
Data_Yr2 = Data%>%filter(Yr_2 == 2)#%>%select(SITE,PLOT,NEST,COV,Amalgams)
colnames(Data_Yr2) = c("SITE", "PLOT","NEST","Cover","BRC_number","Year")
veg_codes = read.csv("../Data/vegetation_codes.csv")
colnames(veg_codes) = c("Species", "BRC_number")
Data_Yr2_veg = Data_Yr2%>% inner_join(veg_codes)




#############

bracken = Data_Yr2_veg%>%filter(BRC_number == 9101619)
totcov_bracken = bracken%>%group_by(SITE)%>%summarise(totcov_brack = sum(Cover))


rhodi = Data_Yr2_veg%>%filter(BRC_number == 9201687)
totcov_rhodi = rhodi%>%group_by(SITE)%>%summarise(totcov_rhod = sum(Cover))

knot =  Data_Yr2_veg%>%filter(BRC_number == 9201528)
totcov_knot = knot%>%group_by(SITE)%>%summarise(totcov_knotweed = sum(Cover))

balsam = Data_Yr2_veg%>%filter(BRC_number == 9201026)
totcov_balsam = balsam%>%group_by(SITE)%>%summarise(totcov_hima = sum(Cover))

nettles = Data_Yr2_veg%>%filter(Species == "Urtica dioica")
totcov_nettles = nettles%>%group_by(SITE)%>%summarise(totcov_urtica = sum(Cover))

Richness = Sitedata%>%select(Site,Richness)
colnames(Richness) = c("SITE","Richness")


# not sure that this is right - coz you will end up with df 
#where only sites with all species co-occur??

invasives = inner_join(Richness,totcov_bracken)
invasives = inner_join(invasives,totcov_balsam)
#invasives = inner_join(invasives,totcov_knot)
invasives = inner_join(Richness,totcov_nettles)
invasives = inner_join(invasives,totcov_rhodi)



                       