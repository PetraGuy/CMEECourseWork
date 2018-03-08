
# exploring relationship between range of ellenbergs and richness of a site


rm(list = ls())
cat("\014")
setwd("~/Documents/CMEECourseWork/CMEEMainProject/Code")

Data = read.csv("../Data/GroundCover.csv")
Data_Yr2 = Data%>%filter(Yr_2 == 2)#%>%select(SITE,PLOT,NEST,COV,Amalgams)
colnames(Data_Yr2) = c("SITE", "PLOT","NEST","Cover","BRC_number","Year")
veg_codes = read.csv("../Data/vegetation_codes.csv")

# the  bryophytes, lichen etc have already been removed from these, 
#because the veg codes csv files is no bryophytes, so need to join this 
#with ground cover to eliminate bryophytes from counts


colnames(veg_codes) = c("Species", "BRC_number")
Data_Yr2_veg = Data_Yr2%>% inner_join(veg_codes)

#get the ellenbergs file

ellenbergs = read.csv("../Data/Spp_lib.csv")
ellenbergs  = ellenbergs[c(2,3,5,6,7,8)]
colnames(ellenbergs) = c("BRC_Name","BRC_number","R","N","W","L")
veg_ellens = left_join(Data_Yr2_veg,ellenbergs)
