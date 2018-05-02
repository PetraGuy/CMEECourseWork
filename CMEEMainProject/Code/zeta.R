# on zeta:
#the data must be in form of presence absence matrix.
#put this matrix into Zeta.decline.ex and the zeta values are available.
#use these to plot zeta vs zeta order and fit either exp regression of trunacated power law.
#Use this model to predict values of zeta for more plots
#Number of plots to fit to = total area wood/200m^2
#Use Sn = SUM(nCr)*zeta from zeta = 1 - required number.
#See Hui 2014 for this formula

library(ggplot2)
library(dplyr)
library(zetadiv)
rm(list = ls())
cat("\014")

ground_flora = read.csv("../Data/GroundCover.csv")
ground_flora = ground_flora%>%filter(Yr_2 == 2)
veg_codes = read.csv("../Data/vegetation_codes.csv")
colnames(ground_flora) = c("SITE", "PLOT","NEST","Cover","BRC_number","Year")
colnames(veg_codes) = c("Species", "BRC_number")
flora = ground_flora%>% inner_join(veg_codes)
Richness = read.csv("../data/SiteRichness.csv")
CompleteSiteLevelvars = read.csv("../Data/CompleteSiteLevelVars.csv")

#######################
#First need to put each site into a site by species presence absence data frame

#create the presence absence matrix
create_presence_absence = function(sitenum){
  Site = flora%>%filter(SITE==sitenum)
  BRC = unique(Site$BRC_number)
  BRC = as.character(BRC)
  columns = length(unique(Site$BRC_number))
  rows = length(unique(Site$PLOT))
  Sitedf = data.frame(matrix(ncol = columns, nrow = rows))
  colnames(Sitedf) = BRC
  plots = unique(Site$PLOT)
  Sitedf$plotnumber = plots

    for (i in seq_along(plots)){
      plot_num = plots[i]
      plot = Site%>%filter(PLOT==plot_num)
      matches = match(plot$BRC_number,BRC)
  
    for (j in 1:length(matches)){
      col = matches[j]
      Sitedf[i,col] = 1
    }
}
  Sitedf[is.na(Sitedf)]=0
  return(Sitedf)
}

pres_abs_dfs = list()
for (i in 1:103){
  pres_abs_dfs[[i]] = create_presence_absence(i)
  
}

saveRDS(pres_abs_dfs, "petras_presence_absence")

# presence absence df for each site now available in pres_abs_df[[sitenumber]]


#############################
#collect coef from zetadiv and compare to mine
#uses zeta decline function to get zeta coef
get_zetas_coef =  function(){
  #browser()
  coef = data.frame(nrow = 2)
  
  for (i in 1:103){
    df =   df = pres_abs_dfs[[i]]
    last.column = ncol(df)
    z_df = df[-last.column]
    rows = nrow(z_df)
    z_decline = Zeta.decline.ex(z_df, 1:rows)
    int =  z_decline$zeta.exp[[1]][[1]]
    slope = z_decline$zeta.exp[[1]][[2]]
    col = c(int,slope)
    coef = as.data.frame(cbind(coef,col))
  }
  return(coef)
}

model_coef = get_zetas_coef()

######################
# 
# # run this to get total species model using fits from zetadiv
# # but they were a bit odd, so redoing below
# total_species = data.frame()
# for (i in 1:103){
#   species = get_species(i)
#   total_species = as.data.frame(rbind(total_species,species))
#   
# }

########################
#uses zeta decline function to get zeta values for a df
get_zetas =  function(sitenum){
  
  df =   df = pres_abs_dfs[[sitenum]]
  last.column = ncol(df)
  z_df = df[-last.column]
  rows = nrow(z_df)
  z_decline = Zeta.decline.ex(z_df, 1:rows)
  zetas = round(z_decline$zeta.val,8)
  return(zetas)
}

#run through multiple dfs and get df of all zetas
get_all_zetas = function(){
  all_zetas = as.data.frame(matrix(nrow = 16))
  rownames(all_zetas) = c(1:16)
    for (i in 1:103){
    zs = get_zetas(i)
    short  = 16 - length(zs)
    pad = rep(NA,short)
    zs = c(zs,pad)
    colname = paste("Site",i)
    all_zetas[colname] = zs
  }
  return(all_zetas)
}

zetas_df = get_all_zetas()
zetas_df = zetas_df[,-1]
saveRDS(zetas_df,"empirical_zetas")

#had to create first column to make it work in for loop

#zetas_df has the empirical zeta values, now fit these to get modeled zetas

#########################
#For each site fit an exponential decay 
#z = Ae^-bi 
# fit1, ln(z) = lnA - bi

#creates a list of linear models
#this is log/order fit so z = Aexp(B*order)
#logz = logA + B*order
#A = expA, B = slope
get_fits_exp = function(data){
 
  model = list()
  for (i in 1:103){
    y = data[,i]
    y = y[!is.na(y)]
    y = y[which(y>0)]
    l = length(y)
    orders = seq(1,l,1)
    model[[i]] = lm(log(y)~orders)
    
  }
  return(model)
}

#creates a list of linear models
# this is log/log fit so z = A(order)^B
#logz = logA + Blog(order)
# A = exp(int), B = slope
get_fits_power = function(data){
  
  model = list()
  for (i in 1:103){
    y = data[,i]
    y = y[!is.na(y)]
    y = y[which(y>0)]
    l = length(y)
    orders = seq(1,l,1)
    model[[i]] = lm(log(y)~log(orders))
    
  }
  return(model)
}

# get the coefficients, se, r2 from model object
get_coef =  function(model){
  coefs_df = as.data.frame(matrix(nrow = 5))
  for (i in 1:103){
    coefs  = summary(model[[i]])$coefficients
    int = coefs[[1]]
    slope = coefs[[2]]
    se_int = coefs[[3]]
    se_slope = coefs[[4]]
    r2 = summary(model[[i]])$r.squared
    col = c(int,slope,se_int, se_slope,r2)
    colname = paste("Site",i)
    coefs_df[colname] = col
  }
  rownames(coefs_df) = c("int","slope","se_int","se_slope","R2")
  coefs_df = coefs_df[,-1]
  return(coefs_df)
}


fits_exp = get_fits_exp(zetas_df)
fits_power = get_fits_power(zetas_df)
zeta_coef_exp = get_coef(fits_exp)
zeta_coef_power = get_coef(fits_power)

#########
# How good are the exponential and power law fits

R2_exp = as.data.frame(t(zeta_coef_exp["R2",]))
R2_exp$coef = "exp"
R2_power = as.data.frame(t(zeta_coef_power["R2",]))
R2_power$coef = "power"
R2s = rbind(R2_exp, R2_power)
ggplot(R2s, aes(R2, fill = coef)) + geom_density(alpha = 0.2)


###########
# get the areas and work out how many orders of zeta are required at each site

areas = CompleteSiteLevelvars%>%select(Site,Area_ha)
areas$orders = round((areas$Area_ha*10000)/200, digits = 0)


#########
# calculate the modeled zetas for each site

model_zetas = data.frame(nrow = 16)
get_model_zetas = function(){
    for ( i in 1:103){
     B = zeta_coef_exp[2,i]
     A = exp(zeta_coef_exp[1,i])
     z = vector()
     for( j in 1:16){
       z[j] = round(A*exp(j*B),8)
     }
     model_zetas = cbind(model_zetas,z)
    }
   model_zetas = model_zetas[,-1]
   return(model_zetas)
}
model_zetas = get_model_zetas()
saveRDS(model_zetas, "modelled_zetas")


##############################################################
#If you use empirical zetas do you get back to richness when you do S16?

get_rich_empirical = function(){
  #browser()
  nCr = vector()
  Rich_emp = vector()
  for (i in 1:103){ 
    zs = zetas_df[,i]
    l = length(zs)
    z = vector()
    sign = vector()
    for (j in 1:l){
      nCr[j] = choose(l,j)
      sign[j] = (-1)^(j+1)
      z[j] = zs[j]
    }
    S = nCr*sign*z
    Rich_emp[i] = sum(S)
  }
  return(Rich_emp)
}
Rich_emp = get_rich_empirical()


#####################
get_rich_mod = function(){
  #browser()
  nCr = vector()
  Rich_mod = vector()
  for (i in 1:103){ 
    zs = model_zetas[,i]
    l = length(zs)
    z = vector()
    sign = vector()
    for (j in 1:l){
      nCr[j] = choose(l,j)
      sign[j] = (-1)^(j+1)
      z[j] = zs[j]
    }
    S = nCr*sign*z
    Rich_mod[i] = sum(S)
  }
  return(Rich_mod)
}
Rich_mod = get_rich_mod()


##########
#look at some zetas

x = c(1:16)
y1 = zetas_df[,1]
y2 = zeta_exp


data = as.data.frame(cbind(x,y1,y2))
ggplot(data, aes(x = x))+
  geom_point(aes(y = y1))+
  geom_point(aes(y = y2), colour = "red")

############
# get Sn for all sites - probably only need to go to 30 at most

richnesses = get_zeta_modeled_richness()
Richness$zeta_model = richnesses


# get_Spl = function(z){
#   # this was to get the fit from zeta div
#   # and cacluate Sn - but i will do my own fit
#   valspl = z$zeta.pl
#   intpl = exp(valspl[[1]][[1]])
#   slopepl = valspl[[1]][[2]]
#     
#     zeta.pl = vector()
#     nCr = vector()
#     n=25
#     sign = vector()
#     for (i in 1:n){
#       zeta.pl[i] = intpl*exp((i^slopepl))
#       nCr[i] = choose(n,i)
#       sign[i] = (-1)^(i+1)
#     }
#     Spl = zeta.pl*nCr*sign
#     Sn = sum(Spl)
#     return(Sn)
#   }
# 
# 
# get_species = function(sitenum){
#   # this runs zeta decline on each site and obtains Sn from 
#   # exp fit to zeta. Will redo and get the actual zeta values, see below
#   sps = vector()
#   df = pres_abs_dfs[[sitenum]]
#   last.column = ncol(df)
#   z_df = df[-last.column]
#   rows = nrow(z_df)
#   z_decline = Zeta.decline.ex(z_df, 1:rows)
#   Sn.exp = get_Sexp(z_decline)
#   Sn.pl = get_Spl(z_decline)
#   sps[1] = Sn.exp
#   sps[2] = Sn.pl
#   return(sps)
# }
# 


