
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


#######################
#First need to put each site into a site by species presence absence data frame

create_dataframe = function(sitenum){
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
  pres_abs_dfs[[i]] = create_dataframe(i)
  
}



#############################
#now run zeta on each - but remember to remove plot column
get_Sexp = function(z){
  valsexp = z$zeta.exp
  intexp = exp(valsexp[[1]][[1]])
  slopeexp = valsexp[[1]][[2]]
  
  zeta.exp = vector()
  nCr = vector()
  n=25
  sign = vector()
  for (i in 1:n){
    zeta.exp[i] = intexp*exp(slopeexp*i)
    nCr[i] = choose(n,i)
    sign[i] = (-1)^(i+1)
  }
  Sexp = zeta.exp*nCr*sign
  Sn = sum(Sexp)
  return(Sn)
}

get_Spl = function(z){
  valspl = z$zeta.pl
  intpl = exp(valspl[[1]][[1]])
  slopepl = valspl[[1]][[2]]
    
    zeta.pl = vector()
    nCr = vector()
    n=25
    sign = vector()
    for (i in 1:n){
      zeta.pl[i] = intpl*exp((i^slopepl))
      nCr[i] = choose(n,i)
      sign[i] = (-1)^(i+1)
    }
    Spl = zeta.pl*nCr*sign
    Sn = sum(Spl)
    return(Sn)
  }


get_species = function(sitenum){
  sps = vector()
  df = pres_abs_dfs[[sitenum]]
  last.column = ncol(df)
  z_df = df[-last.column]
  rows = nrow(z_df)
  z_decline = Zeta.decline.ex(z_df, 1:rows)
  Sn.exp = get_Sexp(z_decline)
  Sn.pl = get_Spl(z_decline)
  sps[1] = Sn.exp
  sps[2] = Sn.pl
  return(sps)
}



######################

total_species = data.frame()
for (i in 1:103){
  species = get_species(i)
  total_species = as.data.frame(rbind(total_species,species))
  
}



