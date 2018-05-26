
rm(list = ls())
cat("\014")

emp_zetas = readRDS("empirical_zetas.RDS")
modelled_coefs = readRDS("zeta_coefficients.RDS")
modelled_zetas = readRDS("modelled_zetas.RDS")

ground_flora = read.csv("../Data/GroundCover.csv")
ground_flora = ground_flora%>%filter(Yr_2 == 2)
veg_codes = read.csv("../Data/vegetation_codes.csv")
colnames(ground_flora) = c("SITE", "PLOT","NEST","Cover","BRC_number","Year")
colnames(veg_codes) = c("Species", "BRC_number")
flora = ground_flora%>% inner_join(veg_codes)
Richness = read.csv("../../data/SiteRichness.csv")
CompleteSiteLevelvars = read.csv("../../Data/CompleteSiteLevelVars.csv")


library(ggplot2)

#####look at area under curve for first 3 zeta values###

A = vector()
for (i in 1:103){
  z = emp_zetas[,i]
  A1 = 0.5*(z[2] - z[1]) + z[2]
  A2 = 0.5*(z[2]- z[3]) + z[3]
  A[i] = A1+A2
  
}
#How does Area under zeta curve compare with richness or heterogeneity

data1 = as.data.frame(cbind(Richness,A))
data1 = data1[,-1]
colnames(data1) = c("Richness","A")
ggplot(data = data1, aes(x = A, y = Richness))+
  geom_point()+
  geom_smooth(method = lm)

data2 = as.data.frame(cbind(CompleteSiteLevelvars$Pos_Hetero_Index, A))
colnames(data2) = c("PHI","A")
data2 = data2[-c(80,5,78),]
ggplot(data = data2, aes(x = A, y = PHI))+
  geom_point()+
  geom_smooth(method = lm)


data3 = as.data.frame(cbind(CompleteSiteLevelvars$no_NVC, A))
colnames(data3) = c("no_NVC","A")
ggplot(data = data3, aes(x = A, y = no_NVC))+
  geom_point()+
  geom_smooth(method = lm)

#Nothing to see here
#Look at initial gradient of zeta curve

g = vector()
for( i in 1:103){
  z = emp_zetas[,i]
  g[i] = z[1]-z[2]
}

#How does initial gradient of zeta curve compare with heterogeneity


data4 = as.data.frame(cbind(Richness,g))
data4 = data4[,-1]
colnames(data4) = c("Richness","g")
ggplot(data = data4, aes(x = g, y = Richness))+
  geom_point()+
  geom_smooth(method = lm)

data5 = as.data.frame(cbind(CompleteSiteLevelvars$Pos_Hetero_Index, g))
colnames(data5) = c("PHI","g")
data5 = data5[-c(80,5,78),]
ggplot(data = data5, aes(x = g, y = PHI))+
  geom_point()+
  geom_smooth(method = lm)


data6 = as.data.frame(cbind(CompleteSiteLevelvars$no_NVC, g))
colnames(data6) = c("no_NVC","A")
ggplot(data = data6, aes(x = g, y = no_NVC))+
  geom_point()+
  geom_smooth(method = lm)

###
#make a coefficicient r. Because z1 = ave alpha div and z2 = ave beta div
#z1-z2 tell you about homog of site. If homg z1-z2 large (basically gradient
#of zeta decay because x2-x1=1) so you coul dlook at z1-z2 - but is
#not comparable between sites coz depends on z1. (So z1-z2)/z1 removes that

r = vector()
for( i in 1:103){
  z = emp_zetas[,i]
  r[i] = (z[1]-z[2])/z[1]
}

data7 = as.data.frame(cbind(Richness,r))
data7 = data7[,-1]
colnames(data7) = c("Richness","r")
ggplot(data = data7, aes(x = r, y = Richness))+
  geom_point()+
  geom_smooth(method = lm)

data8 = as.data.frame(cbind(CompleteSiteLevelvars$Pos_Hetero_Index, r))
colnames(data8) = c("PHI","r")
data8 = data8[-c(80,5,78),]
ggplot(data = data8, aes(x = r, y = PHI))+
  geom_point()+
  geom_smooth(method = lm)


data9 = as.data.frame(cbind(CompleteSiteLevelvars$no_NVC, r))
colnames(data9) = c("no_NVC","A")
ggplot(data = data9, aes(x = r, y = no_NVC))+
  geom_point()+
  geom_smooth(method = lm)

###########look at which curves are a good fit

#Take modelled - emp for power and exp

diff_exp = vector()
diff_pwr = vector()

for (i in 1:103){
  mod_z_exp = modelled_zetas[[1]][1,i]
  mod_z_pwr = modelled_zetas[[3]][1,i]
  z_emp = emp_zetas[1,i]
  diff_exp[i] = z_emp - mod_z_exp
  diff_pwr[i] = z_emp - mod_z_pwr
}

data = as.data.frame(cbind(diff_exp, diff_pwr))
ggplot(stack(data), aes(x = ind, y = values))+
  geom_boxplot(width = 0.3, fill = "grey")+
  ylab("Difference in empirical and modelled values")+
  xlab("Model type")+
  ggtitle("The empircal - modelled value for zeta_1" )


### what if there is a different fit?

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


fits_new = get_fits_exp(zetas_df)

zeta_coef_new = get_coef(fits_exp)


###
#Site 1

x = c(1:16)
y = emp_zetas[,1]
m1 = nls(y~c*(exp(x^(a)))*exp(b*x), start = list(a = -2, b = -0.1, c = 10))
p1 = predict(m1,x)
cor(p1,y)

data1 = as.data.frame(cbind(x,y))
data2 = as.data.frame(cbind(x,p))

ggplot(data = data1, aes(x = x, y = y) )+
  geom_point(size = 4)+
  geom_point(data = data2, aes(x = x,y = p1), colour = "red")

# Site 2
x = c(1:16)
y2 = emp_zetas[,2]
m2 = nls(y2~c*(exp(x^(a)))*exp(b*x), start = list(a = -0.2, b = -0.1, c = 3))
p2 = predict(m2,x)
cor(p2,y2)

data3 = as.data.frame(cbind(x,y2))
data4 = as.data.frame(cbind(x,p2))
ggplot(data = data3, aes(x = x, y = y2) )+
  geom_point(size = 4)+
  geom_point(data = data4, aes(x = x,y = p2), colour = "red")

# Site 5

x = c(1:16)
y5 = emp_zetas[,5]
m5 = nls(y5~c*(exp(x^(a)))*exp(b*x), start = list(a = -2, b = -0.1, c = 5))
p5 = predict(m5,x)
cor(p5,y5)

data3 = as.data.frame(cbind(x,y5))
data4 = as.data.frame(cbind(x,p5))
ggplot(data = data3, aes(x = x, y = y5) )+
  geom_point(size = 4)+
  geom_point(data = data4, aes(x = x,y = p5), colour = "red")

###Lets try them all!!

sites_to_remove = c(5,8,24,44,62,64,67,94)
subset_emp_zeta = emp_zetas[,-sites_to_remove]

m = list()
cor = vector()
pred = data.frame()
sites = c(1:4,6,7,9:23,25:43,45:61,63,65,66,68:93,95:103)
for (i in sites){
  #browser
  colname = paste("Site",i)
  y = subset_emp_zeta[,colname]
  nas = sum(is.na(y))
  l = 16 - nas
  y = y[c(1:l)]
  x = seq(from = 1, to = l, by = 1)
  m[[i]] = nls(y~c*(exp(x^(a)))*exp(b*x), start = list(a = -2, b = -0.1, c = 10))
  p = predict(m[[i]],x)
  pred = as.data.frame(rbind(pred,p))
  cor[i] = cor(p,y, method = "kendall")

  data1 = as.data.frame(cbind(x,y))
  data2 = as.data.frame(cbind(x,p))
  ggplot(data = data1, aes(x = x, y = y) )+
  geom_point(size = 4)+
  geom_point(data = data2, aes(x = x,y = p), colour = "red")
}

#look at pred zeta1 - emp zeta1
pred_zeta1 = pred[,1]
emp_zeta1 = subset_emp_zeta[1,]
diff_newmod = as.data.frame(t(pred_zeta1-emp_zeta1))

# now do boxplot again of diffs

data = as.data.frame(cbind(diff_exp, diff_pwr))
ggplot(stack(data), aes(x = ind, y = values))+
  geom_boxplot(width = 0.3, fill = "grey")+
  geom_boxplot(data = diff_newmod, aes(x = "", y = diff_newmod),width = 0.3, fill = "black")+
  ylab("Difference in empirical and modelled values")+
  xlab("Model type")+
  ggtitle("The empircal - modelled value for zeta_1" )

#####

#Look at R2??
exp_R2 = vector()
pwr_R2 = vector()
for (i in 1:103) {
  exp_R2[i] = modelled_coefs[[1]][5,i]
  pwr_R2[i] = modelled_coefs[[3]][5,i]
}
  
# get R2 for newmod

mean_y = apply(predt,2,mean)

ESS = function(vector,mu){
  ess = sum((vector - mu)^2)
  return(ess)
}

TSS = function(vector,mu){
  tss = sum((vector-mu)^2)
  return(tss)
}

newmod_r2 = vector()
for(i in 1:95){
  #browser()
  estsumsq = ESS(predt[,i],mean_y[i])
  totsumsq = TSS(predt[,i],mean_y[i])
  R2 = estsumsq/totsumsq
  newmod_r2[i] = R2
}

###boxplots of R2

data1 = as.data.frame(cbind(exp_R2,pwr_R2))

data3 = as.data.frame(newmod_r2)

ggplot(data = stack(data1), aes(x = ind, y = values))+
  geom_boxplot(width = 0.3, fill = "grey")+
  geom_boxplot(data = data3, aes(x = "",y = newmod_r2),width = 0.3, fill = "grey")

#######

#what about model coefficients

newmod_coefs = lapply(m, coef)
a = vector()
b = vector()
c = vector()

for (i in 1:103){
  if (!is.element(i,sites_to_remove)) {
  a[i] = newmod_coefs[[i]][[1]]
  b[i] = newmod_coefs[[i]][[2]]
  c[i] = newmod_coefs[[i]][[3]]
  }
  else{
    a[i] = NA
    b[i] = NA
    c[i] = NA
  }
}
  
## Now look at range in coefficients

data = as.data.frame(cbind(a,b,c)) 
ggplot(data = stack(data), aes(x = ind, y = values), na.rm = TRUE)+
  geom_boxplot(width = 0.3, fill = "grey")

c_noNA = na.omit(c) 
zeta1 = t(subset_emp_zeta[1,])
data = as.data.frame(cbind(zeta1,c_noNA))
colnames(data) = c("zeta1","c")

ggplot(data=data, aes(x = c, y = zeta1))+
  geom_point()
  

data = as.data.frame(cbind(na.omit(b), na.omit(a))) 
colnames(data) = c("b","a") 
ggplot(data=data, aes(x = a, y = b))+
  geom_point()



  