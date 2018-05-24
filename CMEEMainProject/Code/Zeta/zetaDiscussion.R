


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

x = c(1:16)
y = emp_zetas[,1]
m1 = nls(y~c*(exp(x^(a)))*exp(b*x), start = list(a = -2, b = -0.17, c = 10))
p1 = predict(m1,x)
cor(p1,y)

data1 = as.data.frame(cbind(x,y))
data2 = as.data.frame(cbind(x,p))

ggplot(data = data1, aes(x = x, y = y) )+
  geom_point(size = 4)+
  geom_point(data = data2, aes(x = x,y = p1), colour = "red")


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
              