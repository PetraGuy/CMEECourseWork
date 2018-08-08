library(reshape)
library(ggplot2)
emp_zetas = readRDS("../Zeta/empirical_zetas.RDS")


x = c(1:16)
zeta_78 = emp_zetas[,78]
zeta_83 = emp_zetas[,83]
zeta_83[16] = 0
zeta_8 = emp_zetas[,8]
zeta_25 = emp_zetas[,25]
zeta_71 = emp_zetas[,71]

data1 = as.data.frame(cbind(x,zeta_78))
data2 = as.data.frame(cbind(x,zeta_83))
data3 = as.data.frame(cbind(x,zeta_8))
data4 = as.data.frame(cbind(x,zeta_25))
data5 = as.data.frame(cbind(x,zeta_71))


colnames(data1)=c("order","zeta")
colnames(data2)=c("order","zeta")
colnames(data3)=c("order","zeta")
colnames(data4)=c("order","zeta")
colnames(data5)=c("order","zeta")




ggplot(data=data1, aes(x = order, y = zeta)) +
  geom_point(aes(colour="Site 78"),size = 3)+
  geom_point(data=data2,aes(colour="Site 83"),size = 3)+
  geom_point(data=data3,aes(colour="Site 8"),size = 3)+
  geom_point(data=data4,aes(colour="Site 25"),size = 3)+
  geom_point(data=data5,aes(colour="Site 71"),size = 3)+
  ggtitle("Zeta Decay Curve")+
  ylab("zeta value")+
  xlab("zeta order")


