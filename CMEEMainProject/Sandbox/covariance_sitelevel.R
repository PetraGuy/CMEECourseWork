


#Various ways of looking at colinearity and most important variables, redone most of this 
#in AnalysingCovariance files


siterich =  read.csv("../Data/SiteRichness.csv")
sitevars = read.csv("../Data/SiteLevelVars.csv")
colnames(sitevars)

fit = lm(Richness ~ Northing + Easting + Alt_m + Area_ha + Perim_m + 
           Pos_Hetero_Index + Buffer3 + no_NVC + meandbh + no_trees, data = sitevars)
visreg(fit, type = "contrast")
visreg(fit, type = "conditional")

visreg(fitphysical)
fitphysical = lm(Richness ~ Alt_m + Area_ha + Northing + Easting +
                  Perim_m + Buffer3 + meandbh + meanph+ meanSOM + meanLBA, data =  sitevars)
fithetero = lm(Richness ~ Pos_Hetero_Index + no_NVC + no_trees + no_MSG + sd_SOM +
                 sd_pH + no_MSG + sd_meandbh + sd_treedensity +sd_LBA, data = sitevars)

physicaldf = as.data.frame(cbind(sitevars$Alt_m, sitevars$Area_ha, sitevars$Perim_m, 
                                 sitevars$Northing, sitevars$Easting,sitevars$Buffer3, 
                                 sitevars$meandbh, sitevars$meanph, sitevars$meanSOM, 
                                 sitevars$meanLBA))

colnames(physicaldf) = c("Alt_m","Area_ha", "Perim_m", 
                         "Northing", "Easting","Buffer3", 
                         "meandbh", "meanph", "meanSOM", 
                         "meanLBA")
heterodf = as.data.frame(cbind(sitevars$Pos_Hetero_Index, sitevars$no_NVC, 
                               sitevars$sd_pH, sitevars$sd_SOM, sitevars$no_MSG,
                               sitevars$sd_LBA, sitevars$sd_meandbh, sitevars$no_trees))

colnames(heterodf) = c("Pos_Hetero_Index", "no_NVC", "sd_pH", "sd_SOM", "no_MSG",
                               "sd_LBA", "sd_meandbh","no_trees")


panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = 1 )
}

physicaldf_Rich = as.data.frame(cbind(sitevars$Richness,physicaldf))
physicalpc = princomp( ~ ., data = physicaldf, cor = TRUE, scores = TRUE)
heteropc = princomp(~., data = heterodf)
biplot(heteropc, scale = 1)

vif(fitphysical)
vif(fithetero)

#####################

fitphysical2 = fitphysical = lm(Richness ~ Alt_m + Area_ha + Northing +
                                  Perim_m + Buffer3 + meandbh + meanph+ meanSOM + meanLBA, data =  sitevars)

fitphysical3 = fitphysical = lm(Richness ~ Alt_m + Area_ha + Northing +
                                  Perim_m + Buffer3 + meandbh +  meanSOM + meanLBA, data =  sitevars)

fitphysical4 = fitphysical = lm(Richness ~  Area_ha + Northing +
                                  Perim_m + Buffer3 + meandbh +  meanSOM + meanLBA, data =  sitevars)
fitphysical5 = fitphysical = lm(Richness ~  Area_ha + Northing +
                                  Perim_m + Buffer3 + meandbh +  meanSOM , data =  sitevars)
fitphysical6 = fitphysical = lm(Richness ~  Area_ha + Northing +
                                  Buffer3 + meandbh +  meanSOM, data =  sitevars)
fitphysical6 = fitphysical = lm(Richness ~ Northing +
                                  Buffer3 + meandbh +  meanSOM, data =  sitevars)


