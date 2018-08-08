
#3d plot

library(rgl)
library(rglwidget)
library(knitr)

plot3d(slopeint$Richness, slopeint$slope, slopeint$intercept, size = 3, lit=TRUE)
