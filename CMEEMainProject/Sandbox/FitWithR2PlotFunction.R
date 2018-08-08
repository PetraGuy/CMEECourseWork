
# from  https://sejohnston.com/2012/08/09/

ggplotRegression <- function (fit) {
  
  require(ggplot2)
  
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                       "Intercept =",signif(fit$coef[[1]],5 ),
                       " Slope =",signif(fit$coef[[2]], 5),
                       " P =",signif(summary(fit)$coef[2,4], 5)))
}


fit1 <- lm(logavecf ~ logarea, data = ave_data)
fit2 = lm(logcf ~ logarea, data = site1_max)
fit3 = lm(logcf ~ logarea, data = site1_min)
ggplotRegression(fit3)

