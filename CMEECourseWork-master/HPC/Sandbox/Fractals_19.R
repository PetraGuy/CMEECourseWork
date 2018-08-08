

chaos_game = function(){
  graphics.off()
  #browser()
  x = vector()
  y = vector()
  X <- list(c(0,0),c(3,4),c(4,1))
  coord = X[[1]]
  x1 = coord[[1]]
  y1 = coord[[2]]
  plot(NA, xlim=c(0,5), ylim=c(0,5), xlab="X", ylab="Y")
  points(x1,y1, cex = 0.2)
  for (i in 1:1000){
   index = sample((1:3),1)
   coord = X[[index]]
   x2 = coord[[1]]
   y2 = coord[[2]]
   x1 = (0.5*x2 + 0.5*x1)
   y1 = (0.5*y2 + 0.5*y1)
   x = c(x, x1)
   y = c(y, y1)
  
  }
  plot(x , y, cex = 0.2)
}
