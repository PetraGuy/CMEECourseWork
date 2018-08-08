plot(NA, xlim=c(0,30), ylim=c(0,30), xlab="X", ylab="Y")
fern_2 = function(start,distance,direction,dir){
  
  coords = turtle(start, distance, direction)
  if (distance > 0.1){
    fern_2(coords,0.38*distance,direction - (pi/4)*dir,dir*-1)
    fern_2(coords,0.87*distance,direction,dir*-1)
  }
}
fern_2(c(15,0),5,pi/2,-1)