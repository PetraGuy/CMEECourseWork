
plot(NA, xlim=c(0,5), ylim=c(0,5), xlab="X", ylab="Y")

turtle = function(start, distance, direction){
  x1 = start[1]
  y1 = start[2]
  x2 = x1 + distance*cos(direction)
  y2 = y1 + distance*sin(direction)
  segments(x1,y1,x2,y2)
  coords = c(x2,y2)
  return(coords)
}


elbow = function(start, distance, direction){
  coords = turtle(start, distance, direction)
  direction = (direction  - pi/4)
  distance = 0.95*distance
  coords = turtle(coords, distance, direction)
  
}

  
spiral = function(start, distance, direction){
  coords = turtle(start, distance, direction)
  direction = (direction - pi/4)
  distance = 0.95*distance
  spiral(coords,distance,direction)
  
}


plot(NA, xlim=c(0,5), ylim=c(0,5), xlab="X", ylab="Y")
spiral_2 = function(start, distance, direction){
    coords = turtle(start, distance, direction)
    direction = (direction - pi/4)
    distance = 0.95*distance
    if (distance > 0.1){
    spiral_2(coords,distance,direction)
    }
  }
start = c(2,2)
spiral_2(start,1,pi/4)


plot(NA, xlim=c(0,10), ylim=c(0,10), xlab="X", ylab="Y")
tree = function(start,distance,direction){
coords = turtle(start, distance, direction)
direction1 = (direction - pi/4)
direction2 = (direction + pi/4)
distance = 0.65*distance
if (distance > 0.1){
  tree(coords,distance,direction1)
  tree(coords,distance,direction2)
 }
}
start = c(5,0)
tree(start,3,pi/2)
####################################################
plot(NA, xlim=c(0,20), ylim=c(0,12), xlab="X", ylab="Y")

turtle1 = function(start, distance, direction){
  x1 = start[1]
  y1 = start[2]
  x2 = x1 + distance*cos(direction)
  y2 = y1 + distance*sin(direction)
  if (distance > 0.5){
    col = "brown"
    lwd = 3
  }else {
    col = "green"
    lwd = 0.5
  }
  segments(x1,y1,x2,y2,col = col,lwd =lwd)
  coords = c(x2,y2)
  return(coords)
}

tree_mod = function(start,distance,direction){
  coords = turtle1(start, distance, direction)
  direction1 = (direction - pi/5)
  direction2 = (direction + pi/5)
  distance = 0.75*distance
  if (distance > 0.05){
    tree_mod(coords,distance,direction1)
    tree_mod(coords,distance,direction2)
  }
}
start = c(10,0)
tree_mod(start,3,pi/2)

##############################################
plot(NA, xlim=c(1,9), ylim=c(0,7), xlab="X", ylab="Y")
fern = function(start,distance,direction){
  coords = turtle(start, distance, direction)
  direction1 = direction + pi/4
  direction2 = pi/2
  if (distance > 0.1){
    fern(coords,0.87*distance, direction1)
    fern(coords,0.38*distance, direction2)
  }
}
start = c(8,0)
fern(start,2,pi/2)

#modified fern such that pi/2 changes slightly to produce curly fern
plot(NA, xlim=c(1,9), ylim=c(0,7), xlab="X", ylab="Y")
fern_mod = function(start,distance,direction){
  coords = turtle(start, distance, direction)
  direction1 = (direction + pi/8)
  direction2 = 0.9*direction
  if (distance > 0.1){
    fern_mod(coords,0.87*distance,direction1)
    fern_mod(coords,0.5*distance,direction2)
  }
}
start = c(8,0)
fern_mod(start,2,pi/2)


plot(NA, xlim=c(0,10), ylim=c(0,10), xlab="X", ylab="Y")
fern_2 = function(start,distance,direction,dir){
  
  coords = turtle(start, distance, direction)
  direction1 = direction - (pi/4)*dir
  dir = dir*-1
  if (distance > 0.1){
    fern_2(coords,0.38*distance,direction1,dir)
    fern_2(coords,0.87*distance,direction,dir)
  }
}
start = c(5,0)
fern_2(start,2,pi/2,-1)

factor = 0.38
plot(NA, xlim=c(0,40), ylim=c(0,40), xlab="X", ylab="Y")
fern_2_mod = function(start,distance,direction,dir){
  #browser()
  coords = turtle(start, distance, direction)
  factor = factor*1.2
  direction1 = direction - (pi/4)*dir
  dir = dir*-1
  if (distance > 0.05){
    fern_2_mod(coords,factor*distance,direction1,dir)
    fern_2_mod(coords,0.87*distance,direction,dir)
  }
}
start = c(20,0)
fern_2_mod(start,5,pi/2,-1)
