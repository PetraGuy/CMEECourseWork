


testloop = function(timer = 20, counter = 10, interval = 10) {
  x = 1
  count = 0
  int = 1
  xvect = vector()
  time = 0
  while (time < timer) {
    time = time + 1
    if (count < counter) {
      x = x*2}
      count = count + 1
      if (int%%interval == 0) {
        xvect = c(xvect, sqrt(x))
        int = int + 1   }      
      print ("end of count")
      
  }
  
 print("out of time")
 return(xvect)
}

mylist = list(c(1,2,3), c("A","B","c"))

mylist[1] = "numbers"
mylist[2] = "letters"