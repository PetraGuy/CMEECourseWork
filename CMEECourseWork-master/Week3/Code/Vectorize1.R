##Exercise 9.1
#Biological Computing Boot Camp
#R Studio Version 1.1.383 ubuntu 16.04 LTS 64bi
#Author Petra Guy 23th October 2017

M = matrix(runif(1000000), 1000,1000)

#AN example of using for loops to sum matrix elements
SumAllElements =  function(M) {
  Dimensions = dim(M)
  Tot = 0
  for (i in 1:Dimensions[1]){
    for (j in 1:Dimensions[2]){
      Tot = Tot + M[i,j]
    }
  }
  return(Tot)
}

print(system.time((SumAllElements(M))))
print(system.time(sum(M)))
