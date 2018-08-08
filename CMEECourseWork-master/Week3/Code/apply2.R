#Exercise 9.1.1 apply family
#Biological Computing Boot Camp
#R Studio Version 1.1.383 ubuntu 16.04 LTS 64bi
#Author Petra Guy 20th October 2017


SomeOperation = function(v) {
  if (sum(v) > 0)
    return (v*100)
}
return(v)

M = matrix(rnorm(100),10,10)
print(apply(M, 1, SomeOperation))

