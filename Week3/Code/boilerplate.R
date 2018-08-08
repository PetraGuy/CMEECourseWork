##7.13 Writig R Functions

#Biological Computing Boot Camp
#R Studio Version 1.1.383 ubuntu 16.04 LTS 64bi
#Author Petra Guy 20th October 2017

MyFunction <- function(Arg1, Arg2) {
  print(paste("Argument", as.character(Arg1), "is a", class(Arg1)))
  print(paste("Argument", as.character(Arg2), "is a", class(Arg2)))
return(c(Arg1,Arg2))
  
}

MyFunction(1,2)
MyFunction("Bob","Geoff")