#9.2.1 Breaking out of loops

#Biological Computing Boot Camp
#R Studio Version 1.1.383 ubuntu 16.04 LTS 64bi
#Author Petra Guy 21th October 2017

i = 0
  while (i < Inf) {
    if (i==20) {
      break }
    else {
      cat ("i equals " , i , "\n")
      i  = i + 1
    }
  }