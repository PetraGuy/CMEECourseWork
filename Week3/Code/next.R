#9.2.2 Using Next
#Biological Computing Boot Camp
#R Studio Version 1.1.383 ubuntu 16.04 LTS 64bi
#Author Petra Guy 25th October 2017
for (i in 1:20) {
  if ((i %% 2) == 0)
    next
  print(i)
  
}
