#!usr/bin/python

"""5.10 Loop exercises"""

__author__ = "Petra Guy, pg5117@ic.ac.uk"
__version__ = "2.7"

#imports

import sys

#constants

#functions

for i in range(5):
	print i
	
my_list = [0,2, "geronimo!", 3.0, True, False]

for k in my_list:
	print k       # just print everything in the my_list

total = 0
summands = [0,1,11,111,1111]

for s in summands:
	print total + s # adding zero?!
	
z = 0
while z < 100:
	z = z + 1
	print(z)
	
b = True
while b:
	print "Geronimo!" # b is true therefore infinite loop
	



