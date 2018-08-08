#!usr/bin/python

"""5.10.1. Oaks, Writing list comprehensions"""

__author__ = "Petra Guy, pg5117@ic.ac.uk"
__version__ = "2.7"

#imports

import sys

#constants

taxa = ["Quercus robur",
		"Fraxinus excelsior",
		"Pinus sylvestris",
		"Quercus cerris",
		"Quercus petrea", ]
		
#functions

def is_an_oak(name):
	return name.lower().startswith("quercus")
#returns all the oaks
	
oaks_loops = set()

for species in taxa:
	if is_an_oak(species):
		oaks_loops.add(species)
print oaks_loops
#loops through elements in taxa, checks them using is_an_oak, adds
#to set called oaks_loop if it is an oak.

oaks_lc = set([species for species in taxa if is_an_oak(species)])
print oaks_lc
# this line does the same as the above loop. first species is the 
#created variable, next bit is replica of the loop statement

oaks_loops = set()
for species in taxa:
	if is_an_oak(species):
		oaks_loops.add(species.upper())
print oaks_loops

oaks_lc = set([species.upper() for species in taxa if is_an_oak(species)])
print oaks_lc

