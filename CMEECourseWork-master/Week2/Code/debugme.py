
#!usr/bin/python

"""5.13.2 Debugging"""

__author__ = "Petra Guy, pg5117@ic.ac.uk"
__version__ = "2.7"

#imports

import sys

#constants

#functions

#These functions are here to create an error
def createabug(x):
	y = x**4
	z = 0.
	y = y/z
	return y

createabug(25)

def createanewbug(x):
	y = x**4
	z = 0.
	y = y/z
	return y

createabug(10)
if (__name__ == "__main__"):
	status = main(sys.argv)
	sys.exit(status)

