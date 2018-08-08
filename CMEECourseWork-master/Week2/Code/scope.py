#!usr/bin/python

"""5.8.3 Scope """

__author__ = "Petra Guy, pg5117@ic.ac.uk"
__version__ = "2.7"

#imports

import sys

#constants

_a_global = 10

#functions

def a_function():
		_a_global = 5 # this wont change _a_global
		_a_local = 4  # this is a local var
		print "Inside the function _a_global is ", _a_global
		print "Inside the function _a_local is ", _a_local
		return None

a_function()
print "Outside the function, the value of _a_global is", _a_global


def a_function():
		global _a_global # defining a global inside will make it global
		_a_global = 5 
		_a_local = 4
		print "Inside the function _a_global is ", _a_global
		print "Inside the function _a_local is ", _a_local
		return None
		
a_function()
print "Outside the function, the value of _a_global is", _a_global


