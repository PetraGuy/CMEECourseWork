#!usr/bin/python

"""5.8.2 What is sysargv"""

__author__ = "Petra Guy, pg5117@ic.ac.uk"
__version__ = "2.7"

#imports

import sys

print "this  is the name of the script: ", sys.argv[0]
print "Number of arguments: " ,len(sys.argv)
print "The arguments are :", str(sys.argv)


