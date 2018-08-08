#!usr/bin/python

"""Chapter 6 Rgular Expressions"""

__author__ = "Petra Guy, pg5117@ic.ac.uk"
__version__ = "2.7"

import re

my_string = "a given string"
match = re.search(r'\s', my_string) #\s = whitespace
print match # there is not match coz no return therefore get funny answer

#match.group()

match = (r's\w*', my_string)#space then any char
print'2',match

match = re.search(r'\d', my_string)
print '3',match


my_string = 'an example'
match = re.search(r'\w*\s', my_string)

if match:
	print 'found a match:', match.group()
else:
	print 'no match'
	
# not sure what the match.group is doing, makes no difference if run wih or without
#? .group returns subgroups of the objecct by indices or names.

MyStr = "an example"

match = re.search(r'\w*\s', MyStr) #= any char then space

if match:
	print 'found a match :', match.group()
else:
	print 'Nope'
	
match = re.search(r'\d', "it takes 2 to tango") #\d any numeric
print match.group()

match = re.search(r'\s\w*\s', "once upon a time") #space, any char, space
print match.group()

match = re.search(r'\s\w{1,3}\s', 'once upon a bit of time')# space 1 or 3 chars?? space
print match.group()

match = re.search(r'\s\w*$', 'once upon a bit of time') 
print match.group()

match = re.search(r'\w*\s\d.*\d', 'take 2 grams of H2O')
print match.group()

match = re.search(r'^\w*.*\s','once upon a time')
print match.group()




