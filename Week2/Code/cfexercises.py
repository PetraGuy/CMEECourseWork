#!usr/bin/python

"""5.9.1 Control flow exercises"""

__author__ = "Petra Guy, pg5117@ic.ac.uk"
__version__ = "2.7"

#imports

import sys


"""print hello for various range() in for loops """
for i in range(3, 17):
	print "hello"

for j in range(12):
	if j % 3 == 0:
		print "hi"

for j in range(15):
	if j % 5 == 3:
		print "ho"
	elif j % 4 == 3:
		print "ha"
		
"""While loops"""
z = 12
while z < 100:
	if z == 31:
		for k in range(7):
			print "err"
	elif z == 18:
		print "eh"
	z = z + 1

	
def foo1(x):
	"""Sqrt input"""
	print x ** 0.5


def foo2(x,y):
	"""Returns the largest value"""
	if x > y:
		return x
	print y


def foo3(x,y,z):
	"""Sorts the three input values"""
	if x > y:
		tmp = y
		y = x
		x = tmp
	if y > z:
		tmp = z
		z = y
		y = tmp
	print [x,y,z]


def foo4(x):
	""" calculates 1*2*3...*x"""
	result = 1
	for i in range(1, x + 1):
		result = result * i
	print result




def foo5(x):
	"""Factorial using recursion"""
	print("factorial has been called with n = " + str(x))
	if x == 1:
		return 1
	else:
		res = x * foo5(x-1)
        print("intermediate result for ", x, ".....",res)
	return res	
	print res

def main(argv):
		foo1(1)
		foo2(2,9)
		foo3(3,4,5)
		foo4(4)
		foo5(4)
		
		


if (__name__ == "__main__"):
		status = main(sys.argv)
		sys.exit(status)
