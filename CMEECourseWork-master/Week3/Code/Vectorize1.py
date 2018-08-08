#!usr/bin/python


"""Vectoize1 in python extra credit example"""

__author__ = "Petra Guy, pg5117@ic.ac.uk"
__version__ = "2.7"

import sys
import numpy as np

# An example of suming matrix elements using for loop for rows and cols

def Sum_Matrix_Elements(M):
	Tot = 0
	for i in range(M.shape[0]):
		for j in range(M.shape[1]):
			Tot = Tot + M[i][j]
	return Tot


def main(argv):
	from timeit import default_timer as timer
	start = timer()
	M = np.random.random((3,3))
	Total = Sum_Matrix_Elements(M)
	print "Matrix element sum is ", Total
	end = timer()
	timetaken = end - start
	print "execution time is ", timetaken
	
	
if (__name__ == "__main__"):
	status = main(sys.argv)
	sys.exit(status)





