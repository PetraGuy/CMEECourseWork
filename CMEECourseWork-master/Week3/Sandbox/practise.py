#!usr/bin/python

"""Vectoize1 in python extra credit example"""

__author__ = "Petra Guy, pg5117@ic.ac.uk"
__version__ = "2.7"

import sys
import numpy as np
import pandas as pd

def createMatrix():
	Row1 = np.random.random((1,10))
	M = np.empty((9,10))
	M = np.concatenate((Row1,M))
	return M
	
def stochrick(X):
	r = 1.2
	K = 1
	sigma = 0.2
	for i in range
	theta = r*(1-X/K)
	X = X * np.exp(theta)
	return X


	

def main(argv):
	from timeit import default_timer as timer
	start = timer()
	df = pd.DataFrame(createMatrix())
	dataout = df.apply(stochrick, axis = 1)
	print dataout
	end = timer()
	timetaken = end - start
	print "execution time is ", timetaken
	
	
if (__name__ == "__main__"):
	status = main(sys.argv)
	sys.exit(status)
