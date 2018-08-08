#!usr/bin/python

"""Vectorize1 in python extra credit example"""

__author__ = "Petra Guy, pg5117@ic.ac.uk"
__version__ = "2.7"

import sys
import numpy as np
import pandas as pd

#This would need to take number array dimensions as variables in the final version
def createMatrix():
	"""This function creates a row on random numbers then appends a zero matrix to it"""
	Row1 = np.random.random((1,1000))
	M = np.zeros((99,1000))
	M = np.concatenate((Row1,M))
	return M

#as above, needs modifiction so that years, r, K etc are variables
#the X.iloc gives row location so iteration is over rows	
def stochrick(X):
	r = 1.2
	K = 1
	sigma = 0.2
	years = 100
	for i in range(years-1):
		theta = r*(1-X.iloc[i]/K)
		X.iloc[i+1]= X.iloc[i+1]+ X.iloc[i]*np.exp(theta)
	return X
	
	
	
# apply function applies stochrick to df.
def main(argv):
	from timeit import default_timer as timer
	start = timer()
	df = pd.DataFrame(createMatrix())
	Mout =  df.apply(stochrick)
	print Mout
	end = timer()
	timetaken = end - start
	print "execution time is ", timetaken
	
	
if (__name__ == "__main__"):
	status = main(sys.argv)
	sys.exit(status)
