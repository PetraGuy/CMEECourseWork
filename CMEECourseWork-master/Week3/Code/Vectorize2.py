"""Vectoize1 in python extra credit example"""

__author__ = "Petra Guy, pg5117@ic.ac.uk"
__version__ = "2.7"

import sys
import numpy as np
import pandas as pd

#This script works with columns instead of rows. Other scripts in Sandbox
#Worked down rows, but I wanted to try this to see if faster
#Result can always be transposed.
#Improvements - variables for the Ricker equation are had coded.
#The stochastic element is random uniform, should be normal.
def get_column(row_count):
	"""Function t generate random numbers for the first row"""
	row = np.random.random((1, row_count))
	data = np.transpose(row)
	df = pd.DataFrame(data=data)
	return df

def get_norm(row_count):
	"""Function to generation the stochastic element"""
	data = np.random.random((1, row_count))
	return data

def stochrick(df, col_count, row_count):
	"""Caluclates the Ricker iteration across columns"""
	r = 1.2
	K = 1
	sigma = 0.2
	i = 1

	while i <= col_count:
		last_col = df.columns[-1]
		factor = get_norm(row_count)
		theta = r * (1 - df[last_col] / K)
		#import pdb; pdb.set_trace()		
		new_col = df[last_col] * np.exp(theta) + factor[0]
		new_col.name = i
		df = pd.concat([df, new_col], axis=1)
		i += 1
	return df


def main(argv):
	from timeit import default_timer as timer
	start = timer()
	df = get_column(row_count=100)
	Mout = stochrick(df, col_count=1000, row_count=100)
	print Mout
	end = timer()
	timetaken = end - start
	print "execution time is ", timetaken


if (__name__ == "__main__"):
	status = main(sys.argv)
	sys.exit(status)
