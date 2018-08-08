#!usr/bin/python

"""Chapter 7 Extra Credit"""

__author__ = "Petra Guy, pg5117@ic.ac.uk"
__version__ = "2.7"

import os
import sys
import pandas as pd
import numpy as np

MyData = []
#the os library allows file names and paths to be stuck together easily
#pandas allows the pd.read straight into the dataframe
def getdata(f):
	"""This function sticks the filename to the path for the data directory"""
	path = "../Data/"
	filepath = os.path.join(path,f)
	df = pd.read_csv(filepath)
	return df

#because this is now a dataframe, you can operate on the columns without the for loop
#the numpy is allowing the tan function.
def calc_height(df):
	"""This functions calculates height of the tree from distance and angle of elevation"""
	df['height'] = df['Distance.m'] * np.tan(np.deg2rad(df['Angle.degrees']))
	return df

def savedata(data,f):
	"""This function creates correct output file name"""
	addition = "get_TreeHeight_"
	outname = addition+f
	path = "../Results/"
	outfilepath = os.path.join(path,outname)
	data.to_csv(outfilepath)


def main(argv):
	MyData = getdata(argv[1])
	MyData = calc_height(MyData)
	savedata(MyData,argv[1])


if (__name__ == "__main__"):
		status = main(sys.argv)
		sys.exit(status)
