#!usr/bin/python

"""Chapter 5 Practical Extra Credit, align fasta sequences"""

__author__ = "Petra Guy, pg5117@ic.ac.uk"
__version__ = "2.7"

#imports
import numpy as np
import sys

#constants

#functions


#This function opens specified file, deletes first two rows because they
#are headers. Removes commas and spaces and creates list of 1 string. This
#function creates the data in the correct format, but it only works because I
#knew the format of the fasta files. This is not good because it might not 
#be transferrable to ANY fasta file. 
def getfastafile(h):
	"""Open fasta file, convert to string"""
	data = []
	data = [line.rstrip("\n") for line in open(h, "r")]
	dataout = data[2:]
	dataout = "'".join(dataout)
	dataout = dataout.replace("'","")
	return dataout

#Takes two strings and geives the longest to string1
def makelongestnumber1 (string1, string2):
	"""Set the longest string to be called longest"""
	if len(string1) > len(string2):
		np1 = string1
		np2 = string2
	else:
		np1 = string2
		np2 = string1
	return np1, np2

#Takes entered string and converts to numpy array, required in match function
def createnumpyarray (list1):
	"""Turn string into numpy array"""
	asnumpy = np.array(list(list1))
	return asnumpy

#Takes two strings, finds longest and shortest, finds difference in length
#Longest has first character removed to replicate moving shorter along the chain
#Longest also has incrementally smaller chunks removed from the end so that
#both strings are always the same length - so can be matched as numpys
def matching (x,y):
	"""Move shorter string along longer matching as you go"""
	longest, shortest = makelongestnumber1(x,y)
	bestscore = 0
	diff = len(longest) - len(shortest)
	for i in range(diff):
		toremove = (diff - i)
		test = longest[i:-toremove]
		testnumpy = createnumpyarray(test)
		shortestnumpy = createnumpyarray(shortest)
		score = sum(testnumpy == shortestnumpy)
		print "match ", score
		if score > bestscore:
			bestscore = score
	return bestscore





def main(argv):
	h1 = getfastafile("../../Week1/Data/407228326.fasta")
	h2 = getfastafile("../../Week1/Data/407228412.fasta")
	longest,shortest = makelongestnumber1(h1,h2)
	longestasnumpy = createnumpyarray(longest)
	z = matching(longest, shortest)
	print "largest match is ", z

if (__name__ == "__main__"):
	status = main(sys.argv)
	sys.exit(status)

	
