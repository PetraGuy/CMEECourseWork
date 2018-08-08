#!usr/bin/python

"""5.12.1 ALign DNA sequences"""

__author__ = "Petra Guy, pg5117@ic.ac.uk"
__version__ = "2.7"

#imports

import sys

#constants

data = [line.strip() for line in open("../Data/seq.csv", "r")]

seq2 = data[0]
seq1 = data[1]
l1 = len(seq1)
l2 = len(seq2)

#functions

def getfilenames ()
	echo 'enter files names to be aligned'
	read f1,f2
	return f1,f2

def makesequences (

if l1 >= l2:
    s1 = seq1
    s2 = seq2
else:
    s1 = seq2
    s2 = seq1
    l1, l2 = l2, l1 # swap the two lengths make s1, l1 the longest

# function that computes a score
# by returning the number of matches 
# starting from arbitrary startpoint - specified below

def calculate_score(s1, s2, l1, l2, startpoint):
	"""calculate_score matches strings from a startpoint"""
	matched = "" # contains string for alignement
	score = 0
	for i in range(l2):
		if (i + startpoint) < l1:
            # if its matching the character
			if s1[i + startpoint] == s2[i]:
				matched = matched + "*"
				score = score + 1
			else:
				matched = matched + "-"
					
    # build some formatted output
	print "." * startpoint + matched           
	print "." * startpoint + s2
	print s1
	print score 
	print ""

	return score
	
#######################################################
# running several times from different starting points
calculate_score(s1, s2, l1, l2, 0)
calculate_score(s1, s2, l1, l2, 1)
calculate_score(s1, s2, l1, l2, 5)


#######################################################
# now try to find the best match (highest score)
my_best_align = None
my_best_score = -1


# here's a loop over all the starting points given the length of l1
# this matches l2 starting at all points of l1
for i in range(l1):
    z = calculate_score(s1, s2, l1, l2, i)
    if z > my_best_score:
        my_best_align = "." * i + s2 #this many spaces from start
        my_best_score = z

print my_best_align
print s1
print "Best score:", my_best_score

f = open("../Sandbox/align_seq_out.txt","wb")
f.write(str(my_best_score))
f.write(str(my_best_align))
 
def main(f1 = ../Data/Fasta1.fasta, f2 = ):
		#call the default files in here
         
if (__name__ == "__main__"):
		status = main(sys.argv)
		sys.exit(status)
