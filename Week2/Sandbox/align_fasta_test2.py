
#f1 = ("../../Week1/Data/407228326.fasta", "r")


data = []
def justgetfile2(h):
	data = [line.rstrip("\n") for line in open(h, "r")]
	dataout = data[2:]
	dataout = "'".join(dataout)
	dataout = dataout.replace("'","")
	return dataout

def makelongestnumber1 (string1, string2):
	if len(string1) > len(string2):
		np1 = string1
		np2 = string2
	else:
		np1 = string2
		np2 = string1
	return np1, np2

def createnumpyarray (list1):
	import numpy as np
	asnumpy = np.array(list(list1))
	return asnumpy

def matching (x,y):
	import numpy as np
	longest, shortest = makelongestnumber1(x,y)
	bestscore = 0
	diff = len(longest) - len(shortest)
	for i in range(diff):
		toremove = (diff - i)
		test = longest[i:-toremove]
		print len(test)
		print len(shortest)
		testnumpy = createnumpyarray(test)
		shortestnumpy = createnumpyarray(shortest)
		score = sum(testnumpy == shortestnumpy)
		print score
		if score > bestscore:
			bestscore = score
	return bestscore

h1 = justgetfile2("../../Week1/Data/407228326.fasta")
h2 = justgetfile2("../../Week1/Data/407228412.fasta")

longest,shortest = makelongestnumber1(h1,h2)
print len(longest)
print len(shortest)

longestasnumpy = createnumpyarray(longest)

z = matching(longest, shortest)
print z


	
