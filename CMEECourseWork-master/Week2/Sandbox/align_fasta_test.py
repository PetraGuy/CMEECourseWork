import numpy as np

# testing functions for align_seq_fasta

f1 = ("../../Week1/Data/407228326.fasta", "r")
f2 = ("../../Week1/Data/407228412.fasta", "r")

def getfilestostrings (f1,f2):
	gene1 = "'".join(line.rstrip("\n") for line in open(f1))
	gene1 = gene1.replace("'", "")
	gene2 = "'".join(line.rstrip("\n") for line in open(f2))
	gene2 = gene2.replace("'","")
	return gene1, gene2


def removetrash (gene1, gene2):
	geneparts1 = gene1.partition("region")
	reqgene1 = geneparts1[2]
	geneparts2 = gene2.partition("16")
	reqgene2 = geneparts2[2]
	return reqgene1, reqgene2

def converttonumpy (reqgene1, reqgene2):
	gene1np = np.array(list(reqgene1))
	gene2np = np.array(list(reqgene2))
	return gene1np, gene2n


def makelongestnumber1 (gene1np, gene2np):
	if len(gene1np) > len(gene2np):
		np1 = gene1np
		np2 = gene2np
	else:
		np1 = gene2np
		np2 = gene1np
	return np1, np2

def matching (np1,np2):
	test = np1
	bestscore = 0
	diff = len(np1) - len(np2)
	for i in range(diff) :
		toremove = (diff + i)
		test = np.delete(test, -toremove)
		score = sum(test == np2)
		if score > bestscore:
			bestscore = score
		test = np.delete(np1, i)
	return bestscore

print bestscore
	




