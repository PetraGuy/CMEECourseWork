#!usr/bin/python

#testing for align_seg_fasta


#import the two files and turn into one long string
gene1 = "'".join(line.rstrip("\n") for line in open("../../Week1/Data/407228326.fasta", "r"))
gene1 = gene1.replace("'", "")
#print gene1

gene2 = "'".join(line.rstrip("\n") for line in open("../../Week1/Data/407228412.fasta", "r"))
gene2 = gene2.replace("'","")
#print gene2
geneparts1 = gene1.partition("region")
reqgene1 = geneparts1[2]
#print len(reqgene1)

geneparts2 = gene2.partition("16")
reqgene2 = geneparts2[2]


toremove = len(reqgene2)-len(reqgene1)

gene2test = reqgene2[:-toremove]

print gene2test
import numpy as np
 
gene1np = np.array(list(reqgene1))
gene2np = np.array(list(gene2test))
print gene2np

#print np.array_equal(gene1np,gene2np)
print sum(gene1np == gene2np)

###############################



