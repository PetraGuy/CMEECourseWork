#!usr/bin/python

"""5.8 Reading csv files"""

__author__ = "Petra Guy, pg5117@ic.ac.uk"
__version__ = "2.7"

#imports

import csv

#read a csv file and append stuff to each row

f = open('../Data/testcsv.csv','rb')

csvread = csv.reader(f)
temp = []
for row in csvread:
		temp.append(tuple(row))
		print row
		print "The species is", row[0]
f.close()

#write only the species to another file

f = open('../Data/testcsv.csv', 'rb')
g = open('../Results/bodymass.csv', 'wb')

csvread = csv.reader(f)
csvwrite = csv.writer(g)
for row in csvread:
		print row
		csvwrite.writerow([row[0], row[4]])
f.close()
g.close()

