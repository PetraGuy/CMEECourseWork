#!usr/bin/python


"""Chapter 6 Regular Expressions"""

__author__ = "Petra Guy, pg5117@ic.ac.uk"
__version__ = "2.7"

import re
import pandas as pd


f = open('../Data/blackbirds.txt', 'r')
text = f.read()
f.close()

# remove tabs and spaces and put a space in:
text = text.replace('\t',' ')
text = text.replace('\n',' ')

text = text.decode('ascii', 'ignore')

my_reg = pd.DataFrame()

matchK = re.findall(r'Kingdom (\w+)', text)
my_reg['Kingdom'] = (matchK)

matchP = re.findall(r'Phylum (\w+)', text)
my_reg['Phylum'] = (matchP)

matchG = re.findall(r'Species (\w+)', text)
my_reg['Genus'] = matchG

matchS = re.findall(r'Species\s*[A-Z]\w*\s*[a-z] (\w+)', text)
my_reg['Species'] = matchS

print my_reg


