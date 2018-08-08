taxa = [ ('Myotis lucifugus','Chiroptera'),
         ('Gerbillus henleyi','Rodentia',),
         ('Peromyscus crinitus', 'Rodentia'),
         ('Mus domesticus', 'Rodentia'),
         ('Cleithrionomys rutilus', 'Rodentia'),
         ('Microgale dobsoni', 'Afrosoricida'),
         ('Microgale talazaci', 'Afrosoricida'),
         ('Lyacon pictus', 'Carnivora'),
         ('Arctocephalus gazella', 'Carnivora'),
         ('Canis lupus', 'Carnivora'),
        ]

# Write a short python script to populate a dictionary called taxa_dic 
# derived from  taxa so that it maps order names to sets of taxa. 
# E.g. 'Chiroptera' : set(['Myotis lucifugus']) etc. 

# ANNOTATE WHAT EVERY BLOCK OR IF NECESSARY, LINE IS DOING! 

# ALSO, PLEASE INCLUDE A DOCSTRING AT THE BEGINNING OF THIS FILE THAT 
# SAYS WHAT THE SCRIPT DOES AND WHO THE AUTHOR IS
	
"""Create a dictionary from the taxa list of tuples using two methods"""

#This first method creates a set of keys. Creating a set ensure the elements are unique

#keys = set([v for k, v in taxa])

#mydict = {}
#loop thru keys assigning each species if in taxa its family is equal to the key
#for key in keys:
#	mydict[key] = []
#	for item in taxa:
#		if item[1] == key:
#			mydict[key].append(item[0])
#print mydict 


#But this method using collections is super neat
#defaultdict(list) says that the values for each key are a list

from collections import defaultdict


mydict2 = defaultdict(list)
for k, v in taxa:
	mydict2[v].append(k)
	
print mydict2
        
