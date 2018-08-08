#!usr/bin/python

"""Chapter 5 Practicals, modify lc2.py, Petra Guy
	Writing list comprehensions and for loops to extract elements from a list"""

__author__ = "Petra Guy, pg5117@ic.ac.uk"
__version__ = "2.7"

#imports

#constants

#functions



# Average UK Rainfall (mm) for 1910 by month
# http://www.metoffice.gov.uk/climate/uk/datasets
rainfall = (('JAN',111.4),
            ('FEB',126.1),
            ('MAR', 49.9),
            ('APR', 95.3),
            ('MAY', 71.8),
            ('JUN', 70.2),
            ('JUL', 97.1),
            ('AUG',140.2),
            ('SEP', 27.0),
            ('OCT', 89.4),
            ('NOV',128.4),
            ('DEC',142.2),
           )

# (1) Use a list comprehension to create a list of month,rainfall tuples where
# the amount of rain was greater than 100 mm.
 
# (2) Use a list comprehension to create a list of just month names where the
# amount of rain was less than 50 mm. 

# (3) Now do (1) and (2) using conventional loops (you can choose to do 
# this before 1 and 2 !). 

# ANNOTATE WHAT EVERY BLOCK OR IF NECESSARY, LINE IS DOING! 

# ALSO, PLEASE INCLUDE A DOCSTRING AT THE BEGINNING OF THIS FILE THAT 
# SAYS WHAT THE SCRIPT DOES AND WHO THE AUTHOR IS


#Here are the list comprehension methods. In the first, since we need each tuple
#element its just x for x in rainfall.
#For the second only want the first item in each element, hence x[0] for x in rainfall

rainfall_over100mm = [x for x in rainfall if x[1] > 100.0]
print rainfall_over100mm

months_less50mm = [x[0] for x in rainfall if x[1] < 50]
print months_less50mm

#The above with for loops

rain_over100mm = list()
for months in rainfall:
	if (months[1] > 100):
		rain_over100mm.append(months[0])
print rain_over100mm


