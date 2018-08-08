import csv
import sys
import pdb
import doctest

#Define function
def is_an_oak(name):
	
	""" Returns True if name is starts with 'quercus '
	
	>>> is_an_oak('quercus')
	True
    
	>>> is_an_oak('Pinus sylvestris')
	False
		
	>>> is_an_oak('Quercus')
	True
	
	>>> is_an_oak('Quercustard')
	False
	"""
	#return name.lower().startswith('quercus ') #The space after the s ensures only quercus is matched
	#I added this bit to catch things with no space	
	 
	if (len(name) ==7):
		return name.lower() == ("quercus") # this bit tests the oakness
	else:								   # if there's no space
		return name.lower().startswith('quercus ')  #  with the space
		
print(is_an_oak.__doc__)

def main(argv): 
    f = open('../Data/TestOaksData.csv','rb')
    g = open('../Results/JustOaksData.csv','wb')
    taxa = csv.reader(f)
    csvwrite = csv.writer(g)
    oaks = set()
    for row in taxa:
        print row
        print "The genus is", row[0]
        if is_an_oak(row[0]):
            print row[0]
            print 'FOUND AN OAK!'
            print " "
            csvwrite.writerow([row[0], row[1]])    
    
    return 0
    
if (__name__ == "__main__"):
    status = main(sys.argv)

doctest.testmod()
