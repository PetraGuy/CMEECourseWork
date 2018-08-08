#Checking this little function to see if it actually worked, because
#I couldnt work out what doc test was telling me.


def is_an_oak(name):

	print len(name)
	if (len(name) == 7):
		return name.lower() == ("quercus")
	else:
		return name.lower().startswith('quercus ')

if is_an_oak("Quercus robur "):
	print "oak"
else:
	print "nope"
