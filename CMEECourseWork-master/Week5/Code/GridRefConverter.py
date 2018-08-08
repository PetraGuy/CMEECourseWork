#!usr/bin/python

"""convert easting and northing to OS GRID"""

__author__ = "Petra Guy, pg5117@ic.ac.uk"
__version__ = "2.7"

import pandas as pd
import math


def getOSGridReference(e, n):

    # The characters for the start of grid ref
    gridChars = "ABCDEFGHJKLMNOPQRSTUVWXYZ"

    # get the 100km-grid indices, so need 6 figs, add 0s if required
    e100k = math.floor(e / 100000)
    n100k = math.floor(n / 100000)

    # translate those into numeric equivalents
    # of the grid letters
    l1 = (19 - n100k) - (19 - n100k) % 5 + math.floor((e100k + 10) / 5)
    l2 = (19 - n100k) * 5 % 25 + e100k % 5

    letPair = gridChars[int(l1)] + gridChars[int(l2)]

    # strip 100km-grid indices from easting & northing,
    # round to 100m
    e100m = math.trunc(round(float(e) / 100))
    egr = str(e100m).rjust(4, "0")[1:]
    if n >= 1000000:
        n = n - 1000000  # Fix Shetland northings
    n100m = math.trunc(round(float(n) / 100))
    ngr = str(n100m).rjust(4, "0")[1:]

    return letPair + egr + ngr


# test
assert getOSGridReference(96000, 906000) == 'NA960060'
assert getOSGridReference(465149, 1214051) == 'HP651141'

# import the file
woodLocs = pd.read_csv(r'../Data/Area_Site.csv')

# add easting and northings; *by100 to make 6 fig
woodLocs['Easting'] = woodLocs['Easting'] * 100
woodLocs['Northing'] = woodLocs['Northing'] * 100

woodLocsOS = woodLocs

# this is a bit like R apply
woodLocsOS['OS'] = woodLocsOS.apply(lambda x: getOSGridReference(x['Easting'], x['Northing']), axis=1)


print woodLocsOS.head()

print woodLocsOS