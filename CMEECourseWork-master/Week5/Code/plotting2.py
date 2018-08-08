#!usr/bin/python

"""Practising with matplot.lib"""

__author__ = "Petra Guy, pg5117@ic.ac.uk"
__version__ = "2.7"

import matplotlib.pyplot as plt
import pandas as pd
import pylab


df = pd.read_csv("../../Week3/Data/KeyWest.CSV")
print(df)
plt.plot(df['Year'],df['Temp'])
plt.show()
