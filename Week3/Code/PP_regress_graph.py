#!usr/bin/python
# The data consists of predator and prey mass by type of feeding interaction
# and predator lifestage. We need to subset the data first by type of
# feeding interation, then by predator lifestage.
# linear models are required for ln(Predmass) ~ ln(Preymass) for predator
# lifestage, for type of feeding interation.

""" Trying out graphing in python with matplotlib """

___author___ = "Petra Guy, pg5117@ic.ac.uk"
___version___ = "2.7"

import pdb
import matplotlib.pyplot as plt
from scipy import stats
import pandas as pd
import numpy as np
from matplotlib.backends.backend_agg import FigureCanvasAgg as FigureCanvas
from matplotlib.figure import Figure
from pprint import pprint as pp

# convert mg to grams
mydata = pd.read_csv("../Data/EcolArchives-E089-51-D1.csv")
mask = mydata['Prey.mass.unit'] == 'mg'
mydata.loc[mydata['Prey.mass.unit'] == 'mg', 'Prey.mass'] = mydata['Prey.mass'] * 1e-3

# take logs of prey mass and pred mass
mydata['logPreyMass'] = np.log(mydata['Prey.mass'])
mydata['logPredMass'] = np.log(mydata['Predator.mass'])

# Create an array of the type of feeding interactions
Feeding = mydata[u'Type.of.feeding.interaction'].unique()
# Create an array of the predator lifestages
life_stages = mydata[u'Predator.lifestage'].unique()

# Subset the database by each feeding interaction
feeding_dict = {elem: pd.DataFrame for elem in Feeding}

for key in feeding_dict.keys():
    mask = mydata[u'Type.of.feeding.interaction'] == key
    feeding_dict[key] = mydata[mask]

for key, value in feeding_dict.items():
    y = feeding_dict[key]['logPredMass']
    x = feeding_dict[key]['logPreyMass']
    fig, ax = plt.subplots()
    for stage in life_stages:
        #need to assign random colour here
        ax.plot(x,y, marker='o', linestyle='', c = stage, ms=12, label=key)
        ax.legend()
        plt.show()
# this plots scatterplots for each feeding type - but each lifestage is is plotted together
#if there was a colour by stage in the second for loop...