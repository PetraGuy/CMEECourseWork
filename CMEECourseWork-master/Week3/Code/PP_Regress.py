#!usr/bin/python
# The data consists of predator and prey mass by type of feeding interaction
# and predator lifestage. We need to subset the data first by type of
# feeding interation, then by predator lifestage.
# linear models are required for ln(Predmass) ~ ln(Preymass) for predator
# lifestage, for type of feeding interation.

""" Rewriting pp regress in python because my R attempt was shoddy"""

___author___ = "Petra Guy, pg5117@ic.ac.uk"
___version___ = "2.7"

import pdb
from scipy import stats
import pandas as pd
import numpy as np
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

# Now to access a subset FeedingDict['insectivorous'] for example
# Now I can take each element in Feeding Dict, subset in the same way
# by lifestage, do linear model, export data.

# use this for reporting in order to define column order
columns = ['feeding', 'stage', 'slope', 'intercept', 'r_value', 'p_value', 'std_err']

# create empty df
regress_data = pd.DataFrame()

# populate
for key, value in feeding_dict.items():
    for stage in life_stages:
        y = feeding_dict[key]['logPredMass']
        x = feeding_dict[key]['logPreyMass']
        slope, intercept, r_value, p_value, std_err = stats.linregress(x, y)
        mean = feeding_dict[key]['logPredMass'].mean()
        df = pd.DataFrame(
            {'feeding': key,
             'stage': stage,
             'slope': slope,
             'intercept': intercept,
             'r_value': r_value,
             'p_value': p_value,
             'std_err': std_err},
            index=[0]
        )
        regress_data = regress_data.append(df, ignore_index=True)
        print key, stage, mean
        print slope, intercept, r_value, p_value, std_err

print regress_data[columns].head()
pass
