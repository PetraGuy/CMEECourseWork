#I need to slice a dataframe by factors and then access each slide to do analysis
import numpy as np
import pandas as pd
data = pd.DataFrame({'Names': ['Joe', 'John', 'Jasper', 'Jez'] *4, 'Ob1' : np.random.rand(16), 'Ob2' : np.random.rand(16)})

#create unique list of names
UniqueNames = data.Names.unique()

#create a data frame dictionary to store your data frames
DataFrameDict = {elem : pd.DataFrame for elem in UniqueNames}

for key in DataFrameDict.keys():
    DataFrameDict[key] = data[:][data.Names == key]

for key,value in DataFrameDict.items():
	mean = DataFrameDict[key]['Ob1'].mean()
	print key, mean
#practise subsetting
		
