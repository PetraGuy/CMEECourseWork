

import pandas as pd
import numpy as np
from sklearn import datasets
from matplotlib.backends.backend_agg import FigureCanvasAgg as FigureCanvas
from matplotlib.figure import Figure

# import some data
iris = datasets.load_iris()

# create dataframe from numpy arrays
df_data = pd.DataFrame(data=iris.data, columns=iris.feature_names)
df_target = pd.DataFrame(data=iris.target, columns=['target'])
df = pd.concat([df_data, df_target], axis=1)

# create one figure
fig = Figure(figsize=(8, 6))
FigureCanvas(fig)
ax = fig.add_subplot(111)

# find all unique values of target
levels = df.groupby(['target']).all().index

for target in levels:
    # select the data
    mask = df['target'] == target
    x, y = df[mask]['petal length (cm)'], df[mask]['petal width (cm)']
    # add scatter to the axes
    ax.scatter(x, y, edgecolor='k')
    # fit and plot regression
    fit = np.polyfit(x, y, deg=1)
    ax.plot(x, fit[0] * x + fit[1])

fig.savefig('../Results/test_regression_group')