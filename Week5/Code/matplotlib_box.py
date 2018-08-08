#!usr/bin/python

"""Creating box plots in matplot lib"""

__author__ = "Petra Guy, pg5117@ic.ac.uk"
__version__ = "2.7"

from sklearn import datasets
from matplotlib.backends.backend_agg import FigureCanvasAgg as FigureCanvas
from matplotlib.figure import Figure
import numpy as np


# use the iris data
iris = datasets.load_iris()

# create the canvas, like ggplot
fig = Figure(figsize=(8, 6))
FigureCanvas(fig)

# create one subplot
# a 3-digit integer or three separate integers describing the position of the subplot.
# If the three integers are I, J, and K, the subplot is the Ith plot on a grid with J rows and K columns.
# returns: The axes of the subplot.
ax = fig.add_subplot(111)

# create boxplot
ax.boxplot(iris.data)

# label & size etc
ax.set_xticklabels(iris.feature_names)
ax.set_ylabel('cm')
ax.grid(False)

# save it to working directory
fig.savefig('test_boxplot_oo')

# can't show it without using pyplot
# fig.show()