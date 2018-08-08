#!usr/bin/python

"""CUsing igraph to plot network"""

__author__ = "Petra Guy, pg5117@ic.ac.uk"
__version__ = "2.7"

import pandas as pd
import numpy as np
import igraph
import pylab

e = open('../Data/QMEE_Net_Mat_edges.csv', 'r')
Adj = pd.read_csv(e) # this is the adjacency matrix
e.close()

n = open('../Data/QMEE_Net_Mat_nodes.csv')
nodes = pd.read_csv(n) #names of nodes
n.close()

a_numpy = Adj.as_matrix()
conn_indices = np.where(a_numpy)
# Get the values as np.array, it's more convenenient.
weights = a_numpy[conn_indices]

# a sequence of (i, j) tuples, each corresponding to an edge from i -> j
edges = zip(*conn_indices)

# initialize the graph from the edge sequence

G = igraph.Graph(edges=edges, directed=True)

# assign node names and weights to be attributes of the vertices and edges
# respectively
node_names = nodes['id']
G.es['width'] = weights
G.vs['label'] = node_names
G.es['weight'] = weights

G.es['width'] = weights/10
# plot the graph
out = igraph.plot(G,'../Results/Netspy.pdf', layout="rt", labels=True, margin=80)
print 'Netspy.pdf saved to Results'
