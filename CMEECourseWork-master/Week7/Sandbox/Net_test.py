

import igraph
import pandas as pd
import numpy as np

node_names = ['A', 'B', 'C']
a = pd.DataFrame([[1,2,3],[3,1,1],[4,0,2]], index=node_names, columns=node_names)
a_numpy = a.as_matrix()
conn_indices = np.where(a_numpy)
# Get the values as np.array, it's more convenenient.
weights = a_numpy[conn_indices]

# a sequence of (i, j) tuples, each corresponding to an edge from i -> j
edges = zip(*conn_indices)

# initialize the graph from the edge sequence
G = igraph.Graph(edges=edges, directed=True)

# assign node names and weights to be attributes of the vertices and edges
# respectively
G.vs['label'] = node_names
G.es['weight'] = weights

# I will also assign the weights to the 'width' attribute of the edges. this
# means that igraph.plot will set the line thicknesses according to the edge
# weights
G.es['width'] = weights

# plot the graph, just for fun
igraph.plot(G, layout="rt", labels=True, margin=80)