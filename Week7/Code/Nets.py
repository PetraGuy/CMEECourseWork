import networkx as nx
import scipy as sc
import matplotlib.pyplot as plt
#import matplotlib.animation as ani #for animation

def GenRdmAdjList(N, C):
    Ids = range(N)
    ALst = []
    #for i in Ids: #do this N times ...Why N times? If we're just selecting random
    if sc.random.uniform(0,1,1) < C:
        Lnk = sc.random.choice(Ids,2).tolist() #selects 2 nos from range N
    if Lnk[0] != Lnk[1]:
        ALst.append(Lnk)
    return ALst
## Assign body mass range
SizRan = ([-10,10]) #use log scale
## Assign number of species (MaxN) and connectance (C)
MaxN = 30
C = 0.75
## Generate adjacency list:
AdjL = sc.array(GenRdmAdjList(MaxN, C))
## Generate species (node) data:
Sps = sc.unique(AdjL) # get species ids
Sizs = sc.random.uniform(SizRan[0],SizRan[1],MaxN)# Generate body sizes (log10 scale)
###### The Plotting #####
plt.close('all')
##Plot using networkx:
## Calculate coordinates for circular configuration:
## (See networkx.layout for inbuilt functions to compute other types of node
# coords)
pos = nx.circular_layout(Sps)