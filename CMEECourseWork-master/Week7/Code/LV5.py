#!usr/bin/python

___author___ = "Petra Guy, pg5117@ic.ac.uk"
___version___ = "2.7"

""" The typical Lotka-Volterra Model simulated using scipy """
import sys
import pylab as p  # Contains matplotlib for plotting
import pandas as pd
import numpy as np
import random


def dR_dt(y0,x0, r,a,z,e):
    '''Iterative Lotka Voltera equation with random gaussian addition'''
    #import pdb; pdb.set_trace()
    results = pd.DataFrame({'R': y0, 'C': x0}, index=[0])
    R=0
    C=0
    """ Returns the growth rate of predator and prey populations at any
    given time step """
    for t in range(0,1000):
        if np.isinf(R) or np.isinf(C):
            print "values too large"
            break
        else:
            eps = random.gauss(0,1)
            Rt = results.iloc[t]['R']
            Ct = results.iloc[t]['C']
            R = Rt*eps + Rt + Rt*r -Rt*Rt*r/35 - a*Ct
            C = Ct - Ct*z+Ct*e*a*Rt
            df = pd.DataFrame({'R':R, 'C':C}, index=[0])
            #print df
        results = results.append(df, ignore_index=True)
    return results

def main(argv):
    r = float(sys.argv[1])
    a = float(sys.argv[2])
    z = float(sys.argv[3])
    e = float(sys.argv[4])
    x0 = float(sys.argv[5])
    y0 = float(sys.argv[6])
    columns = ['R', 'C']
    results = dR_dt(y0,x0, r,a,z,e)
    print results[columns].head()
    print len(results), "iterations with random gaussian"

if (__name__ == "__main__"):
	status = main(sys.argv)
	#sys.exit(0)