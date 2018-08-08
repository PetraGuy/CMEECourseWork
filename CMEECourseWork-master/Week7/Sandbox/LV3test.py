#!usr/bin/python

___author___ = "Petra Guy, pg5117@ic.ac.uk"
___version___ = "2.7"

""" The typical Lotka-Volterra Model simulated using scipy """
import sys
import scipy as sc
import scipy.integrate as integrate
import pylab as p  # Contains matplotlib for plotting
import pandas as pd


def dR_dt(z0, r,a,z,e):
    """ Returns the growth rate of predator and prey populations at any
    given time step """
    #results = results.append(z0, index)
    columns = ['R', 'C']
    results = pd.DataFrame({'R':y0, 'C':x0}, index=[0])
    for t in range(0,1000):
        Rt = results.loc[t,'R']
        Ct = results.loc[t,'C']
        R = Rt + Rt*r -Rt*Rt*r - a*Ct
        C = Ct - Ct*z+Ct*e*a*Rt
        df = pd.DataFrame({'R':R, 'C':C}, index=[0])
        print df
        results = results.append(df, ignore_index=True)
    return results

def main(argv):
    x0 = 10
    y0 = 5
    z0 = sc.array([x0, y0])
    z0 = pd.DataFrame(data = z0)
    r = 1.  # Resource growth rate
    a = 0.1  # Consumer search rate (determines consumption rate)
    z = 1.5  # Consumer mortality rate
    e = 0.75
    dR_dt(z0, r,a,z,e)
    print results[columns].head()

if (__name__ == "__main__"):
	status = main(sys.argv)
sys.exit(status)