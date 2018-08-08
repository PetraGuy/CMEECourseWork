#!usr/bin/python

___author___ = "Petra Guy, pg5117@ic.ac.uk"
___version___ = "2.7"

""" The typical Lotka-Volterra Model simulated using scipy """
import sys
import scipy as sc
import scipy.integrate as integrate
import pylab as p  # Contains matplotlib for plotting
import time


start = time.time()
def dR_dt(z0, t, r,a,z,e):
    """ Returns the growth rate of predator and prey populations at any
    given time step """
    R = z0[0]
    C = z0[1]
    dRdt = r * R - a * R * C
    dydt = -z * C + e * a * R * C
    return sc.array([dRdt, dydt])

def print_plot(pops,t):
    prey, predators = pops.T  # What's this for? - transpose
    f1 = p.figure()  # Open empty figure object
    p.plot(t, prey, 'g-', label='Resource density')  # Plot
    p.plot(t, predators, 'b-', label='Consumer density')
    p.grid()
    p.legend(loc='best')
    p.xlabel('Time')
    p.ylabel('Population')
    p.title('Consumer-Resource population dynamics')
    #p.show()
    f1.savefig('../Results/prey_and_predators_2.pdf')  # Save figure

def main(argv):
    r = float(sys.argv[1])
    a = float(sys.argv[2])
    z = float(sys.argv[3])
    e = float(sys.argv[4])
    t = sc.linspace(0, 15, 1000)
    x0 = 10
    y0 = 5
    z0 = sc.array([x0, y0])
    pops, infodict = integrate.odeint(dR_dt, z0, t, args=(r,a,z,e), full_output=True)
    infodict['message']
    print_plot(pops,t)


print "LV2 takes %f s to run" %(time.time() - start)
if (__name__ == "__main__"):
	status = main(sys.argv)
	#sys.exit(status)