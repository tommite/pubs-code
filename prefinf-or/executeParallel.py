import multiprocessing
from subprocess import call
import sys
from itertools import product

SCRIPT='test.R'
NRPROC=7
instId = sys.argv[1]
startIndex = (int(instId)-1) * NRPROC

## Script calling function
def callScript(parms):
	parStr = ' '.join(map(str, parms))
	print "Starting with parameters ", parStr
	call("R --vanilla --args " + parStr + " < " + SCRIPT, shell=True)

## Define the parameter set
alts = [10, 20, 50]
crit = [5]
nrpref = range(2, 41, 2)
instances = range(1, 11)

allTasks = list(product(alts, crit, nrpref, instances))
myTasks = allTasks[startIndex:(startIndex+NRPROC)]

## Start processing in parallel
pool = multiprocessing.Pool(processes=NRPROC)
print "Making computational tests for instance ID ", instId, " with parameter sets: ", myTasks
r = pool.map_async(callScript, myTasks)
r.wait() # Wait on the results
