import multiprocessing
from subprocess import call
import sys
from itertools import product

NRPROC=7
instId = sys.argv[1]
startIndex = (int(instId)-1) * NRPROC

problem = 'voluprob'
if len(sys.argv) > 2:
        problem = sys.argv[2]

## Script calling function
def callScript(seed):
	print "Starting with seed", seed
	call("make data/thrombo-" + problem + ".rds SEED=" + str(seed), shell=True)
	print "Done"

## Define the parameter set (140 instances)
seeds = range(1, 141)

myTasks = seeds[startIndex:(startIndex+NRPROC)]

## Start processing in parallel
pool = multiprocessing.Pool(processes=NRPROC)
print "Making computational tests for instance ID", instId, "with seeds:", myTasks
r = pool.map_async(callScript, myTasks)
r.wait() # Wait on the results

