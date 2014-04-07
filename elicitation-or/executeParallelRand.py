import multiprocessing
from subprocess import call
import sys
from itertools import product

NRPROC=1 # changed to run 1 proc / node
instId = sys.argv[1]
startIndex = (int(instId)-1) * NRPROC

## Script calling function
def callScript(params):
	n = str(params[0])
	k = str(params[1])
	seedInt = params[2] + (10 * params[0] + params[1])
	seed = str(seedInt)
	print "Starting with seed", seed, "n", n, "k", k
	call("make data/random-restricted.rds SEED=" + seed + " N=" + n + " K=" + k, shell=True)
	print "Done"

## Define the parameter set (multiple instances or each parameter combination)
## For n=3, ..., 10, k=2
## For n=10, k=3, ..., 10
seeds = range(1, 21) # 20 seeds
nk = range(3, 11)
allTasks = list(product(nk, [2], seeds))
allTasks.extend(list(product([10], nk, seeds)))
myTasks = allTasks[startIndex:(startIndex+NRPROC)]

## Start processing in parallel
pool = multiprocessing.Pool(processes=NRPROC)
print "Making computational tests for instance ID", instId
r = pool.map_async(callScript, myTasks)
r.wait() # Wait on the results

