from itertools import product
from os import walk

f = []
for (dirpath, dirnames, filenames) in walk('./'):
    f.extend(filenames)
    break

def realseed(n, k, seed_seq):
    return n * 10 + k + seed_seq

def checkfile(n, k, seed_seq, files):
    fname = 'random-restricted.n.' + str(n) + '.k.' + str(k) + '.seed.' + str(realseed(n, k, seed_seq)) + '.rds'
    return fname in files

seeds = range(1, 21) # 20 seeds
nk = range(3, 11)
allTasks = list(product(nk, [2], seeds))
allTasks.extend(list(product([10], nk, seeds)))


for index in range(0, len(allTasks)):
    (n, k, seed) = allTasks[index]
    if not checkfile(n, k, seed, f):
        print 'INDEX ' + str(index+1) +  ': n ' + str(n) + ' k ' + str(k) \
        + ' seedseq ' + str(seed) + ' seed ' + str(realseed(n, k, seed))

