source('lib/code.R')
source('lib/rnd-problem.R')

args <- commandArgs(trailingOnly=TRUE)
n <- as.integer(args[3])
k <- as.integer(args[4])

opts <- list(cuts=10, planes=3e3)

meas <- gen.problem(n=n, k=k)

set.seed(as.integer(args[2]))

##w <- simplex.sample(n, N)$samples
##smaa(meas, w)

true.w <- simplex.sample(n, 1)$samples

cuts <- get.cuts(true.w, opts$cuts, meas, opts$planes, equal.w.prob=FALSE, ranking=FALSE, sample.planes=sample.planes.restricted.shakeandbake(k))

fname <- gsub(".rds", paste0(".n.", n, ".k.", k, ".seed.", args[2], ".rds"), args[1])

saveRDS(cuts, fname)
