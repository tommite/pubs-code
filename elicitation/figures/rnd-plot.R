library(plyr)
source('lib/code.R')
source('lib/plots.R')

## Computes uncertainty coefficient with a given cut
uc <- function(cut) {
    nalts <- nrow(cut$ra)
    comp.uc(nalts, cut$stopping)
}

args <- commandArgs(trailingOnly=TRUE)

seed.rng <- 1:20
n.rng <- 3:10
k.rng <- 2:10
plot.vals <- c(3, 5, 7, 8, 9)

read.data <- function(n, k, seed) {
    seedInt = seed + (10 * n + k)
    readRDS(paste0("data/random-restricted.n.", n, ".k.", k, ".seed.", seedInt, ".rds"))
}

pdf(args[1], height=4)

par(mfrow=c(1, 2))

plot(1, type='l', xlim=c(1,10), ylim=c(0, 1), xlab='Question', ylab='avg(R)', main='k=2')

for (ind in 1:length(plot.vals)) {
    n <- plot.vals[ind]
    ## compute the 0th question entropy
    res <- alply(seed.rng, 1, read.data, n=n, k=2)
    uc.all <- laply(res, function(cuts) {laply(cuts, uc)}, .drop=FALSE)
    uc.avgs <- colMeans(uc.all)
    lines(uc.avgs, lty=ind)
}

legend(legend=alply(plot.vals, 1, function(n) {paste0('n=', n)}),
       x='topright', lty=1:length(plot.vals), cex=0.5)

plot(1, type='l', xlim=c(1,10), ylim=c(0, 1), xlab='Question', ylab='avg(R)', main='n=10')

for (ind in 1:length(plot.vals)) {
    k <- plot.vals[ind]
    res <- alply(seed.rng, 1, read.data, n=10, k=k)
    uc.all <- laply(res, function(cuts) {laply(cuts, uc)}, .drop=FALSE)
    uc.avgs <- colMeans(uc.all)
    lines(uc.avgs, lty=ind)
}

legend(legend=alply(plot.vals, 1, function(n) {paste0('k=', n)}),
       x='topright', lty=1:length(plot.vals), cex=0.5)

dev.off()
