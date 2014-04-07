library(plyr)

args <- commandArgs(trailingOnly=TRUE)

seeds <- 1:140

norm <- 1 # max entropy for the case

## Computes uncertainty coefficient
comp.r <- function(test) {
    (test$stopping$h - mean(test$stopping$all.ent)) / test$stopping$h
}

h <- function(test) {
    test$stopping$h
}

tests.volu <- lapply(seeds, function(seed) {readRDS(paste0('data/thrombo-voluprob.seed.', seed, '.rds'))})

uc.volu <- laply(tests.volu, function(test) {laply(test, comp.r)})
h.volu <- laply(tests.volu, function(test) {laply(test, h)})

pdf(args[1], height=4)

par(mfrow=c(1, 2))
boxplot(uc.volu, xlab='Question', ylab='Uncertainty coefficient R', ylim=c(0, 1))
boxplot(h.volu, xlab='Question', ylab='Entropy H', ylim=c(0, 1))
dev.off()


