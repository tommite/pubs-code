source('lib/plots.R')

args <- commandArgs(trailingOnly=TRUE)

nr.cuts <- 3

thrombo <- readRDS('data/thrombo-problem.rds')
cuts <- readRDS('data/thrombo-restricted.rds')

pdf(args[1], height=4)
par(mfrow=c(1,2))
plot.ent.ra(cuts, thrombo$smaa.exact$ra['Enoxaparin',1], do.legend=FALSE)
plot.cuts(cuts[1:nr.cuts], thrombo$w.exact)
dev.off()
