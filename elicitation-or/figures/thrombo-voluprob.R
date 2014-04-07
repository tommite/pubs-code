source('lib/plots.R')

args <- commandArgs(trailingOnly=TRUE)

nr.cuts <- 3

thrombo <- readRDS('data/thrombo-problem.rds')
cuts.1 <- readRDS('data/thrombo-voluprob.seed.1.rds')
cuts.2 <- readRDS('data/thrombo-voluprob.seed.2.rds')

pdf(args[1], height=7)
#plot.all(cuts[1:5], thrombo$w.exact, thrombo$smaa.exact$ra['Enoxaparin',1])

par(mfrow=c(2,2))
plot.ent.ra(cuts.1, thrombo$smaa.exact$ra['Enoxaparin',1], do.legend=F)
plot.cuts(cuts.1[1:nr.cuts], thrombo$w.exact)

plot.ent.ra(cuts.2, thrombo$smaa.exact$ra['Enoxaparin',1], do.legend=F)
plot.cuts(cuts.2[1:nr.cuts], thrombo$w.exact)

#plot.all(cuts, thrombo$w.exact, thrombo$smaa.exact$ra['Enoxaparin',1])
dev.off()
