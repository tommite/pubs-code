source('lib/plots.R')

args <- commandArgs(trailingOnly=TRUE)

thrombo <- readRDS('data/thrombo-problem.rds')
cuts <- readRDS('data/thrombo-equiprob.seed.1.rds')

pdf(args[1])
#plot.all(cuts[1:5], thrombo$w.exact, thrombo$smaa.exact$ra['Enoxaparin',1])
plot.ent.ra(cuts, thrombo$smaa.exact$ra['Enoxaparin',1], do.legend=F)
#plot.all(cuts, thrombo$w.exact, thrombo$smaa.exact$ra['Enoxaparin',1])
dev.off()
