source('lib/code.R')
source('run/thrombo-problem.R')
source('lib/simplex.heat.R')

args <- commandArgs(trailingOnly=TRUE)

values.fn <- function(w) {
    values <- smaa.values(meas=meas, pref=w)
    ranks <- smaa.ranks(values)
    ra <- smaa.ra(ranks)
    ent <- smaa.entropy.choice(ra)
    c(ra["Enoxaparin",1], ent,
         mean(values[,"Enoxaparin"]),
         mean(values[,"Heparin"]))
}

data.hm <- simplex.calc.raster(values.fn, 0.001)

transform.inv <- simplex.createTransform(n=3, inverse=TRUE, keepHomogeneous=FALSE)
x.exact <- transform.inv %*% c(w.exact,1)

pdf(args[1], width=7.5, height=3)
par(mfrow=c(1,2))
par(mar=c(1, 1, 1, 1))
lim <- 0.15
criteria <- c(expression(w[p]==1), expression(w[d]==1), expression(w[b]==1))
plot.raster.polytope(data.hm$data[,,1], data.hm$xrng, data.hm$yrng, point=x.exact, criteria=criteria, bounds=lim, textcex=0.8, xaxt='n', yaxt='n', main='RAI(Enox, 1)')
plot.raster.polytope(data.hm$data[,,2], data.hm$xrng, data.hm$yrng, point=x.exact, criteria=criteria, bounds=lim, textcex=0.8, xaxt='n', yaxt='n', main='Entropy')

dev.off()
