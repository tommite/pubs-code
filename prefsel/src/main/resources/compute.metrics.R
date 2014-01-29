library(hitandrun)
library(smaa)

n.samples <- 1E4

## Variables from Java
######
## constraints: contains the input, of which each row is a linear constraint of type ax <= 0
## performances: contains the performances (each row one alternative)
## pair: 2-element int vector containing indices of the alternatives to compute the metrics for

### Check conditions on input vars ###

if (!exists('constraints') || !is.matrix(constraints) || nrow(constraints) < 1) {
  constraints <- matrix(ncol=ncol(performances), nrow=0)
}

user.constr <- list(constr=constraints, dir=rep('<=', nrow(constraints)), rhs=rep(0, nrow(constraints)))
all.constr <- mergeConstraints(simplexConstraints(ncol(constraints)), user.constr)
chain <- hitandrun(all.constr, n.samples=n.samples)

compute.dvf <- function(a1, a2) {
  singlemeas <- rbind(a1, a2)
  meas <- array(0, dim=c(n.samples, 2, length(a1)))
  for (i in 1:n.samples) {
    meas[i,,] <- singlemeas
  }
  smaavals <- smaa.values(meas, chain)
  smaaranks <- smaa.ranks(smaavals)
  ra <- smaa.ra(smaaranks)
  min(ra[,1])
}

all.perfs <- array(0, dim=c(n.samples, nrow(performances), ncol(performances)))
dimnames(all.perfs) <- list(NULL, c('a1', 'a2', 'a3'), c('c1', 'c2'))
for (i in 1:n.samples) {
    all.perfs[i,,] <- performances
}

a1.over.a2 <- performances[pair[2],] - performances[pair[1],]
a2.over.a1 <- -a1.over.a2

a1.chosen.constr <- mergeConstraints(list(constr=a1.over.a2, dir='<=', rhs=0),
                                     all.constr)
a2.chosen.constr <- mergeConstraints(list(constr=a2.over.a1, dir='<=', rhs=0),
                                     all.constr)
a1.chosen.chain <- hitandrun(a1.chosen.constr, n.samples=n.samples)
a2.chosen.chain <- hitandrun(a2.chosen.constr, n.samples=n.samples)

pwi.all <- smaa.pwi(smaa.ranks(smaa.values(all.perfs, chain)))
a1.chosen.ranks <- smaa.ranks(smaa.values(all.perfs, a1.chosen.chain))
a2.chosen.ranks <- smaa.ranks(smaa.values(all.perfs, a2.chosen.chain))
pwi.a1.chosen <- smaa.pwi(a1.chosen.ranks)
pwi.a2.chosen <- smaa.pwi(a2.chosen.ranks)
ra.a1.chosen <- smaa.ra(a1.chosen.ranks)
ra.a2.chosen <- smaa.ra(a2.chosen.ranks)

nr.necessary <- function(pwi) {
    sum(pwi == 1)
}

necessary <- function(pwi) {
    pwi == 1
}

possible <- function(pwi) {
    pwi > 0
}

compute.win <- function() {
    nr.all <- nr.necessary(pwi.all)
    min(nr.necessary(pwi.a1.chosen) - nr.all, nr.necessary(pwi.a2.chosen) - nr.all)
}

compute.apn <- function() {
    apn.all <- sum(possible(pwi.all) - necessary(pwi.all))
    max(apn.all - sum((possible(pwi.a1.chosen) - necessary(pwi.a1.chosen))),
        apn.all - sum((possible(pwi.a2.chosen) - necessary(pwi.a2.chosen))))
}

compute.era <- function() {
    0
}

a1 <- performances[pair[1],]
a2 <- performances[pair[2],]

## Return value has to be list called result, of which each element
## is a single metric (vector for the 'pairs')
results <- list(dvf=compute.dvf(a1, a2), win=compute.win(),
                apn=compute.apn(), era=compute.era())
