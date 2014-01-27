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

a1.over.a2 <- performances[pair[2],] - performances[pair[1],]
a2.over.a1 <- -a1.over.a2

a1.chosen.constr <- mergeConstraints(list(constr=a1.over.a2, dir='<=', rhs=0),
                                     all.constr)
a2.chosen.constr <- mergeConstraints(list(constr=a2.over.a1, dir='<=', rhs=0),
                                     all.constr)
a1.chosen.chain <- hitandrun(a1.chosen.constr, n.samples=n.samples)
a2.chosen.chain <- hitandrun(a2.chosen.constr, n.samples=n.samples)

compute.win <- function() {
}

a1 <- performances[pair[1],]
a2 <- performances[pair[2],]

## Return value has to be list called result, of which each element
## is a single metric (vector for the 'pairs')
results <- list(dvf=compute.dvf(a1, a2))
