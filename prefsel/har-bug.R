library(hitandrun)

n.samples <- 10
constraints <- matrix(c(-1, 1, 1, 1, 0, -1), ncol=3, byrow=T)
user.constr <- list(constr=constraints, rhs=rep(0, nrow(constraints)), dir=rep('<=', nrow(constraints)))
all.constr <- mergeConstraints(simplexConstraints(ncol(constraints)), user.constr)
chain <- hitandrun(all.constr, n.samples=n.samples)
