library(hitandrun)

n.samples <- 1E4

## Variable 'constraints' contains the input, of which each row is a
## linear constraint of type ax <= 0

user.constr <- list(constr=constraints, dir=rep('<=', nrow(constraints)), rhs=rep(0, nrow(constraints)))

all.constr <- mergeConstraints(simplexConstraints(ncol(constraints)), user.constr)

chain <- hitandrun(all.constr, n.samples=n.samples)


## Return value has to be list called result, of which each element
## is a single metric
result <- list(hDVF=0.0)
