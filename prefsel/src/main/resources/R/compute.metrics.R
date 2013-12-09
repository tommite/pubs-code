library(hitandrun)

## Variable 'constraints' contains the input, of which each row is a
## linear constraint of type ax <= 0

constr <- list(constr=constraints, dir=rep('<=', nrow(constraints)), rhs=rep(0, nrow(constraints)))




## Return value has to be list called result, of which each element
## is a single metric
result <- list(hDVF=0.0)
