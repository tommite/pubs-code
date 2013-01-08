source('funcs.R')

## test theorem 1 inference
A <- c(4, 2, 2, 4)
B <- c(1, 3, 1, 3)
X <- c(3, 1, 3, 1)
Y <- c(2, 4, 4, 2)
perf <- rbind(A, B, X, Y)
pref <- t(as.matrix(c(3, 4)))
myres <- fastror(perf, pref)$nec
stopifnot(myres[1, 2] == 3)

## test strange behaviour
nalts <- 10
ncrit <- 5
npref <- 22
instance <- 7
set.seed(1911 + (nalts * 71) + (ncrit * 23) + (npref * 7) + instance)
strangeres <- performTest(nalts, ncrit, npref)
## check problem with UTAGMS implementation (numerical inaccuracy)
stopifnot(strangeres$uta[1,10] == TRUE)

## test strange behaviour #2
nalts <- 20
ncrit <- 5
npref <- 40
instance <- 9
set.seed(1911 + (nalts * 71) + (ncrit * 23) + (npref * 7) + instance)
strangeres <- performTest(nalts, ncrit, npref)
## check problem with UTAGMS implementation (numerical inaccuracy)
stopifnot(strangeres$uta[1,18] == TRUE)

