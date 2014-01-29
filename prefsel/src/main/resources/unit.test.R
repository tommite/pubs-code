EPS <- 0.02 # allowed error

a <- c(1, 0)
b <- c(0, 1)
c <- c(0.75, 0.75)

performances <- rbind(a, b, c)
constraints <- matrix(b - a, nrow=1) # a > b -> b - a <= 0
pair <- c(1, 3)

source('compute.metrics.R')

stopifnot(abs(results$dvf - 0.5) < EPS)
stopifnot(results$win == 1)
stopifnot(results$apn == 2)
stopifnot(abs(results$era - 2/3) < EPS)
stopifnot(abs(results$wpe - 1/6) < EPS)
stopifnot(abs(results$wre - 2/3) < EPS)
