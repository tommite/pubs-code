source('lib/code.R')

## plane.normal2intercept
x0 <- c(1, 1)
normal <- c(-1, 1)
stopifnot(plane.getintercept(x0, normal) == 0)

