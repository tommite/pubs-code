gen.problem <- function(n, k, m=8, N=1E4) {
    set.seed(n * 10 + k) ## all tests with same n & k have same problem

    meas <- array(NA, dim=c(N, m, n))
    
    for (j in 1:n) {
        ## shape parameters
        a <- c(12:15,30,40,40,40,35)
        b <- c(rep(35, 4),40,40,30,15,10)
        ## choose distribution for each criterion
        d <- sample.int(length(a), m)
        ## sample measurements (alternative i, criterion j)
        for (i in 1:m) {
            meas[ , i, j] <- rbeta(N, a[d[i]], b[d[i]])
        }
    }
    meas
}
