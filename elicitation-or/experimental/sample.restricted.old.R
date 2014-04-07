# sample separating hyperplanes such that reference alternatives differ on at most k attributes
sample.planes.restricted.old <- function(k) {
  # version based on direct construction of basis (3x faster)
  make.basis <- function(w, d) {
    n <- length(w)
    k <- n - length(d)
    w.eq <- w[d]
    w.nq <- w[-d]
    f <- 1 - sum(w.eq)
    basis <- sapply(1:k, function(i) {
      x <- rep(0, n)
      x[d] <- w.nq[i] / f
      x[-d][i] <- 1
      x
    })
    qr.Q(qr(basis))
  }

  function(constr, N) {
    W <- harSample(constr, N)
    n <- ncol(W)
    # sample N random directions in k-space
    B <- hypersphere.sample(k, N)
    # sample the n - k attributes along which the alternatives are equal
    D <- sapply(1:N, function(i) { sample.int(n, n - k) })
    if (is.vector(D)) { # in case n-k=1 ...
      D <- as.matrix(D)
    } else {
      D <- t(D)
    }
    # transform the N random directions to n-space
    A <- t(sapply(1:N, function(i) {
      w <- W[i,]
      b <- B[i,]
      d <- D[i,]
      basis <- make.basis(w,d)
      b %*% t(basis)
    }))
    apply(array(1:N), 1,
      function(x) { list(point=W[x,], normal=A[x,]) })
  }
}
