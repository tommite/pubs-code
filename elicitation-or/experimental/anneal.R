library(smaa)
library(hitandrun)
source('code.R')

source('simplex.heat.R')

anneal <- function(s0, E, neighbour, temperature, P, kmax, state.to.cut, max.E) {
  s <- s0
  e <- E(s)
  best.s <- s
  best.e <- e
  k <- 0
  for (k in 1:kmax) {
    T <- temperature(k/kmax)
    new.s <- neighbour(s)
    new.e <- E(new.s)
    if (P(e, new.e, T) > runif(1)) {
      s <- new.s
      e <- new.e
#      lines.cut(state.to.cut(new.s), lty=2, col=heat.colors(100)[ceiling((new.e / max.E) * 100)])
    } else {
#      lines.cut(state.to.cut(new.s), lty=3, col=heat.colors(100)[ceiling((new.e / max.E) * 100)])
    }
    if (!is.na(new.e) && new.e < best.e) {
#      print(paste("New best:", new.e, "@", k))
      best.s <- new.s
      best.e <- new.e
    }
  }
  best.s
}

anneal.cut <- function(w, u, state, minSize=500, entropy=smaa.entropy.choice, max.entropy=1) {
  n <- ncol(init$basis$basis)
  N <- nrow(w)

  # transform an annealing state (in sampling space coordinates) to a cut (in weight space coordinates)
  state.to.cut <- function(s) {
    list(
      point = state$transform %*% s$point,
      normal = state$basis$basis %*% s$normal
    )
  }

  # Energy: entropy for the given state
  energy <- function(s) {
    question.entropy.pairwise(w, u, state.to.cut(s), minSize=minSize/N, entropy=entropy)['h']
  }

  normal.to.transform <- function(a) {
    basis <- solution.basis(list(constr=t(a), dir='=', rhs=0))
    createTransform(basis, keepHomogeneous=TRUE)
  }

  # copy/paste from HAR
  homogeneousCoordinateConstraint <- function(n) {
    list(constr=c(rep(0, n), 1), rhs=c(1), dir=c("="))
  }

  # Neighbour: generate a "close" hyperplane to the current one
  neighbour <- function(s) {
    n <- length(s$normal)

    # Step 1: choose an interior point to rotate around
    transform <- normal.to.transform(s$normal)
    x <- transform %*% findInteriorPoint(transformConstraints(transform, state$constr), homogeneous=TRUE)
    # w <- state$transform %*% x

    # Step 2: generate a random rotation offset (i.e. displacement on the hypersphere)
    rot <- transform %*% c(runif(n=n - 1, min=-0.4, max=0.4), 1) # rotation at most pi/4 with min/max 1
    a <- s$normal + rot[1:(length(rot) - 1)]
    a <- as.vector(a) / sqrt(t(a) %*% a)

    # Step 3: find the range for translation (max / min a * x subject to the constraints)
    hom <- homogeneousCoordinateConstraint(n)
    h <- rcdd::makeH(state$constr$constr, state$constr$rhs, hom$constr, hom$rhs)
    obj <- c(a, 0)
    tr.min <- t(c(a, 0)) %*% rcdd::lpcdd(h, obj, minimize=TRUE)$primal.solution
    tr.max <- t(c(a, 0)) %*% rcdd::lpcdd(h, obj, minimize=FALSE)$primal.solution

    # Step 4: generate a random translation (making sure that minSize is met)
    len <- (tr.max - tr.min) / 5
    d <- runif(n=1, min=max(-len, tr.min), max=min(len, tr.max))
    x <- x + d * c(a, 0)

    list(point=x, normal=a)
  }

  P <- function(e0, e1, T) {
    if (is.na(e1)) {
      0
    } else if (e1 < e0) {
      1
    } else {
      exp((e0 - e1)/T)
    }
  }

  n.iter <- 1000

  temperature <- function(x) {
    max.entropy * 0.975^(x*100)
    #max.entropy / log(x*n.iter)
  }

  # Generate the initial hyperplane
  w0 <- apply(w, 2, mean) # approximate centroid
  tr.inv <- createTransform(state$basis, inverse=TRUE)
  x0 <- tr.inv %*% c(w0, 1)
  a0 <- t(hypersphere.sample(n, 1))
  s0 <- list(point=x0, normal=a0)

  #print(state.to.cut(s0))
  #lines.cut(state.to.cut(s0), lty=1)

  s <- anneal(s0, energy, neighbour, temperature, P, n.iter, state.to.cut, max.E=max.entropy)

  #lines.cut(state.to.cut(s), lty=4)

  #print(energy(s0))
  #print(energy(s))
  list(cut=state.to.cut(s), h=energy(s)) 
}
