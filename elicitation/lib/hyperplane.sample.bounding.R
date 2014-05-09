library(hitandrun)

# sample separating hyperplanes in unrestricted fashion
sample.planes.unrestricted.bounding <- function() {
  function(constr, N) {
    n <- ncol(constr$constr)
    state <- har.init(mergeConstraints(simplexConstraints(n), constr))
    state.to.cut <- function(s) {
      list(
        point = as.vector(state$transform %*% s$point),
        normal = as.vector(state$basis$basis %*% s$normal)
      )
    }
    lapply(
      hyperplane.sample(state$constr, N),
      state.to.cut)
  }
}

homogeneousCoordinateConstraint <- function(n) {
    list(constr=c(rep(0, n), 1), rhs=c(1), dir=c("="))
}

# "simple" attempt to get better hyperplanes
hyperplane.sample <- function(constr, N) {
  # potentially see Clarkson & Shor (1998) "Algorithms for diametral pairs and
  # convex hulls that are optimal, randomized, and incremental" for a better
  # way to calculate the diameter.

  homogeneous <- TRUE # below code is specific for homogeneous coordinates

  # approximate diameter by bounding box diameter
  bb <- createBoundBox(constr, homogeneous)
  diameter <- sqrt(sum((bb$ub-bb$lb)^2))
  n <- ncol(constr$constr) - homogeneous

  # prepare LP for length finding
  hom <- homogeneousCoordinateConstraint(n)
  h <- rcdd::makeH(constr$constr, constr$rhs, hom$constr, hom$rhs)
  lapply(1:N, function(i) {
    x <- NA
    while (all(is.na(x))) {
      # sample direction
      a <- as.vector(hypersphere.sample(n, 1))

      # find min and max translation
      tr.min <- t(c(a, 0)) %*% rcdd::lpcdd(h, c(a, 0), minimize=TRUE)$primal.solution
      tr.max <- t(c(a, 0)) %*% rcdd::lpcdd(h, c(a, 0), minimize=FALSE)$primal.solution

      # accept/reject based on length
      len <- (tr.max - tr.min)
      if (runif(n=1) < len/diameter) {
        d <- runif(n=1, min=tr.min, max=tr.max)
        x <- d * a
      }
    }
    list(point=c(x, 1), normal=a)
  })
}
