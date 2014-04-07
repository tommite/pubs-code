## Make a HAR constraint based on point in plane and a normal
## left: TRUE/FALSE
plane.constraint <- function(point, dir, left) {
    stopifnot(dim(point) == dim(dir))
    if (left) {
        dir <- -dir
    }
    list(constr=t(dir), dir="<=", rhs=t(dir) %*% point)
}

plane.getintercept <- function(x0, normal) {
    stopifnot(dim(x0) == dim(normal))
  -t(normal) %*% x0
}

plane.side <- function(point, planepoint, planedir) {
  c(point, 1) %*% c(planedir, plane.getintercept(planepoint, planedir)) > 0
}
