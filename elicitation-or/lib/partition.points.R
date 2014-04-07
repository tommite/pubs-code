if (!is.loaded("partitionPointsForR")) dyn.load("lib/elicitation.so")

partition.points <- function(points, x0, normal) {
  stopifnot(is.matrix(points))
  N <- nrow(points)
  n <- ncol(points)
  stopifnot(length(x0) == n)
  stopifnot(length(normal) == n)

  .C("partitionPointsForR",
     as.integer(N),
     as.integer(n),
     points,
     x0,
     normal,
     partition=array(0.0, dim=N),
     NAOK=FALSE, DUP=FALSE, PACKAGE="elicitation"
     )$partition > 0
}
