library(R.basic)
library(plyr) # necessary because of retarded apply() behaviors
library(hitandrun)
source('lib/plane.R')
source('lib/code.R')

isVertex <- function(x) {
  isTRUE(all.equal(sort(x, decreasing=TRUE)[1:2], c(1, 0)))
}

lineSegment <- function(p1, p2) {
	lines3d(c(p1[1], p2[1]), c(p1[2], p2[2]), c(p1[3], p2[3]), lty=2)
}

simplex.intercept <- function(x, n, p0, pn, simplexSize=1.0) {
    p0 <- t(p0)
    pn <- t(pn)
  ## form the points of the simplex
  c1 <- c2 <- rep(0, n)
  c1[x[1]] <- simplexSize
  c2[x[2]] <- simplexSize
  
  nom <- (p0 - c1) %*% t(pn)
  den <- (c2 - c1) %*% t(pn)

  if (den == 0) {
    NULL
  } else {
    d <- as.numeric(nom / den)
    loc <- d * (c2 - c1) + c1
    # deal with numerical accuracy on the vertices
    if (isTRUE(all.equal(as.numeric(d), 1))) d <- 1
    if (isTRUE(all.equal(as.numeric(d), 0))) d <- 0
    if (d < 0 || d > 1) {
      NULL
    } else {
      loc
    }
  }
}

## p0 = point in plane
## pn = plane normal
plane.simplex.intercept <- function(p0, pn) {
  stopifnot(dim(p0) == dim(pn))

  n <- length(p0)
  pairs <-  combn(seq(1:n), 2)

  ints <- alply(pairs, 2, simplex.intercept, n, p0, pn, 1.0)

  res <- matrix(NA, nrow=0, ncol=length(p0))
  haveVertex <- FALSE # Vertices will appear more than once, which could screw us up.
  for (i in ints) {
    if (!is.null(i)) {
      if (!isVertex(i) || !haveVertex) {
          res <- rbind(res, i)
      }
      if (isVertex(i)) {
        haveVertex <- TRUE
      }
    }
  }
  res
}

# Calculate the polytope bounded by the given cuts.
# @arg cuts: the cutting planes (with $point and $normal)
# @arg side: the side on each cut (TRUE for left)
# @return an ordered list of vertices
polytope.verts <- function(cuts, side, invertTransform=TRUE) {
  constr <- mergeConstraints(lapply(1:length(cuts), function(i) { plane.constraint(cuts[[i]]$point, cuts[[i]]$normal, side[i]) }))

  transform <- simplex.createTransform(3)
  constr <- simplex.createConstraints(transform, constr)
  verts <- findVertices(constr, TRUE)
  centr <- apply(verts, 2, mean)
  ord <- order(apply(verts, 1, function(x) { atan2(x[1]-centr[1],x[2]-centr[2]) }))
  verts <- verts[ord,]
  if (invertTransform) {
    verts <- verts %*% t(transform)
  }
  verts
}

plot.cuts <- function(cuts, w.exact) {
  normalize <- function(x) {
    x/sqrt(sum(x*x))
  }

  plot3d(c(0,0,1),c(0,1,0),c(1,0,0), theta=115, phi=20, xlab="w[p]", ylab="w[d]", zlab="w[b]", box=TRUE, axes=TRUE)
  polygon3d(c(0,0,1),c(0,1,0),c(1,0,0))

  if (!is.null(w.exact)) {
    # draw each side of the last cut
    side <- sapply(cuts, function(cut) { plane.side(w.exact, cut$point, cut$normal) })
    poly1 <- polytope.verts(cuts, side)
    polygon3d(poly1[,1], poly1[,2], poly1[,3], col=gray(0.6), border=NA)
    side[length(side)] <- !side[length(side)]
    poly2 <- polytope.verts(cuts, side)
    polygon3d(poly2[,1], poly2[,2], poly2[,3], col=gray(0.8), border=NA)

    # draw the "actual" preferences
    points3d(x=w.exact[1], y=w.exact[2], z=w.exact[3], col='black', pch=20) # plot "exact" weight
  }

  index <- 1 
  for (r in cuts) {
    lineInt <- plane.simplex.intercept(r$point, r$normal)
    locText <- if (isVertex(lineInt[1,])) {
      lineInt[2,] - 0.08 * normalize(lineInt[1,] - lineInt[2,])
    } else {
      lineInt[1,] - 0.08 * normalize(lineInt[2,] - lineInt[1,])
    }
    lineSegment(lineInt[1,], lineInt[2,])
    text3d(x = locText[1], y=locText[2], z=locText[3], label = index, cex=0.5)
    index <- index + 1
  }          
}

## Computes uncertainty coefficient
comp.r <- function(test) {
    (test$stopping$h - mean(test$stopping$all.ent)) / test$stopping$h
}

plot.ent.ra <- function(cuts, ra, do.legend=T) {
  ents <- sapply(cuts, function(x) {x$h})
  ent.min <- sapply(cuts, function(x) {x$stopping$h.min})
  ent.max <- sapply(cuts, function(x) {x$stopping$h.max})
  ras <- sapply(cuts, function(x) {x$ra[2,1]})
  rs <- laply(cuts, comp.r)
  yrng <- c(0, 1)
  plot(NA, xlab='Question', ylab='', xlim=c(1, length(cuts)), ylim=yrng, xaxt='n')
  axis(1, at=1:10, labels=0:9)
  polygon(c(1:length(cuts), length(cuts):1), c(ent.min, rev(ent.max)), col='gray', border=NA)
  lines(ents, type='l', lty=1)
  lines(rs, type='l', lty=2)
  lines(ras, type='l', lty=4)
  abline(ra, 0, lty=3)
  if (do.legend) {
      legend(x=c(7, 10), y=c(0.6, 0.8), c('H(Y|A(Q)', 'R before Q', 'min/max H before Q', 'RAI(Enox, 1) before Q', 'Real RAI(Enox, 1)'), lty=c(1, 2, 5, 4, 3), cex=0.4)
  }
}

plot.all <- function(cuts, w.exact, ra, nr.cuts=5) {
  par(mfrow=c(1,2))
  plot.ent.ra(cuts, ra)
  plot.cuts(cuts[1:nr.cuts], w.exact)
}
