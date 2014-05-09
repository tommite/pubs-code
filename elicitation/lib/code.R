library(doParallel)
library(smaa)
library(hitandrun)
library(parallel)
source('lib/partition.points.R')
source('lib/plane.R')
source('lib/hyperplane.sample.bounding.R')
source('lib/hyperplane.sample.shakeandbake.R')

comp.uc <- function(nalts, stopping) {
    norm <- log2(nalts) # bits of information, choice problem
    (stopping$h - stopping$h.min) / norm
}

## Sample N points in n-dimensions, within simplex with additional constraints
harSample <- function(constr, N) {
  n <- ncol(constr$constr)
  transform <- simplex.createTransform(n)
  constr <- simplex.createConstraints(transform, constr)
  seedPoint <- createSeedPoint(constr, homogeneous=TRUE)

  # always generate at least 10^4*(n-1)^3 samples to get uniformity
  M <- max(1E4, N) * (n-1)^3
  thin <- floor(M / N)
  samples <- har(seedPoint, constr, N=M, thin=thin, homogeneous=TRUE, transform=transform)$samples
  stopifnot(nrow(samples) == N) # sanity check
  samples
}

find.cut <- function(meas, nrPlanes, prevCuts, equalWProbs=TRUE, ranking=FALSE, sample.planes, cluster.size=detectCores()) {
  N <- dim(meas)[1]
  n <- dim(meas)[3]

  cl <- makeCluster(cluster.size)
  registerDoParallel(cl)
    
  entropy <- if (ranking) smaa.entropy.ranking else smaa.entropy.choice
  
  ## Sample planes
  if (is.null(prevCuts)) {
    prevCuts <- list(constr=matrix(NA, nrow=0, ncol=n), dir=rep("<=", 0), rhs=rep(0, 0))
  }
  sampling.t <- system.time(
      planes <- sample.planes(prevCuts, nrPlanes)
      )[3]
  message("- planes sampled in ", sampling.t, "s")

  plane.find.t <- system.time(
      best <- best.cutting.plane(prevCuts, meas, planes, equalWProbs=equalWProbs, entropy=entropy)
      )[3]
  message("- best plane found in ", plane.find.t, "s")
  cutPt <- planes[[best$choice]]$point
  cutDir <- planes[[best$choice]]$normal

  pts <- harSample(prevCuts, N)
  stopping.t <- system.time(
      stopping <- stopping.calc(meas, pts, entropy)
      )[3]
  message("- stopping criterion computed in ", stopping.t, "s")
  partition <- partition.points(pts, cutPt, cutDir)
  values <- smaa.values(meas, pts)
  ranks <- smaa.ranks(values)

  stopCluster(cl)

  list(entropies=best$entropies, point=cutPt, normal=cutDir,
       share=sum(partition) / N, h=best$entropies[best$choice, "h"],
       ra=smaa.ra(ranks), stopping=stopping)
}

# meas: measurements
# wn: weights
# entropy.fn: entropy calculation function
stopping.calc <- function(meas, wn, entropy.fn) {
  entropy <- function(w) {
    ra <- smaa.ra(smaa.ranks(smaa.values(meas=meas, pref=w)))
    entropy.fn(ra)
  }

  hn <- entropy(wn)
  all.ent <- aaply(wn, 1, entropy, .parallel=TRUE)
  i.min <- which.min(all.ent)
  i.max <- which.max(all.ent)
  list(h=hn,
       w.min=wn[i.min,],
       h.min=all.ent[i.min],
       w.max=wn[i.max,],
       h.max=all.ent[i.max],
       all.ent=all.ent)
}

get.cuts <- function(tgt, nr, meas, nrPlanes, equal.w.prob, ranking=FALSE, sample.planes=sample.planes.unrestricted()) {
  stopifnot(nr >= 1)
  n <- dim(meas)[3]
  constr <- list(constr=matrix(NA, nrow=0, ncol=n), dir=rep("<=", 0), rhs=rep(0, 0))

  ret <- list()

  for (i in 1:nr) {
    res <- find.cut(meas, nrPlanes, constr, equal.w.prob, ranking=ranking, sample.planes=sample.planes)
    message(i, ". Cut: ", res$share * 100, "% - h: ", sprintf("%.2f", res$h))
    ret[[i]] <- res
    old.nr <- nrow(constr$constr)
    constr <- eliminateRedundant(mergeConstraints(constr,
                               plane.constraint(res$point,
                                                res$normal,
                                                plane.side(tgt, res$point, res$normal))))
    message("- eliminated ", old.nr - nrow(constr$constr) + 1, " redundant constraints")
  }
  ret
}

# select columns of a matrix, ALWAYS returning a matrix
select.col <- function(m, i) {
  m[, i, drop=FALSE]
	matrix(m[, i], nrow=nrow(m))
}

# select rows of a matrix, ALWAYS returning a matrix
select.row <- function(m, i) {
  mi[i, , drop=FALSE]
}

question.entropy.pairwise <- function(w, w1, w2, cut, meas, entropy=smaa.entropy.choice, equalWProbs=FALSE) {
  sel <- partition.points(w, cut$point, cut$normal)
  p1 <- sum(sel) / nrow(w)
  p2 <- 1 - p1

  if (equalWProbs) {
      p1 <- 0.5
      p2 <- 0.5
  }
  v1 <- smaa.values(meas, w1)
  v2 <- smaa.values(meas, w2)
  r1 <- smaa.ranks(v1)
  r2 <- smaa.ranks(v2)

  h1 <- entropy(r1)
  h2 <- entropy(r2)
  
  c('h1'=h1, 'h2'=h2, 'h'=p1 * h1 + p2 * h2)
}

## Choose the best cutting plane (in terms of entropy).
# constr: constraints defining W'
# meas: measurements for the alternatives (matrix)
# cuts: a list, where cuts[[i]]$point and cuts[[i]]$normal give a point and
# normal vector defining a hyperplane
# ranking: whether to compute ranking (TRUE) or choice (FALSE) entropy
# equalWProbs: whether to use equal probabilities for both sides of the cut (TRUE) or to have the probabilities reflecting sizes of the half-spaces (FALSE)
# Return value: the (index of the) best hyperplane
best.cutting.plane <- function(constr, meas, cuts, entropy=smaa.entropy.choice, equalWProbs=TRUE) {
    nrW <- dim(meas)[1]
    n <- dim(meas)[3]

    w <- harSample(constr, nrW) # sample weights to use for estimating the sizes of p(W'')

    hs <- laply(cuts, function(cut) {
        source('lib/code.R') # workaround bug in parallel packend
        w1 <- harSample(mergeConstraints(constr, plane.constraint(cut$point, cut$normal, TRUE)), nrW)
        w2 <- harSample(mergeConstraints(constr, plane.constraint(cut$point, cut$normal, FALSE)), nrW)
        question.entropy.pairwise(w, w1, w2, cut, meas, entropy=entropy, equalWProbs=equalWProbs)
    }, .parallel=TRUE)
    
    colnames(hs) <- c("h1", "h2", "h")
    list(choice=which.min(hs[,"h"]), entropies=hs)
}
