library(hitandrun)
library(MASS)
library(plyr)

EPS <- 1E-15

## Sample separating hyperplanes such that reference alternatives differ on
## at most k attributes
## @return a function(constr, N) which has inputs
##   constr: HAR linear constraint structure
##   N: the amount of planes to sample
##   and that returns a list of N planes (each with $point and $normal)
sample.planes.restricted.shakeandbake <- function(k) {
    function(constr, N) {
        n <- ncol(constr$constr)
        constr <- eliminateRedundant(mergeConstraints(simplexConstraints(n), constr))

        ## Sample the n-k attributes along which the alternatives are equal
        D <- sapply(1:N, function(i) { sample.int(n, n-k) })
        if (is.vector(D)) { # In case n-k=1
            D <- as.matrix(D)
        } else {
            D <- t(D)
        }
        alply(D, 1, sample.with.corner.indices, constr, .parallel=TRUE)
    }
}

sample.with.corner.indices <- function(corner.inds, constr, face.inds.func=face.indices) {
    ## hack around parallel backend bug
    source('lib/hyperplane.sample.shakeandbake.R')
    
    n <- ncol(constr$constr)
    k <- n - length(corner.inds)
    stopifnot(k <= n && k > 1) # sanity check
    num.faces <- nrow(constr$constr)-1
    num.corners <- length(corner.inds)
    num.pts.needed <- n - 1 - num.corners

    state <- har.init(constr)

    corners <- t(sapply(corner.inds, function(x) {
        arr <- array(0, dim=n)
        arr[x] <- 1
        arr
    }))
    
    transform.inv <- simplex.createTransform(n=n, inverse=TRUE, keepHomogeneous=FALSE)
    transform = simplex.createTransform(n=n, inverse=FALSE, keepHomogeneous=FALSE)

    re.sample <- TRUE
    while (re.sample) {
        result <- har.run(state=state, n.samples=num.pts.needed, boundary=TRUE)
        state <- result$state
        face.ind.sample <- face.inds.func(result$samples, constr)
        if (any(rowSums(face.ind.sample) == 0)) { # re-sample if any point not in a face
            re.sample <- TRUE
        } else {
            all.pts <- rbind(result$samples, corners)
            face.ind.all <- face.inds.func(all.pts, constr)
            ## re-sample if they're all in a single face
            re.sample <- any(colSums(face.ind.all) == nrow(face.ind.all))
        }
    }
    pts.minus.1d <- t(transform.inv %*% t(cbind(all.pts, 1)))
    list(point=as.matrix(all.pts[1,]),
         normal=transform %*% c(t(points.to.plane(pts.minus.1d)), 1))    
}

get.poss.ok.indices <- function(face.inds.corners, max.ind, n) {
    max.allowed <- n-3 # you can have n-3 corners in the same face, i.e. n-2 in the simplex-space
    counts <- sapply(1:max.ind, function(ind) {
        sum(face.inds.corners[,ind])
    })
    which(counts <= max.allowed, arr.ind=TRUE)
}

## Sample separating hyperplanes in unrestricted fashion
## by taking n points in different faces of the polytope.
## This is achieved by taking 2x the required samples to
## be sure we have sufficient amount of samples. It's not
## optimal but anyway the shake and bake sampling is not the
## bottleneck in our implementation.
##
## @return a function(constr, N) which has inputs
##   constr: HAR linear constraint structure
##   N: the amount of planes to sample
##   and that returns a list of N planes (each with $point and $normal)
sample.planes.unrestricted.shakeandbake <- function() {
    function(constr, N) {
        n <- ncol(constr$constr)
        constr <- eliminateRedundant(mergeConstraints(simplexConstraints(n), constr))
        num.faces <- nrow(constr$constr)-1

        n.faces <- n-1
        state <- har.init(constr)

        transform.inv <- simplex.createTransform(n=n, inverse=TRUE, keepHomogeneous=FALSE)
        transform = simplex.createTransform(n=n, inverse=FALSE, keepHomogeneous=FALSE)
        ## sample 2x the amount of points times amount
        ## needed for the cuts times the amount of constraints
        ## to have a chance of sampling a sufficient amount
        n.points.to.sample <- 2 * N * n.faces
        samples <- har.run(state=state, n.samples=n.points.to.sample, boundary=TRUE)
        inds <- face.indices(samples$samples, constr)
        stopifnot(nrow(inds) == nrow(samples$samples) && ncol(inds) >= 1) # sanity check
        ## Do the checks in n-1 dims
        samples.minus.1d <- t(transform.inv %*% t(cbind(samples$samples, 1)))
        partitions <- partition.samples(samples.minus.1d, inds, num.faces)
        
        partition.sizes <- sapply(partitions, nrow)
        sampled.face.inds <- replicate(N, sample(1:num.faces, n.faces, prob=partition.sizes))
        if (!is.matrix(sampled.face.inds)) {
            sampled.face.inds <- t(sampled.face.inds)
        }
        ## FIXME: should check first that there's enough samples in each partition
        planes.minus.1d <- sample.planes.from.partitions(partitions, sampled.face.inds)

        lapply(planes.minus.1d, function(p) {
            list(point = transform %*% c(p$point, 1),
                 normal = transform %*% c(p$normal, 1))
        })
    }
}

## Requires the face.inds to be a matrix with each column one sample of face indices
sample.planes.from.partitions <- function(partitions, face.inds, corners=NULL) {
    apply(face.inds, 2, function(inds) {
        points <- matrix(nrow=0, ncol=ncol(partitions[[max(inds)]]))
        for (j in inds) {
            part.j <- partitions[[j]]
            if (!is.matrix(part.j)) {
                part.j <- t(as.matrix(part.j))
            }
            if (nrow(part.j) < 1) {
                stop('partition with no points, cannot sample')
            }
            pt.ind <- sample(nrow(part.j), 1)
            pt <- part.j[pt.ind,]
            partitions[[j]] = part.j[-pt.ind,]
            stopifnot(is.vector(pt) && length(pt) > 0) # sanity check
            points <- rbind(points, pt)
        }
        if (!is.null(corners)) {
            points <- rbind(points, corners)
        }
        list(point=points[1,], normal=t(points.to.plane(points)))
    })
}

## Gives the normal of the plane with the given points
##
## pts: n x n matrix of points for the normal
## PRECOND: dim(pts)[1] == dim(pts)[2]
## @return normal of the plane span by the pts
points.to.plane <- function(pts) {
    stopifnot(dim(pts)[1] == dim(pts)[2]) # precond
    n.comps <- pts[-1,] - pts[1,]
    if (is.matrix(n.comps)) {
        n.comps <- t(n.comps)
    } else { ## 1 point m as matrix -> column vector. pfft sometimes I hate R
        n.comps <- as.matrix(n.comps)
    }
    Null(n.comps)
}

## Give indices of the faces of the samples
##
## samples: p x n matrix (p n-dim samples)
## faces: HAR linear constraint structure for the k faces
## PRECOND: ncol(samples) == ncol(faces$constr)
## @return a matrix with p rows containing face indices of the samples
face.indices <- function(samples, faces) {
    stopifnot(ncol(samples) == ncol(faces$constr)) # precond
    inds <- aaply(samples, 1, function(x) {
        ## sanity check - should be the constraint said below
        stopifnot(faces$constr[1,] == 1)
        ## first row is the x_1 + ... + x_n = 1, so drop that out
        vals <- sapply(2:nrow(faces$constr), function(y.ind) {
            x %*% faces$constr[y.ind,]
        })
        ## Samples can belong to more than 1 face for being very close to
        ## vertices
        vals  >= faces$rhs[-1] - EPS
    }, .drop=FALSE)
    as.matrix(inds)
}

partition.samples <- function(samples, inds, nmax) {
    stopifnot(nrow(samples) == nrow(inds)) # precond
    res <- list()
    for (i in 1:nmax) {
        mt <- samples[inds[,i]==TRUE,]
        if (length(mt) == 0) {
            mt <- matrix(nrow=0, ncol=0)
        } else if (!is.matrix(mt)) {
            mt <- t(as.matrix(mt))
        }
        res[[i]] <- mt
    }
    res
}
