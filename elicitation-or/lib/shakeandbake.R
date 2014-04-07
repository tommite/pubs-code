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
        n.faces <- n-1
        state <- har.init(constr)

        transform.inv <- simplex.createTransform(n=n, inverse=TRUE, keepHomogeneous=FALSE)
        transform = simplex.createTransform(n=n, inverse=FALSE, keepHomogeneous=FALSE)
        ## sample 2x the amount of points times amount
        ## needed for the cuts times the amount of constraints
        ## to have a chance of sampling a sufficient amount
        n.points.to.sample <- 2 * N * n.faces
        samples <- har.run(state=state, n.samples=n.points.to.sample, boundary=T)
        inds <- face.indices(samples$samples, constr)
        ## Do the checks in n-1 dims
        samples.minus.1d <- t(transform.inv %*% t(cbind(samples$samples, 1)))
        partitions <- partition.samples(samples.minus.1d, inds)
        
        all.face.inds <- sort(unique(inds))
        partition.sizes <- sapply(partitions, nrow)
        sampled.face.inds <- replicate(N, sample(all.face.inds, n.faces,
                                                 prob=partition.sizes[partition.sizes > 0]))
        ## FIXME: should check first that there's enough samples in each partition
        planes.minus.1d <- sample.planes.from.partitions(partitions, sampled.face.inds)

        lapply(planes.minus.1d, function(p) {
            list(point = transform %*% c(p$point, 1),
                 normal = transform %*% c(p$normal, 1))
        })
    }
}

sample.planes.from.partitions <- function(partitions, face.inds) {
    if (!is.matrix(face.inds)) {
        face.inds <- as.matrix(face.inds)
    }

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
    Null(n.comps)
}

## Give indices of the faces of the samples
##
## samples: p x n matrix (p n-dim samples)
## faces: HAR linear constraint structure for the k faces
## PRECOND: ncol(samples) == ncol(faces$constr)
## @return a vector of length p containing face indices of the samples
face.indices <- function(samples, faces) {
    stopifnot(ncol(samples) == ncol(faces$constr)) # precond
    apply(samples, 1, function(x) {
        ## sanity check - should be the constraint said below
        stopifnot(faces$constr[1,] == 1)
        ## first row is the x_1 + ... + x_n = 1, so drop that out
        vals <- sapply(2:nrow(faces$constr), function(y.ind) {
            x %*% faces$constr[y.ind,]
        })
        res <- which(vals  >= faces$rhs[-1] - EPS)
        ## should have only one binding constraint - if we have more,
        ## then it's due to numerical inaccuracy and just assign it
        ## to the one it is the closest to
        if (length(res) != 1) {
            res <- which.min(abs(vals - faces$rhs[-1]))
        }
        stopifnot(length(res) == 1)
        res
    })
}

partition.samples <- function(samples, inds) {
    res <- list()
    for (i in 1:max(inds)) {
        mt <- samples[which(inds == i),]
        if (length(mt) == 0) {
            mt <- matrix(nrow=0, ncol=0)
        } else if (!is.matrix(mt)) {
            mt <- t(as.matrix(mt))
        }
        res[[i]] <- mt
    }
    res
}
