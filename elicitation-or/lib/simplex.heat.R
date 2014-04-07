simplex.calc.raster <- function(values.fn, resolution) {
    transform.inv <- simplex.createTransform(n=3, inverse=TRUE, keepHomogeneous=FALSE)
    v1 <- transform.inv %*% c(1,0,0,1)
    v2 <- transform.inv %*% c(0,1,0,1)
    v3 <- transform.inv %*% c(0,0,1,1)
    vs <- cbind(v1,v2,v3)
    x.min <- min(vs[1,])
    x.max <- max(vs[1,])
    y.min <- min(vs[2,])
    y.max <- max(vs[2,])
    
    transform <- simplex.createTransform(3)
    constr <- simplex.createConstraints(transform=transform)

    ncomps <- length(values.fn(as.numeric(c(0, 0, 1))))

    x <- seq(from=x.min, to=x.max, by=resolution)
    y <- seq(from=y.max, to=y.min, by=-resolution)
    m <- array(NA, dim=c(length(y),ncol=length(x),ncomps))
    
    for (i in 1:nrow(m)) {
        for (j in 1:ncol(m)) {
            v <- c(x[j], y[i], 1)
            hit <- all(constr$constr %*% as.numeric(v) <= 0)
            if (hit) {
                w <- as.vector(transform %*% as.numeric(v))		
                m[i, j,] <- values.fn(w)
            }
        }
    }
    list(data=m, xrng=c(x.min, x.max), yrng=c(y.min, y.max))
}

plot.raster.polytope <- function(data, xrng, yrng, criteria, point=NULL, bounds=0.15, textcex=1.5, ...) {
    raster <- as.raster((data * 0.8) + 0.2)
    plot(NA, NA, type='n', xlim=c(xrng[1]-bounds,xrng[2]+bounds) , ylim=c(yrng[1]-bounds, yrng[2]+bounds), xlab='', ylab='', ...)
    rasterImage(raster, xleft=xrng[1], xright=xrng[2], ybottom=yrng[1], ytop=yrng[2])
    vertices <- t(matrix(c(-0.83, 0.4, 0, -0.87, 0.83, 0.4), byrow=T, ncol=2))
    lines(c(xrng[1], xrng[2]), c(yrng[2], yrng[2]), col='black')
    lines(c(xrng[1], mean(xrng)), c(yrng[2], yrng[1]), col='black')
    lines(c(mean(xrng), xrng[2]), c(yrng[1], yrng[2]), col='black')
    if (!is.null(point)) points(point[1], point[2], pch=20)
    text(vertices[1,], vertices[2,], labels=criteria, cex=textcex)
}

lines.cut <- function(cut,...) {
        transform.inv <- simplex.createTransform(n=3, inverse=TRUE, keepHomogeneous=FALSE)
	point <- transform.inv %*% c(cut$point, 1)
	dir <- rbind(c(0,-1), c(1, 0)) %*% transform.inv %*% c(cut$normal, 1)

	p0 <- point - dir
	p1 <- point + dir

	lines(c(p0[1], p1[1]), c(p0[2], p1[2]), ...)
}
