set.seed(1911)

source('code.R')
source('problem.R')
source('ratio-problem.R')

cuts1 <- dget('cuts1')
#cuts2 <- dget('cuts2')
#cuts3 <- dget('cuts3')

ra <- function(w) {
  smaa.ra(smaa.ranks(smaa.values(meas=meas, pref=w)))
}

entropy <- function(w) {
  smaa.entropy.choice(ra=ra(w))
}

cu <- function(h, h.min) {
  (h - h.min) / h
}

stopping.calc <- function(cut) {
  wn <-
    if (plane.side(rat.pt, cut$point, cut$normal)) {
      cut$w1
    } else {
      cut$w2
    }

  hn <- entropy(wn)
  i.min <- which.min(apply(wn, 1, entropy))
  i.max <- which.max(apply(wn, 1, entropy))
  list(h=hn,
     ra=ra(wn),
     w.min=wn[i.min,],
     h.min=entropy(wn[i.min,]),
     w.max=wn[i.max,],
     h.max=entropy(wn[i.max,]))
}

w0 <- simplex.sample(n=3, N=1E4)$samples
h0 <- entropy(w0)
h0.min <- min(apply(w0, 1, entropy))

cu(h0, h0.min)

stopping1 <- lapply(cuts1, stopping.calc)
#stopping3 <- lapply(cuts3, stopping.calc)

source('simplex.heat.R')
max.entropy <- 1
data <- simplex.calc.raster(entropy, 0.01)
criteria <- c("p", "d", "b")
plot.raster.polytope(data$data[,,1]/max.entropy, data$xrng, data$yrng, criteria=criteria, bounds=0.15)

polytope.verts <- function(cuts) {
  constr <- mergeConstraints(lapply(cuts, function(cut) { plane.constraint(cut$point, cut$normal, plane.side(rat.pt, cut$point, cut$normal)) }))

  transform <- simplex.createTransform(3)
  constr <- simplex.createConstraints(transform, constr)
  verts <- findVertices(constr, TRUE)
  centr <- apply(verts, 2, mean)
  ord <- order(apply(verts, 1, function(x) { atan2(x[1]-centr[1],x[2]-centr[2]) }))
  verts[ord,]
}

verts <- polytope.verts(cuts1)[,1:2]
polygon(verts)

w.point <- function(w) {
  (simplex.createTransform(3, inverse=TRUE) %*% c(w, 1))[1:2]
}

### START PLOTTING
pdf()

lapply(1:10, function(i) {
  plot.raster.polytope(pmin(data$data[,,1]/max.entropy*2, 1), data$xrng, data$yrng, criteria=criteria, bounds=0.15, main=paste("After Q", i))
  polygon(polytope.verts(cuts1[1:i]))
  points(t(w.point(rat.pt)), pch='*')
  points(t(w.point(stopping1[[i]]$w.max)), pch='o')
  points(t(w.point(stopping1[[i]]$w.min)), pch='x')
})

ra.est <- sapply(stopping1, function(s) { s$ra[2,1] })
ra.min <- sapply(stopping1, function(s) { ra(s$w.max)[2,1] })
ra.max <- sapply(stopping1, function(s) { ra(s$w.min)[2,1] })

plot(ra.est, ylim=c(0,1), type='l', lty=1, xlab="question", ylab="RAI(Enox, 1)")
lines(ra.min, lty=2)
lines(ra.max, lty=2)
abline(h=ra(rat.pt)[2,1], lty=3)

plot(sapply(stopping1, function(s) { s$h }), ylim=c(0,1), type='l', xlab="question", ylab="entropy")
lines(sapply(stopping1, function(s) { s$h.min }), lty=2)
lines(sapply(stopping1, function(s) { s$h.max }), lty=2)
abline(h=entropy(rat.pt), lty=3)

plot(sapply(stopping1, function(s) { cu(s$h, s$h.min) }), ylim=c(0,1), xlab="question", ylab="uncertainty coefficient")

dev.off()
### END PLOTTING



### What's wrong with question entropy??
h1 <- sapply(cuts1, function(cut) { entropy(cut$w1) })
h2 <- sapply(cuts1, function(cut) { entropy(cut$w2) })
p <- sapply(cuts1, function(cut) { cut$share / 100 })
qh <- p*h1 + (1-p)*h2
qh - sapply(cuts1, function(cut) { cut$h })


bla <- sapply(cuts1, function(cut) { cut$entropies[which.min(cut$entropies[,'h']),] })
h1 - bla['h1',]



### What would be the value of asking one of the "good" questions?

evaluate.cut.w <- function(constr, cut, w) {
  w1 <- harSample(mergeConstraints(constr, plane.constraint(cut$point, cut$normal, TRUE)), nrW)
  w2 <- harSample(mergeConstraints(constr, plane.constraint(cut$point, cut$normal, FALSE)), nrW)
  h1 <- entropy(w1)
  h2 <- entropy(w2)
  p <- sum(partition.points(w, cut$point, cut$normal))/nrow(w)
  qh <- p*h1 + (1-p)*h2
  c(qh=qh, h1=h1, h2=h2, p=p)
}

evaluate.cut <- function(constr, cut) {
  w <- harSample(constr, nrW)
  evaluate.cut.w(constr, cut, w)
}

## skip question 9?
constr <- mergeConstraints(lapply(cuts1[1:8], function(cut) { plane.constraint(cut$point, cut$normal, plane.side(rat.pt, cut$point, cut$normal)) }))
hyp9 <- evaluate.cut(constr, cuts1[[10]])
hyp9['qh'] < qh[9]
# TRUE

## question 7 directly after 3?
constr <- mergeConstraints(lapply(cuts1[1:3], function(cut) { plane.constraint(cut$point, cut$normal, plane.side(rat.pt, cut$point, cut$normal)) }))
hyp4 <- evaluate.cut(constr, cuts1[[7]])
hyp4['qh'] < qh[4]
# TRUE

## question 7 directly after 2?
constr <- mergeConstraints(lapply(cuts1[1:2], function(cut) { plane.constraint(cut$point, cut$normal, plane.side(rat.pt, cut$point, cut$normal)) }))
hyp3 <- evaluate.cut(constr, cuts1[[7]])
hyp3['qh'] < qh[3]
# FALSE

## question 2 immediately?
hyp1 <- evaluate.cut(NULL, cuts1[[2]])
hyp1['qh'] < qh[1]
# FALSE

cu(c(h0, sapply(stopping1, function(s) { s$h }))[1:10], qh)
cu(c(h0, sapply(stopping1, function(s) { s$h }))[1:10], pmin(h1,h2))


## experiment: better hyperplane sampling?
source('hyperplane-sample.R')

sample.planes.test <- function(constr, N) {
  state <- har.init(mergeConstraints(simplexConstraints(3), constr))
  state.to.cut <- function(s) {
    list(
      point = as.vector(state$transform %*% s$point),
      normal = as.vector(state$basis$basis %*% s$normal)
    )
  }
  lapply(
    hyperplane.sample(state$constr, N, homogeneous=TRUE),
    state.to.cut)
}

sample.planes.old <- function(constr, N) {
  W <- harSample(constr, N)
  sample.planes.unrestricted()(W)
}

# evaluate resulting cuts

constr <- mergeConstraints(lapply(cuts1[1:3], function(cut) { plane.constraint(cut$point, cut$normal, plane.side(rat.pt, cut$point, cut$normal)) }))

plot.planes <- function(constr, planes) {
  plot.raster.polytope(pmin(data$data[,,1]/max.entropy*2, 1), data$xrng, data$yrng, criteria=criteria, bounds=0.15)
  polygon(polytope.verts(cuts1[1:3]))
  lapply(planes, lines.cut)
}

testRuns <- function(constr, nrRuns, nrW, nrPlanes, planesFn) {
  lapply(1:nrRuns, function(i) {
    w <- harSample(constr, nrW)
    planes <- planesFn(constr, nrPlanes)
    eval <- sapply(planes, function(plane) { evaluate.cut.w(constr, plane, w) })
    list(planes=planes, entropy=eval)
  })
}

eval.old <- testRuns(constr, 20, 1E4, 1E4, sample.planes.old)
win.old <- sapply(eval.old, function(e) { which.min(e$entropy['qh',]) })
eval.new <- testRuns(constr, 20, 1E4, 1E4, sample.planes.test)
win.new <- sapply(eval.new, function(e) { which.min(e$entropy['qh',]) })

plane.old <- mapply(function(e, i) { e$planes[[i]] }, eval.old, win.old, SIMPLIFY=FALSE)
plane.new <- mapply(function(e, i) { e$planes[[i]] }, eval.new, win.new, SIMPLIFY=FALSE)

entr.old <- mapply(function(e, i) { e$entropy[,i] }, eval.old, win.old)
entr.new <- mapply(function(e, i) { e$entropy[,i] }, eval.new, win.new)
