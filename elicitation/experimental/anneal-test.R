N <- 1E4 
#m <- 20
#n <- 8
m <- 10
n <- 3
max.entropy <- -log2(1/m)

#perf <- t(sapply(1:m, function(x) {randomPointFromHypersphere(n)}))
perf <- hypersphere.sample(n, m)

## Construct the exact measurements (10000 samples)
meas <- array(dim=c(N, m, n))
for (i in 1:N) {
  meas[i,,] <- perf
}

constr <- simplexConstraints(n)
init <- har.init(constr)
w <- har.run(init, N)$samples

u <- smaa.values(meas, w)

entropy.calc <- function(w) {
    values <- smaa.values(meas=meas, pref=w)
    ranks <- smaa.ranks(values)
    ra <- smaa.ra(ranks)
    smaa.entropy.choice(ra)
}
#data <- simplex.calc.raster(entropy.calc, 0.02)

#criteria <- c("w1", "w2", "w3")
#plot.raster.polytope(data$data[,,1]/max.entropy, data$xrng, data$yrng, criteria=criteria, bounds=0.15)

#find.cut(meas, 1E3, NULL, equalWProbs=FALSE, minSize=0.02, ranking=FALSE)
#anneal.cut(w, u, init, max.entropy=max.entropy)

res.sample <- lapply(1:100, function(i) {
  print(i)
  res <- find.cut(meas, 1E3, NULL, equalWProbs=FALSE, minSize=0.02, ranking=FALSE)
  list(point=res$point, normal=res$normal, h=res$h)
})

res.anneal <- lapply(1:100, function(i) {
  print(i)
  res <- anneal.cut(w, u, init, max.entropy=max.entropy)
  list(point=res$cut$point, normal=res$cut$normal, h=res$h)
})

h.anneal <- sapply(res.anneal, function(x) { x$h })
h.sample <- sapply(res.sample, function(x) { x$h })
boxplot(list(h.anneal, h.sample))
