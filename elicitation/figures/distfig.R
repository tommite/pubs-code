args <- commandArgs(trailingOnly=TRUE)
pdf(args[1], height=7/3)

par(mfrow=c(1,3), pty='s')

xvals <- seq(0, 1, length=1000)

proxHep <- dbeta(xvals, shape1=20, shape2=116)
proxEno <- dbeta(xvals, shape1=8, shape2=121)
distHep <- dbeta(xvals, shape1=40, shape2=96)
distEno <- dbeta(xvals, shape1=32, shape2=97)
bleedHep <- dbeta(xvals, shape1=1, shape2=135)
bleedEno <- dbeta(xvals, shape1=5, shape2=124)

plot(xvals, proxHep, type='l', lty=1, ylim=c(0, max(c(proxHep, proxEno))),
     xlab = 'p(Proximal DVT)', ylab = 'PDF')
points(xvals, proxEno, type='l', lty=2)

plot(xvals, distHep, type='l', lty=1, ylim=c(0, max(c(distHep, distEno))),
     xlab = 'p(Distal DVT)', ylab = '')
points(xvals, distEno, type='l', lty=2)

plot(xvals, bleedHep, type='l', lty=1, ylim=c(0, max(c(bleedHep, bleedEno))),
     xlab = 'p(Major bleeds)', ylab = '')
points(xvals, bleedEno, type='l', lty=2)

dev.off()
