args <- commandArgs(trailingOnly=TRUE)

## shape parameters
a <- c(12:15,30,40,40,40,35)
b <- c(rep(35, 4),40,40,30,15,10)

x <- seq(0, 1, by=0.01)

pdf(args[1])
plot(NA, xlim = c(0, 1), ylim = c(0, 8), xlab="partial value", ylab="density")
for (i in 1:length(a)) { lines(x, dbeta(x, a[i], b[i])) }
dev.off()
