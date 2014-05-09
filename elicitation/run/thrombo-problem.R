library(hitandrun)
library(smaa)

args <- commandArgs(trailingOnly=TRUE)

set.seed(as.integer(args[2]))

### Generate measurement samples from beta distributions for the thrombolytics example
n <- 3
m <- 2
nrW <- 10000
nrX <- nrW

## "Benefit": proximal DVT risk (p)
## "Benefit": distal DVT risk (d)
## "Risk": major bleeding risk (b)

# Heparin
m.p0 <- rbeta(n=nrX,20,116)
m.d0 <- rbeta(n=nrX,40,96)
m.b0 <- rbeta(n=nrX,1,135)

# Enoxaparin
m.p1 <- rbeta(n=nrX,8,121)
m.d1 <- rbeta(n=nrX,32,97)
m.b1 <- rbeta(n=nrX,5,124)

# Measurement scales (worst value first)
s.p <- c(0.25, 0.0)
s.d <- c(0.4, 0.15)
s.b <- c(0.1, 0.0)

# Partial value functions
partialValue <- function(worst, best) { 
    if (best > worst) {
        function(x) {
            (x - worst) / (best - worst)
        }
    } else {
        function(x) {
            (worst - x) / (worst - best)
        }
    }
}
u.p <- partialValue(s.p[1], s.p[2])
u.d <- partialValue(s.d[1], s.d[2])
u.b <- partialValue(s.b[1], s.b[2])

meas <- array(c(
    u.p(m.p0), u.p(m.p1),
    u.d(m.d0), u.d(m.d1),
    u.b(m.b0), u.b(m.b1)), dim=c(nrX,m,n))

dimnames(meas) <- list(NULL, c("Heparin", "Enoxaparin"), c("Prox DVT", "Dist DVT", "Bleed"))

### Analyze the example using previously obtained ratio bound preferences
### and calculate (fictional) exact preferences

pbWorstX <- 0.13
pbBestX <- 0.11
# w_p u_p(x) = w_b * 1
# w_p / w_b = 1 / u_p(x)
pbLow <- 1 / u.p(pbBestX)
pbHigh <- 1 / u.p(pbWorstX)

bdWorstX <- 0.05
bdBestX <- 0.04
# w_b u_b(x) = w_d * 1
# w_b / w_d = 1 / u_b(x)
bdLow <- 1 / u.b(bdBestX)
bdHigh <- 1 / u.b(bdWorstX)

# wp(1) > wb(3) > wd(2)
userConstr <- mergeConstraints(
	lowerRatioConstraint(n, 1, 3, pbLow), # w_p / w_b
	upperRatioConstraint(n, 1, 3, pbHigh),
	lowerRatioConstraint(n, 3, 2, bdLow), # w_b / w_d
	upperRatioConstraint(n, 3, 2, bdHigh)
)

# use HAR to sample weights
transform <- simplex.createTransform(n)
constr <- simplex.createConstraints(transform, userConstr)
seedPoint <- createSeedPoint(constr, homogeneous=TRUE)
pref <- har(seedPoint, constr, 3*nrW, 3, homogeneous=TRUE, transform=transform)$samples

smaa.interval <- smaa(meas, pref)

## w's = p, b, d
pb.rat <- mean(pbLow, pbHigh)
bd.rat <- mean(bdLow, bdHigh)

## w1 / w3 = pb.rat
## w3 / w2 = bd.rat
## => w1 = w3 * pb.rat, w3 = w2 * bd.rat,
## w1 + w2 + w3 = 1
rat.a <- matrix(c(1, 0, -pb.rat,
                  0, -bd.rat, 1,
                  1, 1, 1), byrow=TRUE, ncol=3)
rat.b <- c(0, 0, 1)
w.exact <- qr.solve(a=rat.a, b=rat.b) ## average ratio statement point

smaa.exact <- smaa(meas, w.exact)

### write the output
saveRDS(list(
  meas=meas,
  constr.interval=userConstr,
  smaa.interval=smaa.interval,
  w.exact=w.exact,
  smaa.exact=smaa.exact), args[1])
