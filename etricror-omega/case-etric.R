library(ROI.plugin.glpk)
library(ROI)
solvers <- ROI_installed_solvers()
if (!is.na(solvers['symphony'])) {
  .solver <<- 'symphony'
} else if (!is.na(solvers['glpk'])) {
  .solver <<- 'glpk'
} else {
  stop("No ROI Symphony or GLPK plugin installed")
}
source('etricror.R')

make.test <- function(revised) {
    if (revised) {
        file.ext <- 'revised'
    } else {
        file.ext <- 'original'
    }
    perfs <- read.table(file="alts.csv", sep=",", header=TRUE)
    rownames(perfs) = perfs[,1]
    perfs = perfs[,2:6]

    profs <- read.table(file="profs.csv", sep=",", header=FALSE)
    rownames(profs) = profs[,1]
    profs = profs[,2:6]
    colnames(profs) <- colnames(perfs)

    thresholds <- matrix(c(
        0, 0.01, 0, 0.02, FALSE,
        0, 0, 1.9, 0, FALSE,
        0, 0, 1.9, 0, FALSE,
        0, 0, 1.9, 0, FALSE,
        0, 0, 2, 0, FALSE),ncol=5, byrow=TRUE)
    
    ## 1st iteration
    assigs1 <- matrix(c(
        1, 1,
        8, 2,
        13, 2,
        16, 3,
        40, 4)
                      , ncol=2, byrow=TRUE)
    
    message("--- starting tests, iteration 1")
    pos1 <- etricror(perfs, profs, assigs1, FALSE, th=thresholds)
    nec1 <- etricror(perfs, profs, assigs1, TRUE, th=thresholds)
    write.csv(nec1$relation,
              file=paste(file.ext, "-necessary-relation-1.csv", sep=''))
    write.csv(pos1$relation,
              file=paste(file.ext, "-possible-relation-1.csv", sep=''))
    
    ## 2nd iteration
    assigs2 <- matrix(c(
        1, 1,
        8, 2,
        13, 2,
        16, 3,
        18, 3,
        31, 4,
        35, 4,
        40, 4)
                      , ncol=2, byrow=TRUE)
    
    
    message("--- starting tests, iteration 2")
    pos2 <- etricror(perfs, profs, assigs2, FALSE, th=thresholds)
    nec2 <- etricror(perfs, profs, assigs2, TRUE, th=thresholds)
    write.csv(nec2$relation,
              file=paste(file.ext, "-necessary-relation-2.csv", sep=''))
    write.csv(pos2$relation,
              file=paste(file.ext, "-possible-relation-2.csv", sep=''))
    
    ## 3rd iteration
    assigs3 <- matrix(c(
        1, 1,
        8, 2,
        12, 2,
        13, 2,
        16, 3,
        18, 3,
        25, 3,
        31, 4,
        34, 4,
        35, 4,
        40, 4)
                      , ncol=2, byrow=TRUE)
    
    
    message("--- starting tests, iteration 3")
    pos3 <- etricror(perfs, profs, assigs3, FALSE, th=thresholds)
    nec3 <- etricror(perfs, profs, assigs3, TRUE, th=thresholds)
    write.csv(nec3$relation,
              file=paste(file.ext, "-necessary-relation-3.csv", sep=''))
    write.csv(pos3$relation,
              file=paste(file.ext, "-possible-relation-3.csv", sep=''))
    
    ## find lambda range
    minl = optimizeLambda(perfs, profs, assigs1, thresholds, FALSE)
    maxl = optimizeLambda(perfs, profs, assigs1, thresholds, TRUE)
    message("lambda range ", minl, " ", maxl)
    
    vars = pos1$solution[c(1:5, getLambdaIndex(nrow(perfs), ncol(perfs),
        nrow(profs)))]
    vars = rbind(vars, pos2$solution[c(1:5, getLambdaIndex(nrow(perfs), ncol(perfs), nrow(profs)))])
    vars = rbind(vars, pos3$solution[c(1:5, getLambdaIndex(nrow(perfs), ncol(perfs), nrow(profs)))])
    rownames(vars) <- c("it1", "it2", "it3")
    colnames(vars) <- c(colnames(perfs), "lambda")
    
    write.csv(vars, file=paste(file.ext, "-solution-variables.csv", sep=''))
    
    ## Plots
    pdf(paste(file.ext, '-varplot.pdf', sep=''))
    matplot(vars, type='b', lty=1:6, pch=1, col=1, xaxt="n",
            xlab='Iteration', ylab='', ylim=c(0.0, 0.8), yaxt="n")
    axis(side=1, at=c(1, 2, 3), label=c("1", "2", "3"))
    axis(side=2, at=seq(0.0, 0.8, 0.1))
    cnames <- c(expression('w'[1]), expression('w'[2]), expression('w'[3]),
                expression('w'[4]), expression('w'[5]), expression(lambda))
    legend(x=2.7, y=0.65, lty=1:6, cnames)
    dev.off()
}

make.test(TRUE) # with the revised procedure
make.test(FALSE) # with the original ETRI-C procedure
