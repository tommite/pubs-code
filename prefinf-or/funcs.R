library(ror)
library(intervals)

## no information: -2
## does not hold (by Lemma 1): -1
## by preference statement: 1
## by transitivity: 2
## by theorem 1: 3

randomPointFromHypersphere <- function(ncrit) {
  rns <- c()
  while(TRUE) {
    rns <- rnorm(ncrit)
    if (all(rns > 0)) {
      break
    }
  }
  mul <- 1 / sqrt(sum(rns * rns))
  return(rns * mul)
}

checkSingleStatementNecessary <- function(aind, bind, performances, preferences) {
  a <- performances[aind,]
  b <- performances[bind,]

  for (p in 1:nrow(preferences)) {
    xind <- preferences[p,1]
    yind <- preferences[p,2]
    if (aind != xind && aind != yind && bind != xind && bind != yind) {
      x <- performances[xind,]
      y <- performances[yind,]

      holds = TRUE
      
      for (i in 1:ncol(performances)) { # loop over all criteria
        if (x[i] > y[i] && b[i] > a[i]) { # Lemma 1
          holds = FALSE
        }
        else if (x[i] > y[i] && a[i] >= b[i]) { # condition (i)
          if (a[i] < x[i] || y[i] < b[i]) {
            holds = FALSE
          }
        }
        else if (x[i] <= y[i] && a[i] < b[i]) { # condition (ii)
          if (a[i] < x[i] || y[i] < b[i]) {
            holds = FALSE
          }
        } else if (a[i] < b[i]) { # condition (iii)
          holds = FALSE
        }
      } # end for

      if (holds) {
        return(3)
      }
    }
  }
  
  return(-1)
}

addTransitiveStatements <- function(relation, a, b) {
  ## Forward transitivity
  possibleAdds <- which(relation[b,] > 0)
  for (i in possibleAdds) {
    if (relation[a,i] < 1) {
      relation[a,i] <- 2
      relation <- addTransitiveStatements(relation, a, i)
    }
  }
  ## Backwards transitivity
  possibleAddsRev <- which(relation[,a] == 1)
  for (i in possibleAddsRev) {
    if (relation[i,b] < 1) {
      relation[i,b] <- 2
      relation <- addTransitiveStatements(relation, i, b)
    }
  }  
  return(relation)
}

## Checks lemma 1 but for a set of preference statements - it still needs to be re-written in the paper
checkLemma1 <- function(nec, performances, preferences) {
  nalts <- nrow(performances)
  ncrit <- ncol(performances)
  piMinus <- list()
  
  for (crit in 1:ncrit) {
    piMinus[[crit]] <- Intervals_full()
  }
  for (row in 1:nrow(preferences)) {
    aind <- preferences[row,1]
    bind <- preferences[row,2]

    stopifnot (aind != bind) # sanity check

    a <- performances[aind,]
    b <- performances[bind,]
    for (crit in 1:ncrit) {
      if (b[crit] > a[crit]) {
        ival <- Intervals_full(matrix(c(a[crit], b[crit]), byrow=TRUE, ncol=2), closed=c(TRUE, TRUE))
        piMinus[[crit]] <- interval_union(piMinus[[crit]], ival)
      }
    }
  }

  for (i in 1:(nalts-1)) {
    for (j in (i+1):nalts) {
      if (nec[i,j] == -2 || nec[j,i] == -2) {
        a <- performances[i,]
        b <- performances[j,]
        for (crit in 1:ncrit) {
          if (a[crit] != b[crit]) {
            altsIval <- Intervals_full(matrix(sort(c(a[crit], b[crit])), ncol=2, byrow=TRUE), closed=c(TRUE, TRUE))
            ivalCrit <- piMinus[[crit]]
            intsec <- interval_intersection(altsIval, ivalCrit)
            fullOverlap = FALSE
            if (any(dim(intsec) != dim(ivalCrit))) {
              fullOverlap = TRUE
            } else if (any(intsec != ivalCrit)) {
              fullOverlap = TRUE
            }
            if (!fullOverlap) {
              if (a[crit] > b[crit] && nec[j,i] == -2) {
                nec[j,i] <- -1
              } else if (nec[i,j] == -2) { # b > a
                nec[i,j] <- -1
              }
            }
          }
        }
      }
    }
  }
  return(nec)
}

singleStatementInference <- function(nec, performances, preferences) {
  newFound <- TRUE
  nalts <- nrow(performances)

  restarts <- -1

  while(newFound) {
    restarts <- restarts + 1
    newFound <- FALSE
    for (i in 1:nalts) {
      for (j in 1:nalts) {
        if (nec[i,j] == -2) {
#          cat("Checking single statement inference: a =", i, ", b =",j,"\n")
          res <- checkSingleStatementNecessary(i, j, performances, preferences)
          if (res > 0) {
            nec[i,j] <- res
            newFound <- TRUE
            nec <- addTransitiveStatements(nec, i, j)
          }
        }
      }
    }
  }
  return(list(nec=nec, restarts=restarts))
}

fastror <- function(performances, preferences) {
  nalts <- nrow(performances)

  ## initialize necessary relation, -2 = no information
  nec <- matrix(-2, nrow=nalts, ncol=nalts)
  for (i in 1:nalts) {
    nec[i,i] <- 1
  }

  for (i in 1:nalts) {
    for (j in 1:nalts) {
      if (i != j) {
        if (all(performances[i,] > performances[j,])) {
          nec[i,j] <- 2
        }
      }
    }
  }

  # Set the preferences in there
  for (i in 1:nrow(preferences)) {
    aind <- preferences[i,1]
    bind <- preferences[i,2]
    nec[aind, bind] <- 1
  }

  # add transitive statements
  for (i in 1:nalts) {
    for (j in 1:nalts) {
      if (i != j && nec[i,j] > 0) {
        nec <- addTransitiveStatements(nec, i, j)
      }
    }
  }

  nec <- checkLemma1(nec, performances, preferences)
  ssinf <- singleStatementInference(nec, performances, preferences)

  return(ssinf)
}

performTest <- function(nalts, ncrit, npref) {
  cat ("Test:", nalts, "alts,", ncrit, "crits,", npref, "preference statements\n")

  ok <- FALSE
  while (!ok) {
    ok <- TRUE
    performances <- t(replicate(nalts, rnorm(ncrit)))

    allPrefs <- t(combn(nalts, 2))
    stopifnot(nrow(allPrefs) >= npref) # sanity check
    prefInds <- sample(seq(1:nrow(allPrefs)), npref)

    preferences <- allPrefs[prefInds,]

    for (i in 1:nrow(preferences)) {
      if(all(performances[preferences[i,2]] > performances[preferences[i,1]])) {
        ok <- FALSE
        break
      }
    }
  }

  cat ("Performances:\n")
  print(performances)
  
  cat ("Preferences:\n")
  print(preferences)

  fastres <- fastror(performances, preferences)
  utares <- utagms(performances, preferences, necessary=TRUE, strongPrefs=FALSE)

  return(list(fast=fastres$nec, uta=utares, restarts=fastres$restarts, perf=performances, pref=preferences))
}

fullTest <- function(nalts, ncrit, npref) {
  res <- performTest(nalts, ncrit, npref)
  diff <- res$uta - (res$fast > 0)
  return(list(res=res, diff=diff))
}

findDiff <- function(nalts, ncrit, npref) {
  seed <- 10
  while(TRUE) {
    print(seed)
    set.seed(seed)
    r <- fullTest(nalts, ncrit, npref)
    stopifnot(sum(r$diff > 0) == 0)
    seed <- seed + 1    
  }
}
