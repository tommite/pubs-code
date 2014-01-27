M <- 2
MINEPS <- 1E-10

combineConstraintsMatrix <- function(...) {
  allConst = list(...)

  lhs <- c()
  dir <- c()
  rhs <- c()

  for (const in allConst) {
    lhs <- rbind(lhs, const$lhs)
    dir <- rbind(dir, const$dir)
    rhs <- rbind(rhs, const$rhs)
  }

  return(list(lhs=lhs, dir=dir, rhs=rhs))
}

## performances: m x j matrix of performances, m alternatives and j criteria
## profiles: t x j matrix of category profiles, t categories
## assignments: a x 2 matrix of reference alternative assignments,
##   where each row is a single assignment and the first column index of the
##   alternative, the second column index of the desired class
## necessary: whether to compute the necessary (T) or possible (F) relation
## th: a n x 5 matrix of thresholds, each row of being of format:
##   q, qMult, p, pMult, ascending (TRUE or FALSE)
## revised: whether to use the revised assignment procedures (T),
##   or the original ones (F)
etricror <- function(performances, profiles, assignments, necessary=TRUE, th, revised=FALSE) {
  stopifnot(ncol(performances) == ncol(profiles))
  stopifnot(ncol(assignments) == 2)
  stopifnot(ncol(th) == 5)
  stopifnot(nrow(th) == ncol(performances))

  message("--- constructing base model")
  baseVars <- buildBaseModel(performances, profiles, assignments, th)
  asVarsEL <- buildELModel(performances, profiles, assignments, th, revised)
  asVarsEU <- buildEUModel(performances, profiles, assignments, th, revised)
  baseModel <- combineConstraintsMatrix(baseVars, asVarsEL, asVarsEU)  

  message("--- checking model consistency")
  cons <- checkETRICConsistency(baseModel, performances, profiles, assignments, th)
  if (!cons$val) {
    stop("inconsistent model")
  }
  message("--- model consistent, computing relation")
  
  rel <- matrix(nrow=nrow(performances), ncol=nrow(profiles))
  solutions <- c()
  for (i in 1:nrow(rel)) {
    for(j in 1:ncol(rel)) {
      message("alt ", i, " - cat ", j, " - necessary: ", necessary)
      if (necessary) {
        necres <- checkETRICRelationNecessary(baseModel, performances, profiles, assignments, i, j, th=th, revised=revised)
        rel[i,j] = necres$val
        solutions <- c(solutions, necres$sol)
      } else { # possible
        posres <- checkETRICRelationPossible(baseModel, performances, profiles, assignments, i, j, th=th, revised=revised)
        rel[i,j] = posres$val
      }
    }
  }
  
  if (!is.null(rownames(performances))) {
    rownames(rel) <- rownames(performances)
  }
  if (!is.null(rownames(profiles))) {
    colnames(rel) <- rownames(profiles)
  }
  list(relation=rel, solution=cons$sol)
}

sampleWeights <- function(n) {
  t <- c(0, sort(runif(n-1)), 1);
  t[2:(n+1)] - t[1:n]
}

sampleLambda <- function() {
  runif(1, min=0.5, max=1.0)
}

drawParams <- function(performances, profiles, assignments, th) {
  nCrit = ncol(performances)
  nAlts = nrow(performances)
  nCats = nrow(profiles)
  nAss = nrow(assignments)

  misses <- 0
  ok <- FALSE
  while (!ok) {
    ok <- TRUE
    lambda <- sampleLambda()
    w <- sampleWeights(nCrit)

    message("--- iteration ", (misses+1))
    for (a in 1:nAss) {
      alt <- performances[assignments[a,1],]
      worst <- getWorstAssignment(alt, profiles, w, lambda, th)
      best <- getBestAssignment(alt, profiles, w, lambda, th)
      classInd <- assignments[a, 2]
      message("w ", worst, " b ", best, " act ", classInd)
      if (worst != classInd || best != classInd) {
        ok = FALSE
        break
      }
    }
    if (!ok) {
      misses = misses + 1
    }
  }
  return(list(misses=misses,w=w,lambda=lambda))
}

getWorstAssignment <- function(alt, profiles, w, lambda, th) {
  stopifnot(nrow(th) == ncol(profiles))
  h = nrow(profiles) - 1
  while (h > 0) {
    b = profiles[h,]
    b1 = profiles[(h+1),]
    if (outrankingRelation(alt, b, w, lambda, th) && !outrankingRelation(b, alt, w, lambda, th) &&
        (concordance(alt, b1, w, th) > concordance(b, alt, w, th) ||
         (concordance(alt, b1, w, th) >= concordance(b, alt, w, th) &&
          (!outrankingRelation(a, b1, w, lambda, th) && !outrankingRelation(b1, a, w, lambda, th))))){
      return(h+1)
    }
    h <- h-1
  }
  return(1)
}

getBestAssignment <- function(alt, profiles, w, lambda, th) {
  h = 2
  while (h <= (nrow(profiles))) {
    b = profiles[h,]
    b1 = profiles[(h-1),]
    if (h <= nrow(profiles)) {
      if (outrankingRelation(b, alt, w, lambda, th) && !outrankingRelation(alt, b, w, lambda, th) &&
          (concordance(b1, alt, w, th) > concordance(alt, b, w, th) ||
           (concordance(b1, alt, w, th) >= concordance(b, alt, w, th) &&
            (!outrankingRelation(a, b1, w, lambda, th) &&
             !outrankingRelation(b1, a, w, lambda, th))))){
        return(h-1)
      }
    }
    h <- h+1
  }
  return(nrow(profiles))
}

checkETRICConsistency <- function(allConst, performances, profiles, assignments, th) {
  nAlts <- nrow(performances)
  nCrit <- ncol(performances)
  nAssignments <- nrow(assignments)
  nCats <- nrow(profiles)

  return(solveProblem(nAlts, nCats, nCrit, nAssignments, allConst, FALSE))
}

## aIndex: index of the alternative
## cIndex: index of the category
checkETRICRelationPossible <- function(allConst, performances, profiles, assignments, aIndex, h, th, revised) {

  nAlts <- nrow(performances)
  nCrit <- ncol(performances)
  nAssignments <- nrow(assignments)
  nCats <- nrow(profiles)
  
  if (h < nCats) {
    posModel1 <- buildLPModel(aIndex, h, performances, profiles, assignments, revised)
    allConst <- combineConstraintsMatrix(allConst, posModel1)
  }
  if (h > 1) {
    posModel2 <- buildUPModel(aIndex, h, performances, profiles, assignments, revised)
    allConst <- combineConstraintsMatrix(allConst, posModel2)
  }
  return(solveProblem(nAlts, nCats, nCrit, nAssignments, allConst, FALSE))
}

checkETRICRelationNecessary <- function(baseModel, performances, profiles, assignments, aInd, h, th, revised) {

  nAlts <- nrow(performances)
  nCrit <- ncol(performances)
  nAssignments <- nrow(assignments)
  nCats <- nrow(profiles)
  
  res1 <- list(val=TRUE)
  res2 <- list(val=TRUE)

  if (h < nCats) {
    necModelLeft <- buildNecModel(aInd, h, performances, profiles, assignments, th, TRUE, revised=revised)
    allConst1 <- combineConstraintsMatrix(baseModel, necModelLeft)
    res1 <- solveProblem(nAlts, nCats, nCrit, nAssignments, allConst1, TRUE)
  }
  if (h > 1) {
    necModelRight <- buildNecModel(aInd, h, performances, profiles, assignments, th, FALSE, revised=revised)
    allConst2 <- combineConstraintsMatrix(baseModel, necModelRight)
    res2 <- solveProblem(nAlts, nCats, nCrit, nAssignments, allConst2, TRUE)
  }
  val <- (res1$val && res2$val)
  list(val=val, res1sol=res1$sol, res2sol=res2$sol)
}


solveProblem <- function (nAlts, nCats, nCrit, nAssignments, allConst, necessary) {
  message("Solving MILP with ", ncol(allConst$lhs), " variables")
  
  obj <- L_objective(buildObjectiveFunction(nAlts, nCats, nCrit, nAssignments))
  roiConst <- L_constraint(allConst$lhs, allConst$dir, allConst$rhs)
  
  lp <- OP(objective=obj, constraints=roiConst, maximum=TRUE, types=getConstraintTypes(nAlts, nCats, nCrit, nAssignments))
  
  ret <- ROI_solve(lp, .solver)

  val <- FALSE
  
  if (necessary == TRUE) {
    val <- (ret$status$code != 0 || ret$objval < MINEPS)
  } else { # possible
    val <- (ret$status$code == 0 && ret$objval >= MINEPS)
  }
  list(val=val, sol=ret$solution)
}

optimizeLambda <- function (performances, profiles, assignments, th, toMax, revised=FALSE) {
  nAlts <- nrow(performances)
  nCats <- nrow(profiles)
  nCrit <- ncol(performances)
  nAssignments <- nrow(assignments)

  baseVars <- buildBaseModel(performances, profiles, assignments, th)
  asVarsEL <- buildELModel(performances, profiles, assignments, th, revised)
  asVarsEU <- buildEUModel(performances, profiles, assignments, th, revised)
  baseModel <- combineConstraintsMatrix(baseVars, asVarsEL, asVarsEU)
  
  message("Solving MILP with ", ncol(baseModel$lhs), " variables")

  objrow = rep(0, getNrBaseVars(nAlts, nCrit, nAssignments, nCats))
  objrow[getLambdaIndex(nAlts, nCrit, nCats)] = 1
  
  obj <- L_objective(objrow)
  roiConst <- L_constraint(baseModel$lhs, baseModel$dir, baseModel$rhs)
  
  lp <- OP(objective=obj, constraints=roiConst, maximum=toMax, types=getConstraintTypes(nAlts, nCats, nCrit, nAssignments))
  
  ret <- ROI_solve(lp, .solver)

  if (ret$status$code != 0) {
    error("No feasible solution found")
  }
  ret$objval
}

getConstraintTypes <- function(nAlts, nCats, nCrit, nAssignments) {
  row = rep("C", getNrBaseVars(nAlts, nCrit, nAssignments, nCats))
  nr01 <- nAssignments * 2 + 12 + 2
  row[(length(row)-nr01+1):length(row)] = "B"
  return(row)
}

buildObjectiveFunction <- function(nAlts, nCats, nCrit, nAssignments) {
  row = rep(0, getNrBaseVars(nAlts, nCrit, nAssignments, nCats))
  row[getEpsilonIndex(nAlts, nCrit, nCats)] = 1
  return(row)
}

buildBaseModel <- function(performances, profiles, assignments, th) {
  nAlts <- nrow(performances)
  nCrit <- ncol(performances)
  nAssignments <- nrow(assignments)
  nCats <- nrow(profiles)

  ec <- buildEpsilonStrictlyPositiveConstraint(nAlts, nCrit, nAssignments, nCats)
  b1 <- buildB1Constraint(nAlts, nCrit, nAssignments, nCats)
  b2 <- buildB2Constraint(nAlts, nCrit, nAssignments, nCats)
  b3 <- buildB3Constraint(nAlts, nCrit, nAssignments, nCats)
  b4 <- buildB4Constraint(nAlts, nCrit, nAssignments, nCats)
  b5 <- buildB5Constraint(performances, profiles, nAssignments, th)

  allConst <- combineConstraintsMatrix(ec, b1, b2, b3, b4, b5)
  colnames(allConst$lhs) <- getColNames(nAlts, nCrit, nAssignments, nCats)
  return(allConst)
}

buildEpsilonStrictlyPositiveConstraint <- function(nAlts, nCrit, nAssignments, nCats) {
  row <- rep(0, getNrBaseVars(nAlts, nCrit, nAssignments, nCats))
  row[getEpsilonIndex(nAlts, nCrit, nCats)] = 1   
  row = t(as.matrix(row))
  rownames(row) <- c("EC")
  dir = matrix(c(">="))
  rownames(dir) <- c("EC")
  rhs = matrix(c(MINEPS))
  rownames(rhs) <- c("EC")
  return(list(lhs=row,dir=dir, rhs = rhs))
}

buildLPModel <- function(aInd, h, performances, profiles, assignments, revised) {
  nAlts <- nrow(performances)
  nCrit <- ncol(performances)
  nAssignments <- nrow(assignments)
  nCats <- nrow(profiles)

  lp1 <- buildLP1Constraint(aInd, h, performances, profiles, assignments)
  lp2 <- buildLP2Constraint(aInd, h, performances, profiles, assignments)
  lp3 <- buildLP3Constraint(aInd, h, performances, profiles, assignments)
  lp4 <- buildLP4Constraint(aInd, h, performances, profiles, assignments)
  lp5 <- buildLP5Constraint(aInd, h, performances, profiles, assignments)
  lp6 <- buildLP6Constraint(aInd, h, performances, profiles, assignments)
  lp7 <- buildLP7Constraint(aInd, h, performances, profiles, assignments)
  lp8 <- buildLP8Constraint(aInd, h, performances, profiles, assignments)

  if (revised) {
      allConst <- combineConstraintsMatrix(lp1, lp2, lp3, lp7)
  } else {
      allConst <- combineConstraintsMatrix(lp1, lp2, lp3, lp4, lp5, lp6, lp7, lp8)
  }
  colnames(allConst$lhs) <- getColNames(nAlts, nCrit, nAssignments, nCats)
  
  return(allConst)
}

buildLP1Constraint <- function(aInd, h, performances, profiles, assignments) {
  nAlts <- nrow(performances)
  nCats <- nrow(profiles)
  nCrit <- ncol(performances)
  nAssignments <- nrow(assignments)

  stopifnot(aInd >= 1 && aInd <= nrow(performances))
  stopifnot(h >= 1 && h <= nrow(profiles))
  
  lhs <- matrix(nrow=0, ncol=getNrBaseVars(nAlts, nCrit, nAssignments, nCats))
  nr <- 0
  dir <- matrix(ncol=1, nrow=0)
  rhs <- matrix(ncol=1, nrow=0)

  if (h < nCats) {
    row = buildCABrow(aInd, h, nAlts, nCrit, nAssignments, nCats)
    row[getLambdaIndex(nAlts, nCrit, nCats)] = -1
    row[getEpsilonIndex(nAlts, nCrit, nCats)] = 1   
    row[getPossibleVarIndex(1, nAlts, nCrit, nCats, nAssignments)] = M 
    lhs <- rbind(lhs, row)
    dir <- rbind(dir, "<=")
    rhs <- rbind(rhs, M)
    
    nr <- nr + 1
  }
  rnames <- rep("LP1", nr)
  rownames(lhs) <- rnames
  rownames(dir) <- rnames  
  rownames(rhs) <- rnames
  
  return(list(lhs=lhs, dir=dir, rhs=rhs))  
}

buildLP7Constraint <- function(aInd, h, performances, profiles, assignments) {
  nAlts <- nrow(performances)
  nCats <- nrow(profiles)
  nCrit <- ncol(performances)
  nAssignments <- nrow(assignments)

  stopifnot(aInd >= 1 && aInd <= nrow(performances))
  stopifnot(h >= 1 && h <= nrow(profiles))
  
  lhs <- matrix(nrow=0, ncol=getNrBaseVars(nAlts, nCrit, nAssignments, nCats)) 
  nr <- 0
  dir <- matrix(ncol=1, nrow=0)
  rhs <- matrix(ncol=1, nrow=0)
  if (h < nCats) {
    row <- rep(0, getNrBaseVars(nAlts, nCrit, nAssignments, nCats))
    row[getPossibleVarIndex(1, nAlts, nCrit, nCats, nAssignments)] = 1
    row[getPossibleVarIndex(3, nAlts, nCrit, nCats, nAssignments)] = 1
    row[getPossibleVarIndex(5, nAlts, nCrit, nCats, nAssignments)] = 1
    lhs <- rbind(lhs, row)
    dir <- rbind(dir, "==")
    rhs <- rbind(rhs, 1)
    
    nr <- nr + 1
  }
  
  rnames <- rep("LP7", nr)
  rownames(lhs) <- rnames
  rownames(dir) <- rnames  
  rownames(rhs) <- rnames
  
  return(list(lhs=lhs, dir=dir, rhs=rhs))  
}

buildLP8Constraint <- function(aInd, h, performances, profiles, assignments) {
  nAlts <- nrow(performances)
  nCats <- nrow(profiles)
  nCrit <- ncol(performances)
  nAssignments <- nrow(assignments)

  stopifnot(aInd >= 1 && aInd <= nrow(performances))
  stopifnot(h >= 1 && h <= nrow(profiles))
  
  lhs <- matrix(nrow=0, ncol=getNrBaseVars(nAlts, nCrit, nAssignments, nCats))
  nr <- 0
  dir <- matrix(ncol=1, nrow=0)
  rhs <- matrix(ncol=1, nrow=0)
  if (h < nCats) {
    row <- rep(0, getNrBaseVars(nAlts, nCrit, nAssignments, nCats))
    row[getPossibleVarIndex(5, nAlts, nCrit, nCats, nAssignments)] = -1
    row[getPossibleVarIndex(7, nAlts, nCrit, nCats, nAssignments)] = 1
    row[getPossibleVarIndex(9, nAlts, nCrit, nCats, nAssignments)] = 1
    row[getPossibleVarIndex(11, nAlts, nCrit, nCats, nAssignments)] = 1
    lhs <- rbind(lhs, row)
    dir <- rbind(dir, "==")
    rhs <- rbind(rhs, 0)
    
    nr <- nr + 1
  }
  
  rnames <- rep("LP8", nr)
  rownames(lhs) <- rnames
  rownames(dir) <- rnames  
  rownames(rhs) <- rnames
  
  return(list(lhs=lhs, dir=dir, rhs=rhs))  
}

buildLP4Constraint <- function(aInd, h, performances, profiles, assignments) {
  nAlts <- nrow(performances)
  nCats <- nrow(profiles)
  nCrit <- ncol(performances)
  nAssignments <- nrow(assignments)

  stopifnot(aInd >= 1 && aInd <= nrow(performances))
  stopifnot(h >= 1 && h <= nrow(profiles))
  
  lhs <- matrix(nrow=0, ncol=getNrBaseVars(nAlts, nCrit, nAssignments, nCats))
  nr <- 0
  dir <- matrix(ncol=1, nrow=0)
  rhs <- matrix(ncol=1, nrow=0)
  if (h < nCats) {
    rowAB = buildCABrow(aInd, h+1, nAlts, nCrit, nAssignments, nCats)
    rowBA = buildCBArow(aInd, h, nAlts, nCrit, nAssignments, nCats)
    row = rowBA - rowAB
    row[getPossibleVarIndex(7, nAlts, nCrit, nCats, nAssignments)] = -M
    row[getEpsilonIndex(nAlts, nCrit, nCats)] = -1
    lhs <- rbind(lhs, row)
    dir <- rbind(dir, ">=")
    rhs <- rbind(rhs, -M)
    
    nr <- nr + 1
  }
  
  rnames <- rep("LP4", nr)
  rownames(lhs) <- rnames
  rownames(dir) <- rnames
  rownames(rhs) <- rnames
  
  return(list(lhs=lhs, dir=dir, rhs=rhs))  
}

buildLP5Constraint <- function(aInd, h, performances, profiles, assignments) {
  nAlts <- nrow(performances)
  nCats <- nrow(profiles)
  nCrit <- ncol(performances)
  nAssignments <- nrow(assignments)

  stopifnot(aInd >= 1 && aInd <= nrow(performances))
  stopifnot(h >= 1 && h <= nrow(profiles))
  
  lhs <- matrix(nrow=0, ncol=getNrBaseVars(nAlts, nCrit, nAssignments, nCats))
  nr <- 0
  dir <- matrix(ncol=1, nrow=0)
  rhs <- matrix(ncol=1, nrow=0)
  if (h < nCats) {
    row = buildCABrow(aInd, h+1, nAlts, nCrit, nAssignments, nCats)
    row[getPossibleVarIndex(9, nAlts, nCrit, nCats, nAssignments)] = -M
    row[getLambdaIndex(nAlts, nCrit, nCats)] = -1
    lhs <- rbind(lhs, row)
    dir <- rbind(dir, ">=")
    rhs <- rbind(rhs, -M)
    
    nr <- nr + 1
  }
  
  rnames <- rep("LP5", nr)
  rownames(lhs) <- rnames
  rownames(dir) <- rnames
  rownames(rhs) <- rnames
  
  return(list(lhs=lhs, dir=dir, rhs=rhs))  
}

buildLP6Constraint <- function(aInd, h, performances, profiles, assignments) {
  nAlts <- nrow(performances)
  nCats <- nrow(profiles)
  nCrit <- ncol(performances)
  nAssignments <- nrow(assignments)

  stopifnot(aInd >= 1 && aInd <= nrow(performances))
  stopifnot(h >= 1 && h <= nrow(profiles))
  
  lhs <- matrix(nrow=0, ncol=getNrBaseVars(nAlts, nCrit, nAssignments, nCats))
  nr <- 0
  dir <- matrix(ncol=1, nrow=0)
  rhs <- matrix(ncol=1, nrow=0)
  if (h < nCats) {
    row = buildCBArow(aInd, h+1, nAlts, nCrit, nAssignments, nCats)
    row[getPossibleVarIndex(11, nAlts, nCrit, nCats, nAssignments)] = -M
    row[getLambdaIndex(nAlts, nCrit, nCats)] = -1
    lhs <- rbind(lhs, row)
    dir <- rbind(dir, ">=")
    rhs <- rbind(rhs, -M)
    
    nr <- nr + 1
  }
  
  rnames <- rep("LP6", nr)
  rownames(lhs) <- rnames
  rownames(dir) <- rnames
  rownames(rhs) <- rnames
  
  return(list(lhs=lhs, dir=dir, rhs=rhs))  
}

buildLP3Constraint <- function(aInd, h, performances, profiles, assignments) {
  nAlts <- nrow(performances)
  nCats <- nrow(profiles)
  nCrit <- ncol(performances)
  nAssignments <- nrow(assignments)

  stopifnot(aInd >= 1 && aInd <= nrow(performances))
  stopifnot(h >= 1 && h <= nrow(profiles))
  
  lhs <- matrix(nrow=0, ncol=getNrBaseVars(nAlts, nCrit, nAssignments, nCats))
  nr <- 0
  dir <- matrix(ncol=1, nrow=0)
  rhs <- matrix(ncol=1, nrow=0)
  if (h < nCats) {
    rowAB = buildCABrow(aInd, h+1, nAlts, nCrit, nAssignments, nCats)
    rowBA = buildCBArow(aInd, h, nAlts, nCrit, nAssignments, nCats)
    row = rowBA - rowAB
    row[getPossibleVarIndex(5, nAlts, nCrit, nCats, nAssignments)] = -M 
    lhs <- rbind(lhs, row)
    dir <- rbind(dir, ">=")
    rhs <- rbind(rhs, -M)
    
    nr <- nr + 1
  }
  
  rnames <- rep("LP3", nr)
  rownames(lhs) <- rnames
  rownames(dir) <- rnames
  rownames(rhs) <- rnames
  
  return(list(lhs=lhs, dir=dir, rhs=rhs))  
}

buildLP2Constraint <- function(aInd, h, performances, profiles, assignments) {
  nAlts <- nrow(performances)
  nCats <- nrow(profiles)
  nCrit <- ncol(performances)
  nAssignments <- nrow(assignments)

  stopifnot(aInd >= 1 && aInd <= nrow(performances))
  stopifnot(h >= 1 && h <= nrow(profiles))
  
  lhs <- matrix(nrow=0, ncol=getNrBaseVars(nAlts, nCrit, nAssignments, nCats))
  nr <- 0
  dir <- matrix(ncol=1, nrow=0)
  rhs <- matrix(ncol=1, nrow=0)
  if (h < nCats) {
    row = buildCBArow(aInd, h, nAlts, nCrit, nAssignments, nCats)
    row[getLambdaIndex(nAlts, nCrit, nCats)] = -1
    row[getPossibleVarIndex(3, nAlts, nCrit, nCats, nAssignments)] = -M 
    lhs <- rbind(lhs, row)
    dir <- rbind(dir, ">=")
    rhs <- rbind(rhs, -M)
    
    nr <- nr + 1
  }
  
  rnames <- rep("LP2", nr)
  rownames(lhs) <- rnames
  rownames(dir) <- rnames  
  rownames(rhs) <- rnames
  
  return(list(lhs=lhs, dir=dir, rhs=rhs))  
}

buildUPModel <- function(aInd, h, performances, profiles, assignments, revised) {
  nAlts <- nrow(performances)
  nCrit <- ncol(performances)
  nAssignments <- nrow(assignments)
  nCats <- nrow(profiles)

  up1 <- buildUP1Constraint(aInd, h, performances, profiles, assignments)
  up2 <- buildUP2Constraint(aInd, h, performances, profiles, assignments)
  up3 <- buildUP3Constraint(aInd, h, performances, profiles, assignments)
  up4 <- buildUP4Constraint(aInd, h, performances, profiles, assignments)
  up5 <- buildUP5Constraint(aInd, h, performances, profiles, assignments)
  up6 <- buildUP6Constraint(aInd, h, performances, profiles, assignments)
  up7 <- buildUP7Constraint(aInd, h, performances, profiles, assignments)
  up8 <- buildUP8Constraint(aInd, h, performances, profiles, assignments)

  if (revised) {
      allConst <- combineConstraintsMatrix(up1, up2, up3, up7)
  } else {
      allConst <- combineConstraintsMatrix(up1, up2, up3, up4, up5, up6, up7, up8)
  }
  colnames(allConst$lhs) <- getColNames(nAlts, nCrit, nAssignments, nCats)
  
  return(allConst)
}

buildUP1Constraint <- function(aInd, h, performances, profiles, assignments) {
  nAlts <- nrow(performances)
  nCats <- nrow(profiles)
  nCrit <- ncol(performances)
  nAssignments <- nrow(assignments)

  stopifnot(aInd >= 1 && aInd <= nrow(performances))
  stopifnot(h >= 1 && h <= nrow(profiles))
  
  lhs <- matrix(nrow=0, ncol=getNrBaseVars(nAlts, nCrit, nAssignments, nCats))
  nr <- 0
  dir <- matrix(ncol=1, nrow=0)
  rhs <- matrix(ncol=1, nrow=0)
  if (h > 1) {
    row = buildCBArow(aInd, h, nAlts, nCrit, nAssignments, nCats)
    row[getLambdaIndex(nAlts, nCrit, nCats)] = -1
    row[getEpsilonIndex(nAlts, nCrit, nCats)] = 1   
    row[getPossibleVarIndex(2, nAlts, nCrit, nCats, nAssignments)] = M
    lhs <- rbind(lhs, row)
    dir <- rbind(dir, "<=")
    rhs <- rbind(rhs, M)
    
    nr <- nr + 1
  }
  rnames <- rep("UP1", nr)
  rownames(lhs) <- rnames
  rownames(dir) <- rnames  
  rownames(rhs) <- rnames

  return(list(lhs=lhs, dir=dir, rhs=rhs))  
}

buildUP7Constraint <- function(aInd, h, performances, profiles, assignments) {
  nAlts <- nrow(performances)
  nCats <- nrow(profiles)
  nCrit <- ncol(performances)
  nAssignments <- nrow(assignments)

  stopifnot(aInd >= 1 && aInd <= nrow(performances))
  stopifnot(h >= 1 && h <= nrow(profiles))
  
  lhs <- matrix(nrow=0, ncol=getNrBaseVars(nAlts, nCrit, nAssignments, nCats))
  nr <- 0
  dir <- matrix(ncol=1, nrow=0)
  rhs <- matrix(ncol=1, nrow=0)
  if (h > 1) {
    row <- rep(0, getNrBaseVars(nAlts, nCrit, nAssignments, nCats))
    row[getPossibleVarIndex(2, nAlts, nCrit, nCats, nAssignments)] = 1
    row[getPossibleVarIndex(4, nAlts, nCrit, nCats, nAssignments)] = 1
    row[getPossibleVarIndex(6, nAlts, nCrit, nCats, nAssignments)] = 1
    lhs <- rbind(lhs, row)
    dir <- rbind(dir, "==")
    rhs <- rbind(rhs, 1)
    
    nr <- nr + 1
  }
  
  rnames <- rep("UP7", nr)
  rownames(lhs) <- rnames
  rownames(dir) <- rnames  
  rownames(rhs) <- rnames
  
  return(list(lhs=lhs, dir=dir, rhs=rhs))  
}

buildUP8Constraint <- function(aInd, h, performances, profiles, assignments) {
  nAlts <- nrow(performances)
  nCats <- nrow(profiles)
  nCrit <- ncol(performances)
  nAssignments <- nrow(assignments)

  stopifnot(aInd >= 1 && aInd <= nrow(performances))
  stopifnot(h >= 1 && h <= nrow(profiles))
  
  lhs <- matrix(nrow=0, ncol=getNrBaseVars(nAlts, nCrit, nAssignments, nCats))
  nr <- 0
  dir <- matrix(ncol=1, nrow=0)
  rhs <- matrix(ncol=1, nrow=0)
  if (h > 1) {
    row <- rep(0, getNrBaseVars(nAlts, nCrit, nAssignments, nCats))
    row[getPossibleVarIndex(6, nAlts, nCrit, nCats, nAssignments)] = -1
    row[getPossibleVarIndex(8, nAlts, nCrit, nCats, nAssignments)] = 1
    row[getPossibleVarIndex(10, nAlts, nCrit, nCats, nAssignments)] = 1
    row[getPossibleVarIndex(12, nAlts, nCrit, nCats, nAssignments)] = 1
    lhs <- rbind(lhs, row)
    dir <- rbind(dir, "==")
    rhs <- rbind(rhs, 0)
    
    nr <- nr + 1
  }
  
  rnames <- rep("UP8", nr)
  rownames(lhs) <- rnames
  rownames(dir) <- rnames  
  rownames(rhs) <- rnames
  
  return(list(lhs=lhs, dir=dir, rhs=rhs))  
}

buildUP4Constraint <- function(aInd, h, performances, profiles, assignments) {
  nAlts <- nrow(performances)
  nCats <- nrow(profiles)
  nCrit <- ncol(performances)
  nAssignments <- nrow(assignments)

  stopifnot(aInd >= 1 && aInd <= nrow(performances))
  stopifnot(h >= 1 && h <= nrow(profiles))
  
  lhs <- matrix(nrow=0, ncol=getNrBaseVars(nAlts, nCrit, nAssignments, nCats))
  nr <- 0
  dir <- matrix(ncol=1, nrow=0)
  rhs <- matrix(ncol=1, nrow=0)
  if (h > 1) {
    rowAB = buildCABrow(aInd, h, nAlts, nCrit, nAssignments, nCats)
    rowBA = buildCBArow(aInd, h-1, nAlts, nCrit, nAssignments, nCats)
    row = rowAB - rowBA
    row[getPossibleVarIndex(8, nAlts, nCrit, nCats, nAssignments)] = -M
    row[getEpsilonIndex(nAlts, nCrit, nCats)] = -1
    lhs <- rbind(lhs, row)
    dir <- rbind(dir, ">=")
    rhs <- rbind(rhs, -M)
    
    nr <- nr + 1
  }
  
  rnames <- rep("UP4", nr)
  rownames(lhs) <- rnames
  rownames(dir) <- rnames
  rownames(rhs) <- rnames
  
  return(list(lhs=lhs, dir=dir, rhs=rhs))  
}

buildUP5Constraint <- function(aInd, h, performances, profiles, assignments) {
  nAlts <- nrow(performances)
  nCats <- nrow(profiles)
  nCrit <- ncol(performances)
  nAssignments <- nrow(assignments)

  stopifnot(aInd >= 1 && aInd <= nrow(performances))
  stopifnot(h >= 1 && h <= nrow(profiles))
  
  lhs <- matrix(nrow=0, ncol=getNrBaseVars(nAlts, nCrit, nAssignments, nCats))
  nr <- 0

  dir <- matrix(ncol=1, nrow=0)
  rhs <- matrix(ncol=1, nrow=0)
  if (h > 1) {
    row = buildCABrow(aInd, h-1, nAlts, nCrit, nAssignments, nCats)
    row[getPossibleVarIndex(10, nAlts, nCrit, nCats, nAssignments)] = -M
    row[getLambdaIndex(nAlts, nCrit, nCats)] = -1
    lhs <- rbind(lhs, row)
    dir <- rbind(dir, ">=")
    rhs <- rbind(rhs, -M)
    
    nr <- nr + 1
  }
  
  rnames <- rep("UP5", nr)
  rownames(lhs) <- rnames
  rownames(dir) <- rnames
  rownames(rhs) <- rnames
  
  return(list(lhs=lhs, dir=dir, rhs=rhs))  
}

buildUP6Constraint <- function(aInd, h, performances, profiles, assignments) {
  nAlts <- nrow(performances)
  nCats <- nrow(profiles)
  nCrit <- ncol(performances)
  nAssignments <- nrow(assignments)

  stopifnot(aInd >= 1 && aInd <= nrow(performances))
  stopifnot(h >= 1 && h <= nrow(profiles))
  
  lhs <- matrix(nrow=0, ncol=getNrBaseVars(nAlts, nCrit, nAssignments, nCats))
  nr <- 0
  dir <- matrix(ncol=1, nrow=0)
  rhs <- matrix(ncol=1, nrow=0)
  if (h > 1) {
    row = buildCBArow(aInd, h-1, nAlts, nCrit, nAssignments, nCats)
    row[getPossibleVarIndex(12, nAlts, nCrit, nCats, nAssignments)] = -M
    row[getLambdaIndex(nAlts, nCrit, nCats)] = -1
    lhs <- rbind(lhs, row)
    dir <- rbind(dir, ">=")
    rhs <- rbind(rhs, -M)
    
    nr <- nr + 1
  }
  
  rnames <- rep("UP6", nr)
  rownames(lhs) <- rnames
  rownames(dir) <- rnames
  rownames(rhs) <- rnames
  
  return(list(lhs=lhs, dir=dir, rhs=rhs))  
}


buildUP3Constraint <- function(aInd, h, performances, profiles, assignments) {
  nAlts <- nrow(performances)
  nCats <- nrow(profiles)
  nCrit <- ncol(performances)
  nAssignments <- nrow(assignments)

  stopifnot(aInd >= 1 && aInd <= nrow(performances))
  stopifnot(h >= 1 && h <= nrow(profiles))
  
  lhs <- matrix(nrow=0, ncol=getNrBaseVars(nAlts, nCrit, nAssignments, nCats))
  nr <- 0
  dir <- matrix(ncol=1, nrow=0)
  rhs <- matrix(ncol=1, nrow=0)
  if (h > 1) {
    rowAB = buildCABrow(aInd, h, nAlts, nCrit, nAssignments, nCats)
    rowBA = buildCBArow(aInd, h-1, nAlts, nCrit, nAssignments, nCats)
    row = rowAB - rowBA
    row[getPossibleVarIndex(6, nAlts, nCrit, nCats, nAssignments)] = -M 
    lhs <- rbind(lhs, row)
    dir <- rbind(dir, ">=")
    rhs <- rbind(rhs, -M)
    
    nr <- nr + 1
  }
  
  rnames <- rep("UP3", nr)
  rownames(lhs) <- rnames
  rownames(dir) <- rnames
  rownames(rhs) <- rnames
  
  return(list(lhs=lhs, dir=dir, rhs=rhs))  
}

buildUP2Constraint <- function(aInd, h, performances, profiles, assignments) {
  nAlts <- nrow(performances)
  nCats <- nrow(profiles)
  nCrit <- ncol(performances)
  nAssignments <- nrow(assignments)

  stopifnot(aInd >= 1 && aInd <= nrow(performances))
  stopifnot(h >= 1 && h <= nrow(profiles))
  
  lhs <- matrix(nrow=0, ncol=getNrBaseVars(nAlts, nCrit, nAssignments, nCats))
  nr <- 0
  dir <- matrix(ncol=1, nrow=0)
  rhs <- matrix(ncol=1, nrow=0)
  if (h > 1) {
    row = buildCABrow(aInd, h, nAlts, nCrit, nAssignments, nCats)
    row[getLambdaIndex(nAlts, nCrit, nCats)] = -1
    row[getPossibleVarIndex(4, nAlts, nCrit, nCats, nAssignments)] = -M 
    lhs <- rbind(lhs, row)
    dir <- rbind(dir, ">=")
    rhs <- rbind(rhs, -M)
    
    nr <- nr + 1
  }
  
  rnames <- rep("UP2", nr)
  rownames(lhs) <- rnames
  rownames(dir) <- rnames  
  rownames(rhs) <- rnames
  
  return(list(lhs=lhs, dir=dir, rhs=rhs))  
}

buildEUModel <- function(performances, profiles, assignments, th, revised) {
  nAlts <- nrow(performances)
  nCrit <- ncol(performances)
  nAssignments <- nrow(assignments)
  nCats <- nrow(profiles)

  allConst <- c()
  for (a in 1:nAssignments) {
    allConst <- combineConstraintsMatrix(allConst, buildEUModel1Ass(a, performances, profiles, assignments, th, revised))
  }
  
  colnames(allConst$lhs) <- getColNames(nAlts, nCrit, nAssignments, nCats)
  
  return(allConst)
}

buildEUModel1Ass <- function (a, performances, profiles, assignments, th, revised) {
  nAlts <- nrow(performances)
  nCats <- nrow(profiles)
  nCrit <- ncol(performances)
  nAssignments <- nrow(assignments)

  aInd = assignments[a, 1]
  cInd = assignments[a, 2]

  res <- c()
  ## Build EU1
  res <- matrix(nrow=0, ncol=getNrBaseVars(nAlts, nCrit, nAssignments, nCats))
  eu1 <- c()
  eu2 <- c()
  if (cInd < nCats) {
    ## EU1
    eu1row = buildCBArow(aInd, (cInd+1), nAlts, nCrit, nAssignments, nCats)
    eu1row[getLambdaIndex(nAlts, nCrit, nCats)] = -1
    res <- t(as.matrix(eu1row))
    dir <- as.matrix(rep(">=", nrow(res)))
    rhs <- as.matrix(rep(0, nrow(res)))
    eu1 = addRowNames(list(lhs=res, dir=dir, rhs=rhs), "EU1")

    ## EU2
    eu2row = buildCABrow(aInd, (cInd+1), nAlts, nCrit, nAssignments, nCats)
    eu2row[getLambdaIndex(nAlts, nCrit, nCats)] = -1
    eu2row[getEpsilonIndex(nAlts, nCrit, nCats)] = 1
    res <- t(as.matrix(eu2row))
    dir <- as.matrix(rep("<=", nrow(res)))
    rhs <- as.matrix(rep(0, nrow(res)))
    eu2 = addRowNames(list(lhs=res, dir=dir, rhs=rhs), "EU2")
  }
  
  eu3 <- buildEU3Constraint(a, performances, profiles, assignments, th, revised)
  eu41 <- buildEU41Constraint(a, performances, profiles, assignments, th)
  eu42 <- buildEU42Constraint(a, performances, profiles, assignments, th)
  eu43 <- buildEU43Constraint(a, performances, profiles, assignments, th)
  if (revised) {
      return(combineConstraintsMatrix(eu1, eu2, eu3))
  } else {
      return(combineConstraintsMatrix(eu1, eu2, eu3, eu41, eu42, eu43))
  }
}

addRowNames <- function(consts, name) {
  nrows <- nrow(consts$lhs)
  if (nrows > 0) {
    names <- rep(name, nrows)
    rownames(consts$lhs) <- names
    rownames(consts$dir) <- names
    rownames(consts$rhs) <- names
  }
  return(consts)
}

buildEU43Constraint <- function(i, performances, profiles, assignments, th) {    
  nAlts <- nrow(performances)
  nCats <- nrow(profiles)
  nCrit <- ncol(performances)
  nAssignments <- nrow(assignments)

  res <- matrix(nrow=0, ncol=getNrBaseVars(nAlts, nCrit, nAssignments, nCats))
  nr <- 0
  aInd = assignments[i, 1]
  cInd = assignments[i, 2]
  if (cInd < nCats) {
    rowBA = buildCBArow(aInd, cInd, nAlts, nCrit, nAssignments, nCats)
    row = -rowBA
    row[getLambdaIndex(nAlts, nCrit, nCats)] = 1
    row[getEpsilonIndex(nAlts, nCrit, nCats)] = -1
    row[getAssignmentVarIndex(i, FALSE, nAlts, nCrit, nCats)] = -M
    res <-  rbind(res, row)
    nr <- nr + 1
  }

  rnames <- rep("EU43", nr)
  res <- as.matrix(res)
  dir <- as.matrix(rep(">=", nr))
  rhs <- as.matrix(rep(-M, nr))
  rownames(res) <- rnames
  rownames(dir) <- rnames  
  rownames(rhs) <- rnames
  
  return(list(lhs=res, dir=dir, rhs=rhs))
}

buildEU42Constraint <- function(i, performances, profiles, assignments, th) {    
  nAlts <- nrow(performances)
  nCats <- nrow(profiles)
  nCrit <- ncol(performances)
  nAssignments <- nrow(assignments)

  res <- matrix(nrow=0, ncol=getNrBaseVars(nAlts, nCrit, nAssignments, nCats))
  nr <- 0
  aInd = assignments[i, 1]
  cInd = assignments[i, 2]
  if (cInd < nCats) {
    rowAB = buildCABrow(aInd, cInd, nAlts, nCrit, nAssignments, nCats)
    row = -rowAB
    row[getLambdaIndex(nAlts, nCrit, nCats)] = 1
    row[getEpsilonIndex(nAlts, nCrit, nCats)] = -1
    row[getAssignmentVarIndex(i, FALSE, nAlts, nCrit, nCats)] = -M
    res <-  rbind(res, row)
    nr <- nr + 1
  }

  rnames <- rep("EU42", nr)
  res <- as.matrix(res)
  dir <- as.matrix(rep(">=", nr))
  rhs <- as.matrix(rep(-M, nr))
  rownames(res) <- rnames
  rownames(dir) <- rnames  
  rownames(rhs) <- rnames
  
  return(list(lhs=res, dir=dir, rhs=rhs))
}

buildEU41Constraint <- function(i, performances, profiles, assignments, th) {    
  nAlts <- nrow(performances)
  nCats <- nrow(profiles)
  nCrit <- ncol(performances)
  nAssignments <- nrow(assignments)

  res <- matrix(nrow=0, ncol=getNrBaseVars(nAlts, nCrit, nAssignments, nCats))
  nr <- 0
  
  aInd = assignments[i, 1]
  cInd = assignments[i, 2]
  if (cInd < nCats) {
    rowAB = buildCABrow(aInd, (cInd+1), nAlts, nCrit, nAssignments, nCats)
    rowBA = buildCBArow(aInd, cInd, nAlts, nCrit, nAssignments, nCats)
    row = rowBA - rowAB
    row[getAssignmentVarIndex(i, FALSE, nAlts, nCrit, nCats)] = -M
    res <-  rbind(res, row)
    nr <- nr + 1
  }

  rnames <- rep("EU41", nr)
  res <- as.matrix(res)
  dir <- as.matrix(rep(">=", nr))
  rhs <- as.matrix(rep(-M, nr))
  rownames(res) <- rnames
  rownames(dir) <- rnames  
  rownames(rhs) <- rnames
  
  return(list(lhs=res, dir=dir, rhs=rhs))
}

buildEU3Constraint <- function(i, performances, profiles, assignments, th, revised) {    
  nAlts <- nrow(performances)
  nCats <- nrow(profiles)
  nCrit <- ncol(performances)
  nAssignments <- nrow(assignments)

  res <- matrix(nrow=0, ncol=getNrBaseVars(nAlts, nCrit, nAssignments, nCats))
  nr <- 0
  aInd = assignments[i, 1]
  cInd = assignments[i, 2]
  if (cInd < nCats) {
    rowAB = buildCABrow(aInd, (cInd+1), nAlts, nCrit, nAssignments, nCats)
    rowBA = buildCBArow(aInd, cInd, nAlts, nCrit, nAssignments, nCats)
    row = rowBA - rowAB
    row[getEpsilonIndex(nAlts, nCrit, nCats)] = -1
    if (!revised) {
        row[getAssignmentVarIndex(i, FALSE, nAlts, nCrit, nCats)] = M
    }
    res <-  rbind(res, row)
    nr <- nr + 1
  }

  rnames <- rep("EU3", nr)
  res <- as.matrix(res)
  dir <- as.matrix(rep(">=", nr))
  rhs <- as.matrix(rep(0, nr))
  rownames(res) <- rnames
  rownames(dir) <- rnames  
  rownames(rhs) <- rnames
  
  return(list(lhs=res, dir=dir, rhs=rhs))
}

buildEU1Constraint <- function(i, performances, profiles, assignments, th) {    
  nAlts <- nrow(performances)
  nCats <- nrow(profiles)
  nCrit <- ncol(performances)
  nAssignments <- nrow(assignments)

  res <- matrix(nrow=0, ncol=getNrBaseVars(nAlts, nCrit, nAssignments, nCats))
  nr <- 0
  aInd = assignments[i, 1]
  cInd = assignments[i, 2]
  if (cInd < nCats) {
    row = buildCBArow(aInd, (cInd+1), nAlts, nCrit, nAssignments, nCats)
    row[getLambdaIndex(nAlts, nCrit, nCats)] = -1
    res <-  rbind(res, row)
    nr <- nr + 1
  }

  rnames <- rep("EU1", nr)
  res <- as.matrix(res)
  dir <- as.matrix(rep(">=", nr))
  rhs <- as.matrix(rep(0, nr))
  rownames(res) <- rnames
  rownames(dir) <- rnames  
  rownames(rhs) <- rnames
  
  return(list(lhs=res, dir=dir, rhs=rhs))
}

buildEU2Constraint <- function(i, performances, profiles, assignments, th) {    
  nAlts <- nrow(performances)
  nCats <- nrow(profiles)
  nCrit <- ncol(performances)
  nAssignments <- nrow(assignments)

  res <- matrix(nrow=0, ncol=getNrBaseVars(nAlts, nCrit, nAssignments, nCats))
  nr <- 0
  aInd = assignments[i, 1]
  cInd = assignments[i, 2]
  if (cInd < nCats) {
    row = buildCABrow(aInd, (cInd+1), nAlts, nCrit, nAssignments, nCats)
    row[getLambdaIndex(nAlts, nCrit, nCats)] = -1
    row[getEpsilonIndex(nAlts, nCrit, nCats)] = 1
    res <-  rbind(res, row)
    nr <- nr + 1
  }

  rnames <- rep("EU2", nr)
  res <- as.matrix(res)
  dir <- as.matrix(rep("<=", nr))
  rhs <- as.matrix(rep(0, nr))
  rownames(res) <- rnames
  rownames(dir) <- rnames  
  rownames(rhs) <- rnames
  
  return(list(lhs=res, dir=dir, rhs=rhs))
}

buildELModel <- function(performances, profiles, assignments, th, revised) {
  nAlts <- nrow(performances)
  nCrit <- ncol(performances)
  nAssignments <- nrow(assignments)
  nCats <- nrow(profiles)

  allConst <- c()
  for (a in 1:nAssignments) {
    allConst <- combineConstraintsMatrix(allConst, buildELModel1Ass(a, performances, profiles, assignments, th, revised))
  }
  colnames(allConst$lhs) <- getColNames(nAlts, nCrit, nAssignments, nCats)
  return(allConst)
}

buildELModel1Ass <- function(a, performances, profiles, assignments, th, revised) {
  el1 <- buildEL1Constraint(a, performances, profiles, assignments, th)
  el2 <- buildEL2Constraint(a, performances, profiles, assignments, th)
  el3 <- buildEL3Constraint(a, performances, profiles, assignments, th, revised)
  el41 <- buildEL41Constraint(a, performances, profiles, assignments, th)
  el42 <- buildEL42Constraint(a, performances, profiles, assignments, th)
  el43 <- buildEL43Constraint(a, performances, profiles, assignments, th)
  if (revised) {
      return(combineConstraintsMatrix(el1, el2, el3))
  } else {
      return(combineConstraintsMatrix(el1, el2, el3, el41, el42, el43))
  }
}

buildNecModel <- function(aInd, h, performances, profiles, assignments, th, left, revised) {
  nAlts <- nrow(performances)
  nCats <- nrow(profiles)
  nCrit <- ncol(performances)
  nAssignments <- nrow(assignments)

  fakeAs = assignments
  fakeAs[1,1] = aInd
  
  if (left) {
    stopifnot(h < nCats)
    fakeAs[1,2] = h+1

    m <- buildELModel1Ass(1, performances, profiles, fakeAs, th, revised)
    index <- getAssignmentVarIndex(1, TRUE, nAlts, nCrit, nCats)
    vars <- m$lhs[,index]
    m$lhs[,index] = 0
    m$lhs[,getNecessaryVarIndex(nAlts, nCrit, nCats, nAssignments, TRUE)] = t(vars)
  } else {
    stopifnot(h > 1)
    
    fakeAs[1,2] = h-1
    m <- buildEUModel1Ass(1, performances, profiles, fakeAs, th, revised)
    index <- getAssignmentVarIndex(1, FALSE, nAlts, nCrit, nCats)
    vars <- m$lhs[,index]
    m$lhs[,index] = 0
    m$lhs[,getNecessaryVarIndex(nAlts, nCrit, nCats, nAssignments, FALSE)] = t(vars)
  }

  colnames(m$lhs) <- getColNames(nAlts, nCrit, nAssignments, nCats)
  return(m)
}

buildEL43Constraint <- function(i, performances, profiles, assignments, th) {    
  nAlts <- nrow(performances)
  nCats <- nrow(profiles)
  nCrit <- ncol(performances)
  nAssignments <- nrow(assignments)

  res <- matrix(nrow=0, ncol=getNrBaseVars(nAlts, nCrit, nAssignments, nCats))
  nr <- 0
  aInd = assignments[i, 1]
  cInd = assignments[i, 2]
  if (cInd > 1) {
    rowBA = buildCBArow(aInd, (cInd), nAlts, nCrit, nAssignments, nCats)
    row = -rowBA
    row[getLambdaIndex(nAlts, nCrit, nCats)] = 1
    row[getEpsilonIndex(nAlts, nCrit, nCats)] = -1
    row[getAssignmentVarIndex(i, TRUE, nAlts, nCrit, nCats)] = -M
    res <-  rbind(res, row)
    nr <- nr + 1
  }

  rnames <- rep("EL43", nr)
  res <- as.matrix(res)
  dir <- as.matrix(rep(">=", nr))
  rhs <- as.matrix(rep(-M, nr))
  rownames(res) <- rnames
  rownames(dir) <- rnames  
  rownames(rhs) <- rnames
  
  return(list(lhs=res, dir=dir, rhs=rhs))
}

buildEL42Constraint <- function(i, performances, profiles, assignments, th) {    
  nAlts <- nrow(performances)
  nCats <- nrow(profiles)
  nCrit <- ncol(performances)
  nAssignments <- nrow(assignments)

  res <- matrix(nrow=0, ncol=getNrBaseVars(nAlts, nCrit, nAssignments, nCats))
  nr <- 0
  aInd = assignments[i, 1]
  cInd = assignments[i, 2]
  if (cInd > 1) {
    rowAB = buildCABrow(aInd, cInd, nAlts, nCrit, nAssignments, nCats)
    row = -rowAB
    row[getLambdaIndex(nAlts, nCrit, nCats)] = 1
    row[getEpsilonIndex(nAlts, nCrit, nCats)] = -1
    row[getAssignmentVarIndex(i, TRUE, nAlts, nCrit, nCats)] = -M
    res <-  rbind(res, row)
    nr <- nr + 1
  }

  rnames <- rep("EL42", nr)
  res <- as.matrix(res)
  dir <- as.matrix(rep(">=", nr))
  rhs <- as.matrix(rep(-M, nr))
  rownames(res) <- rnames
  rownames(dir) <- rnames  
  rownames(rhs) <- rnames
  
  return(list(lhs=res, dir=dir, rhs=rhs))
}

buildEL41Constraint <- function(i, performances, profiles, assignments, th) {    
  nAlts <- nrow(performances)
  nCats <- nrow(profiles)
  nCrit <- ncol(performances)
  nAssignments <- nrow(assignments)

  res <- matrix(nrow=0, ncol=getNrBaseVars(nAlts, nCrit, nAssignments, nCats))
  nr <- 0
  aInd = assignments[i, 1]
  cInd = assignments[i, 2]
  if (cInd > 1) {
    rowAB = buildCABrow(aInd, cInd, nAlts, nCrit, nAssignments, nCats)
    rowBA = buildCBArow(aInd, (cInd-1), nAlts, nCrit, nAssignments, nCats)
    row = rowAB - rowBA
    row[getAssignmentVarIndex(i, TRUE, nAlts, nCrit, nCats)] = -M
    res <-  rbind(res, row)
    nr <- nr + 1
  }

  rnames <- rep("EL41", nr)
  res <- as.matrix(res)
  dir <- as.matrix(rep(">=", nr))
  rhs <- as.matrix(rep(-M, nr))
  rownames(res) <- rnames
  rownames(dir) <- rnames  
  rownames(rhs) <- rnames
  
  return(list(lhs=res, dir=dir, rhs=rhs))
}

buildEL3Constraint <- function(i, performances, profiles, assignments, th, revised) {
  nAlts <- nrow(performances)
  nCats <- nrow(profiles)
  nCrit <- ncol(performances)
  nAssignments <- nrow(assignments)

  res <- matrix(nrow=0, ncol=getNrBaseVars(nAlts, nCrit, nAssignments, nCats))
  nr <- 0

  aInd = assignments[i, 1]
  cInd = assignments[i, 2]
  if (cInd > 1) {
    rowAB = buildCABrow(aInd, cInd, nAlts, nCrit, nAssignments, nCats)
    rowBA = buildCBArow(aInd, (cInd-1), nAlts, nCrit, nAssignments, nCats)
    row = rowAB - rowBA
    row[getEpsilonIndex(nAlts, nCrit, nCats)] = -1
    if (!revised) {
        row[getAssignmentVarIndex(i, TRUE, nAlts, nCrit, nCats)] = M
    }
    res <-  rbind(res, row)
    nr <- nr + 1
  }

  rnames <- rep("EL3", nr)
  res <- as.matrix(res)
  dir <- as.matrix(rep(">=", nr))
  rhs <- as.matrix(rep(0, nr))
  rownames(res) <- rnames
  rownames(dir) <- rnames  
  rownames(rhs) <- rnames
  
  return(list(lhs=res, dir=dir, rhs=rhs))
}

buildEL1Constraint <- function(i, performances, profiles, assignments, th) {    
  nAlts <- nrow(performances)
  nCats <- nrow(profiles)
  nCrit <- ncol(performances)
  nAssignments <- nrow(assignments)

  res <- matrix(nrow=0, ncol=getNrBaseVars(nAlts, nCrit, nAssignments, nCats))
  nr <- 0
  aInd = assignments[i, 1]
  cInd = assignments[i, 2]
  if (cInd > 1) {
    row = buildCABrow(aInd, (cInd-1), nAlts, nCrit, nAssignments, nCats)
    row[getLambdaIndex(nAlts, nCrit, nCats)] = -1
    res <-  rbind(res, row)
    nr <- nr + 1
  }

  rnames <- rep("EL1", nr)
  res <- as.matrix(res)
  dir <- as.matrix(rep(">=", nr))
  rhs <- as.matrix(rep(0, nr))
  rownames(res) <- rnames
  rownames(dir) <- rnames  
  rownames(rhs) <- rnames
  
  return(list(lhs=res, dir=dir, rhs=rhs))
}

buildEL2Constraint <- function(i, performances, profiles, assignments, th) {    
  nAlts <- nrow(performances)
  nCats <- nrow(profiles)
  nCrit <- ncol(performances)
  nAssignments <- nrow(assignments)

  res <- matrix(nrow=0, ncol=getNrBaseVars(nAlts, nCrit, nAssignments, nCats))
  nr <- 0
  aInd = assignments[i, 1]
  cInd = assignments[i, 2]
  if (cInd > 1) {
    row = buildCBArow(aInd, (cInd-1), nAlts, nCrit, nAssignments, nCats)
    row[getLambdaIndex(nAlts, nCrit, nCats)] = -1
    row[getEpsilonIndex(nAlts, nCrit, nCats)] = 1
    res <-  rbind(res, row)
    nr <- nr + 1
  }

  rnames <- rep("EL2", nr)
  res <- as.matrix(res)
  dir <- as.matrix(rep("<=", nr))
  rhs <- as.matrix(rep(0, nr))
  rownames(res) <- rnames
  rownames(dir) <- rnames  
  rownames(rhs) <- rnames
  
  return(list(lhs=res, dir=dir, rhs=rhs))
}

buildCABrow <- function(aInd, bInd, nAlts, nCrit, nAssignments, nCats) {
  row <- rep(0, getNrBaseVars(nAlts, nCrit, nAssignments, nCats))
  for (j in 1:nCrit) {
    row[getCjABIndex(j, aInd, bInd, nAlts, nCats, nCrit)] = 1
  }
  return(row)
}

buildCBArow <- function(aInd, bInd, nAlts, nCrit, nAssignments, nCats) {
  row <- rep(0, getNrBaseVars(nAlts, nCrit, nAssignments, nCats))
  for (j in 1:nCrit) {
    row[getCjBAIndex(j, aInd, bInd, nAlts, nCats, nCrit)] = 1
  }
  return(row)
}

getColNames <- function(nAlts, nCrit, nAssignments, nCats) {
  res <- paste('w', 1:nCrit, sep='')

  for (j in 1:nCrit) {
    for (a in 1:nAlts) {
      for (b in 1:nCats) {
        res = c(res, paste('c', j, '(a', a, ',b', b, ')', sep=''))
      }
    }
  }

  for (j in 1:nCrit) {
    for (b in 1:nCats) {    
      for (a in 1:nAlts) {
        res = c(res, paste('c', j, '(b', b, ',a', a, ')', sep=''))
      }
    }
  }

  for (j in 1:nCrit) {
    for (b in 1:(nCats-1)) {
      res = c(res, paste('c', j, '(b', b, ',b', (b+1), ')', sep=''))
    }
  }

  res = c(res, 'lam')
  res = c(res, 'e')

  for (a in 1:nAssignments) {
    res = c(res, paste("ass", a, "L", sep=''), paste("ass", a, "R", sep=''))
  }

  posInds <- c("1", "2", "3", "41", "42", "43")
  for (i in posInds) {
    res = c(res, paste("pos", i, "L", sep=''), paste("pos", i, "R", sep=''))
  }

  res = c(res, "necL", "necR")

  return(res)
}

buildB1Constraint <- function(nAlts, nCrit, nAssignments, nCats) {
  lhs <- rep(0, getNrBaseVars(nAlts, nCrit, nAssignments, nCats))
  for (j in 1 : nCrit) {
    lhs[getWjIndex(j)] = 1
  }
  lhs <- t(as.matrix(lhs))
  dir <- as.matrix("==")
  rhs <- as.matrix(c(1))
  rownames(lhs) <- c("B1")
  rownames(dir) <- c("B1")
  rownames(rhs) <- c("B1")
  return(list(lhs=lhs, dir=dir, rhs=rhs))
}

buildB2Constraint <- function(nAlts, nCrit, nAssignments, nCats) {
  res <- c()

  tMinus1 <- nCats - 1
  
  for (h in 1:tMinus1) {
    lhs <- rep(0, getNrBaseVars(nAlts, nCrit, nAssignments, nCats))
    for (j in seq(1 : nCrit)) {
      varIndex <- getCjBhBh1Index(nCrit, nAlts, nCats, j, h)
      lhs[varIndex] = 1
    }
    ## + epsilon
    lhs[getEpsilonIndex(nAlts, nCrit, nCats)] = 1
    ## -lambda
    lhs[getLambdaIndex(nAlts, nCrit, nCats)] = -1
    lhs <- t(as.matrix(lhs))
    rownames(lhs) <- c("B2")
    res <- rbind(res, lhs)
  }
  dir <- as.matrix(rep("<=", tMinus1))
  rownames(dir) <- rep("B2", tMinus1)
  rhs <- as.matrix(rep(0, tMinus1))
  rownames(rhs) <- rep("B2", tMinus1)
  return(list(lhs=res, dir=dir, rhs=rhs))
}

buildB3Constraint <- function(nAlts, nCrit, nAssignments, nCats) {
  lhs1 <- rep(0, getNrBaseVars(nAlts, nCrit, nAssignments, nCats))
  lhs1[getLambdaIndex(nAlts, nCrit, nCats)] = 1
  lhs2 <- rep(0, getNrBaseVars(nAlts, nCrit, nAssignments, nCats))
  lhs2[getLambdaIndex(nAlts, nCrit, nCats)] = 1

  lhs <- rbind(lhs1, lhs2)
  rownames(lhs) <- c("B3", "B3")
  rhs = as.matrix(c(0.5, 1.0))
  rownames(rhs) <- c("B3", "B3")
  dir <- as.matrix(c(">=", "<="))
  rownames(dir) <- c("B3", "B3")
  return(list(lhs=lhs, dir=dir, rhs=rhs))
}

buildB4Constraint <- function(nAlts, nCrit, nAssignments, nCats) {
  lhsRes <- c()
  rhsRes <- c()

  for (j in 1 : nCrit) {
    lhs1 <- rep(0, getNrBaseVars(nAlts, nCrit, nAssignments, nCats))
    lhs2 <- rep(0, getNrBaseVars(nAlts, nCrit, nAssignments, nCats))

    lhs1[getEpsilonIndex(nAlts, nCrit, nCats)] = 1
    lhs1[getWjIndex(j)] = -1
    
    lhs2[getWjIndex(j)] = 1

    lhsRes <- rbind(lhsRes, lhs1, lhs2)
    rhsRes <- rbind(rhsRes, 0, 0.5)
  }
  rownames(lhsRes) <- rep("B4", nrow(lhsRes))
  dir <- as.matrix(rep("<=", nCrit*2))
  rownames(dir) <- rep("B4", nrow(dir))
  rownames(rhsRes) <- rep("B4", nrow(rhsRes))
  return(list(lhs=lhsRes, dir=dir, rhs=rhsRes))
}

buildB5Constraint <- function(performances, profiles, nAssignments, th) {
  
  nAlts <- nrow(performances)
  nCats <- nrow(profiles)
  nCrit <- ncol(performances)

  nrows <- nCrit*nAlts*nCats*2 + nCrit*(nCats-1)
  
  lhs = matrix(0, ncol=getNrBaseVars(nAlts, nCrit, nAssignments, nCats), nrow=nrows)

  row <- 0
  for (j in 1 : nCrit) {
    for (aInd in 1 : nAlts) {
      for (bInd in 1 : nCats) {
        row = row + 1        
        indAB <- getCjABIndex(j, aInd, bInd, nAlts, nCats, nCrit)
        lhs[row,indAB] = 1
        lhs[row,getWjIndex(j)] = -1 *
          outranking(performances[aInd,j], profiles[bInd,j],
                     th[j,1], th[j,2], th[j, 3], th[j, 4], th[j, 5])
      }
    }
  }
  for (j in 1 : nCrit) {
    for (bInd in 1 : nCats) {
      for (aInd in 1 : nAlts) {
        row = row + 1        
        indBA <- getCjBAIndex(j, aInd, bInd, nAlts, nCats, nCrit)        
        lhs[row,indBA] = 1
        val <- -1 * outranking(profiles[bInd,j], performances[aInd,j],
                               th[j,1], th[j,2], th[j, 3], th[j, 4], th[j, 5])
        lhs[row,getWjIndex(j)] = val
      }
    }
  }
  for(j in 1:nCrit) {
    for (h in 1:(nCats-1)) {
      row = row + 1
      lhs[row,getWjIndex(j)] = -1 *
        outranking(profiles[h,j], profiles[h+1, j],
                               th[j,1], th[j,2], th[j, 3], th[j, 4], th[j, 5])
      lhs[row,getCjBhBh1Index(nCrit, nAlts, nCats, j, h)] = 1
    }    
  }
  rnames <- paste("B5.", seq(1:row), sep='')
  rownames(lhs) <- rnames
  dir <- as.matrix(rep("==", row))
  rownames(dir) <- rnames
  rhs <- as.matrix(rep(0, row))
  rownames(rhs) <- rnames
  return(list(lhs=lhs, dir=dir, rhs=rhs))
}

getLambdaIndex <- function(nAlts, nCrit, nCats) {
  return (nCrit + nCrit * nAlts * nCats * 2 + nCrit * (nCats-1) + 1)
}

getEpsilonIndex <- function(nAlts, nCrit, nCats) {
  return (getLambdaIndex(nAlts, nCrit, nCats) + 1)
}

getNecessaryVarIndex <- function(nAlts, nCrit, nCats, nAssignments, left) {
  offset = getEpsilonIndex(nAlts, nCrit, nCats) + 2 * nAssignments + 12
  if (left) {
    return (offset + 1)
  } else {
    return (offset + 2)
  }
}

getPossibleVarIndex <- function(varIndex, nAlts, nCrit, nCats, nAssignments) {
  offset = getEpsilonIndex(nAlts, nCrit, nCats) + 2 * nAssignments
  return (offset + varIndex)
}

getAssignmentVarIndex <- function(asIndex, left, nAlts, nCrit, nCats) {
  stopifnot(is.logical(left))
  
  offset = getEpsilonIndex(nAlts, nCrit, nCats) + 1
  if (left) {
    return((asIndex-1) * 2 + offset)
  } else {
    return((asIndex-1) * 2 + offset + 1)
  }
}

getCjBhBh1Index <- function (nCrit, nAlts, nCats, j, h) {
  stopifnot(h > 0 && h < nCats)
  stopifnot(j > 0 && j <= nCrit)
  
  offset <- nCrit +  (nCrit * nAlts * nCats * 2)
  tMinus1 <- nCats - 1
  return(offset + (j-1) * tMinus1 + h)
}

## order is j1a1b1, j1a1b2, j2a1b1, ...
getCjABIndex <- function(j, aInd, bInd, nAlts, nCats, nCrit) {
  stopifnot(bInd <= nCats && bInd > 0)
  stopifnot(aInd <= nAlts && aInd > 0)
  stopifnot(j <= nCrit && j > 0)
  
  offset <- nCrit
  index <- (j - 1) * nCats * nAlts + (aInd - 1) * nCats + bInd
  return (offset + index)
}

getCjBAIndex <- function(j, aInd, bInd, nAlts, nCats, nCrit) {
  stopifnot(bInd <= nCats && bInd > 0)
  stopifnot(aInd <= nAlts && aInd > 0)
  stopifnot(j <= nCrit && j > 0)

  offset <- nCrit + (nCrit * nAlts * nCats)
  return (offset + (j - 1) * nCats * nAlts + (bInd - 1) * nAlts + aInd)
}

getWjIndex <- function(j) {
  return (j)
}

getNrBaseVars <- function(nAlts, nCrit, nAssignments, nCats) {
  return(getEpsilonIndex(nAlts=nAlts, nCrit=nCrit, nCats=nCats)
         + 2 * nAssignments + 12 + 2)
}

outrankingRelation <- function(x, y, w, lambda, th) {
  concordance(x, y, w, th) >= lambda
}

concordance <- function (x, y, w, th) {
  stopifnot(nrow(th) == length(w))
  
  nCrit <- nrow(th)
  sum <- 0
  for (j in 1:nCrit) {
    sum = sum + w[j] * outranking(x[j], y[j], th[j,1], th[j,2], th[j,3], th[j,4], th[j,5])
  }
  sum
}

## p is preference threshold
## q is indifference threshold
outranking <- function(x, y, q, qMult, p, pMult, ascending) {
  stopifnot(p >= 0 && q >= 0 && p >= q)
  diff <- y - x
  if (ascending == FALSE) {
    diff = 0 - diff
  }

  indif <- q + qMult * x
  pref <- p + pMult * x
  
  if (diff <= indif) {
    return (1)
  } else if (diff >= pref) {
    return(0)
  } else {
    return ((pref-diff) / (pref - indif))
  }
}
