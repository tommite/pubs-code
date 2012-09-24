## Generates a value function
genValueFunc <- function(n) {
   c(0, sort(runif(n-2)), 1) * runif(1);
}

## Returns TRUE, if the values in v adhere to holistic preferences
checkValues <- function(v) {
  all((v[10] > v[9]), # check that DEN > AUT
  (v[3] > v[4]), # check that SPA > SWE
  (v[11] > v[12])); # check that FRA > CZE
}

data <- read.table(file="webometrics-euro.csv", sep=",", header=TRUE);
n <- length(data)-1; # nr criteria
m <- length(t(data[1])); # nr alternatives
meas <- data.matrix(data[,2:5])
set.seed(31337);
burninSize <- 50000;
thinSize <- 20;
mainIters <- 10000;

currentValueFunc <- matrix(nrow=n, ncol=m);
alternativeValues <- matrix(nrow=m, ncol=n);

## generate index vectors
ind <- c();
for (i in 1:n) {
  uelems <- sort(unique(meas[,i]));
  thisind <- match (meas[,i], uelems);
  ind <- cbind(ind, thisind);
}
colnames(ind) <- colnames(data)[2:5]

## number of characteristic points
nrPoints <- apply(ind, 2, max);

## matrix to store all value functions
allValueFuncs <- matrix(nrow=n*max(mainIters, burninSize), ncol=m);

assignAlternativeValues <- function() {
  ## permute the value functions
  for (i in 1:n) {
    alternativeValues[,i] <<- currentValueFunc[i,][ind[,i]]
  }
}

generateInitialValueFunctions <- function() {
  while(TRUE) {
    ## Generate a random one
    for (i in 1:n) {
      v <- genValueFunc(nrPoints[i]);
      currentValueFunc[i,1:length(v)] <<- v;
    }
    assignAlternativeValues();
    
    ## compute full values
    fullValues <- apply(alternativeValues, 1, sum);
    if (checkValues(fullValues)) {
      break;
    }
  }
}

gibbsSample <- function(iters, thinning, recAllV = FALSE) {
  currentCriterion = 1;
  currentPoint = 1;
  cumulativePointsSum <- cumsum(nrPoints);

  for (i in 1:(iters*thinning)) {
    currentPoint <- ((i-1) %% sum(nrPoints)) + 1;
    currentCriterion <- which.min(cumulativePointsSum < currentPoint);
    currentIndex <- currentPoint;
    if (currentCriterion > 1) {
      currentIndex = currentPoint - cumulativePointsSum[currentCriterion-1];
    }
    ## The first point of each value function is not updated (has to remain 0)
    if (currentIndex == 1) {
      currentIndex <- currentIndex + 1;
    }
    ## now we know which point to update: currentValueFunc[currentCriterion,currentIndex]
    ## we sample in the range from the previous point to the next one
    prevPoint <- currentValueFunc[currentCriterion,currentIndex-1];
    ## For last point, the max value is 1
    nextPoint <- 1;
    ## For the others, the next point
    if (currentIndex < nrPoints[currentCriterion]) {
      nextPoint <- currentValueFunc[currentCriterion,currentIndex+1];
    }
    newPoint <- runif(1, min=prevPoint, max=nextPoint);
    oldPoint <- currentValueFunc[currentCriterion, currentIndex];

    currentValueFunc[currentCriterion,currentIndex] = newPoint;

    ## assign the alternative values for this function
    alternativeValues[,currentCriterion] = currentValueFunc[currentCriterion,][ind[,currentCriterion]];

    ## compute full values
    fullValues <- apply(alternativeValues, 1, sum);
    if (!checkValues(fullValues)) {
      ## not ok, retain previous point
      currentValueFunc[currentCriterion,currentIndex] = oldPoint;
    }
    if (i %% thinning == 0 && recAllV) {
      index <- as.integer(i / thinning) - 1;
      start <- (index*n)+1;
      end <- (index+1)*n;
      allValueFuncs[start:end,] <<- currentValueFunc;
    }
    if (i %% (thinning*100) == 0) {
      print (i / thinning);
    }
  }
}



## burnin
generateInitialValueFunctions();
message("Burnin of ", burninSize);
gibbsSample(burninSize, 1, TRUE);


# plot burnins
message("Plotting burnins");
par(mfrow=c(2,2))
for (i in 1:n) {
  f <- allValueFuncs[seq(from=i,to=burninSize*n,by=n),1:nrPoints[i]];
  plot(apply(f, 1, sum), col=rainbow(n)[i], type="l", ylim=c(0,12), main=colnames(data)[i+1]);
}

## 10000 iters
message("Generate ",mainIters, " value functions with ", thinSize," thinning");
gibbsSample(mainIters, thinSize, TRUE);

## Compute rank acceptabilities and pair-wise winning indices
message("Computing rank acceptabilities and pair-wise winning indices");
racs <- matrix(0, m, m);
pwwi <- matrix(0, m, m);
for (i in 0:(mainIters-1)) {
  startIndex <- (i*n) + 1;
  endIndex <- (i+1)*n;

  currentValueFunc <<- allValueFuncs[startIndex:endIndex,];

  assignAlternativeValues();

  fullValues <- apply(alternativeValues, 1, sum);
  ## Sanity check
  stopifnot(checkValues(fullValues));
  ranks <- sort.int(fullValues, decreasing=TRUE, index.return=TRUE)$ix;
  ## Sanity check
  stopifnot(length(ranks) == 20);
  for (j in 1:length(ranks)) {
    thisalt <- ranks[j];
    racs[thisalt, j] <- racs[thisalt, j] + 1;
    # row, col = alt in row wins alt in col
    if (j < length(ranks)) {
      for (k in (j+1):length(ranks)) {
        pwwi[thisalt, ranks[k]] <- pwwi[thisalt, ranks[k]] + 1;
      }
    }
  }
  if (i %% 1000 == 0) {
    message(i);
  }
}
racs <- racs * 100 / mainIters;
pwwi <- pwwi * 100 / mainIters;

## make it fancy
rownames(racs) <- data[,1]
colnames(racs) <- as.character(1:20)
rownames(pwwi) <- data[,1]
colnames(pwwi) <- data[,1]

# compute min and max ranks
minmax <- apply(racs,1, function(x) min(which(x>0)))
minmax <- cbind(minmax, apply(racs,1, function(x) max(which(x>0))))
colnames(minmax) <- c("Min", "Max")

# compute min and max chracteristic points
message("Computing min and max of the characteristic points");
cmins = matrix(NaN, nrow=4, ncol=20);
cmaxs = matrix(NaN, nrow=4, ncol=20);
for (i in 1:n) {
  f <- allValueFuncs[seq(from=i,to=nrow(allValueFuncs),by=n),];
  cmins[i,] <- apply(f, 2, min);
  cmaxs[i,] <- apply(f, 2, max);
}
rownames(cmins) <- colnames(data[2:5])
rownames(cmaxs) <- colnames(data[2:5])
colnames(cmins) <- as.character(1:20)
colnames(cmaxs) <- as.character(1:20)

#write.csv(pwwi, "pwwi.csv");
#write.csv(cmins, "char-mins.csv");
#write.csv(cmaxs, "char-maxs.csv");
#write.csv(racs, "racs.csv");
#pdf("racs.pdf");
#barplot(t(racs), legend=TRUE, col=heat.colors(20));

