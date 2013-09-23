classes <- list (
                 c(22, 24), # china, afghanistan
                 c(14, 16, 18), # hong kong, bangladesh, bhutan
                 c(5, 7, 12), # taiwan, timorleste, malaysia
                 c(4) # japan
                 );

checkValues <- function(v) {
  for (i in 1:(length(classes)-1)) {
    for (j in (i+1):(length(classes))) {
      if (!all(sapply(v[classes[[i]]], '<', v[classes[[j]]]))) {
        return(FALSE);
      }
    }
  }
  return(TRUE);
}


data <- read.table(file="asian-democracy.csv", sep=",", header=TRUE)
n <- length(data)-1; # nr criteria
m <- length(t(data[1])); # nr alternatives
meas <- data.matrix(data[,2:(n+1)])
set.seed(31337);
mainIters <- 10000;
alternativeValues <- matrix(nrow=m, ncol=n);

## generate index vectors
ind <- c();
for (i in 1:n) {
  uelems <- sort(unique(meas[,i]));
  thisind <- match (meas[,i], uelems);
  ind <- cbind(ind, thisind);
}
colnames(ind) <- colnames(data)[2:(n+1)]
rownames(ind) <- data[,1]

## compute normalized values
normValues <- apply(meas, 2, function(x) (x-min(x)) / (max(x)-min(x)))
rownames(normValues) <- data[,1]

## Generates a value function
genValueFunc <- function(n) {
   c(0, sort(runif(n-2)), 1);   
}

sampleWeights <- function(n) {
  t <- c(0, sort(runif(n-1)), 1);
  t[2:(n+1)] - t[1:n]
}

currentValueFunc <- matrix(nrow=n, ncol=m);

## number of characteristic points
nrPoints <- apply(ind, 2, max);

## matrix to store all value functions
allValueFuncs <- matrix(nrow=n*mainIters, ncol=m);

assignAlternativeValues <- function() {
  ## permute the value functions
  for (i in 1:n) {
    alternativeValues[,i] <<- currentValueFunc[i,][ind[,i]]
  }
}

sampleValueFunctions <- function() {
  while(TRUE) {
    w <- sampleWeights(n);
    ## Generate a random one
    for (i in 1:n) {
      v <- genValueFunc(nrPoints[i]);
      v <- v * w[i];
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

##### REJECTION
message("10000 value functions with rejection sampling");
## compute with rejection sampling
for (i in 0:(mainIters-1)) {
  startIndex <- (i*n) + 1;
  endIndex <- (i+1)*n;
  sampleValueFunctions();
  allValueFuncs[startIndex:endIndex,] <- currentValueFunc;
  if (i %% 100 == 0) {
    message(i);
  }
}

getClassIndex <- function(thisVal) {
  curIndex <- 1
  for (k in 1:length(classes)) {
    if (curIndex > 1) {
      if (thisVal < classMins[[k]] && thisVal > classMaxs[[k-1]]) {
        return(curIndex-1)
      }
    }
    if (k == 1) {
      if (thisVal <= classMaxs[[1]]) {
        return (curIndex)
      }
    } else if (k == length(classes)) {
      if (thisVal >= classMaxs[[k]]) {
        return (curIndex)
      }
    } else if (thisVal >= classMins[[k]] && thisVal <= classMaxs[[k]]) {
      return (curIndex)
    }
    curIndex <- curIndex + 2;
  }
}

message("Computing CAI and APOI");
cai <- matrix(0, m, length(classes)*2-1)
apoi <- matrix(0, m, m)

for (i in 0:(mainIters-1)) {
  startIndex <- (i*n) + 1;
  endIndex <- (i+1)*n;

  currentValueFunc <<- allValueFuncs[startIndex:endIndex,];

  assignAlternativeValues();

  fullValues <- apply(alternativeValues, 1, sum);
  ## Sanity check
  stopifnot(checkValues(fullValues));
  ## Find min and max values for the classes
  assignVals <- lapply(classes, function(x) fullValues[x])
  classMins <- lapply(assignVals, min)
  classMaxs <- lapply(assignVals, max)
  
  for (j in 1:m) {
    index <- getClassIndex(fullValues[j])
    cai[j,index] <- cai[j,index] + 1
    ## THEN THE PWI
    for (k in 1:m) {
      otherIndex <- getClassIndex(fullValues[k])
      if (index >= otherIndex) {
        apoi[j,k] <- apoi[j,k] + 1
      }
    }
  }
  if (i %% 1000 == 0) {
    message(i);
  }
}

cai <- cai * 100 / mainIters
apoi <- apoi * 100 / mainIters
rownames(cai) <- data[,1]
colnames(cai) <- seq(1, length(classes), 0.5);
rownames(apoi) <- data[,1]
colnames(apoi) <- data[,1]

## sanity check
stopifnot(all(apply(cai, 1, sum)==100))

write.csv(cai, "cai.csv")
write.csv(apoi, "apoi.csv")
#pdf("racs.pdf");

