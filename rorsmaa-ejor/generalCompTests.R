source('mainCompTests.R');

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

## Compute rank acceptabilities and pair-wise winning indices
message("Computing rank acceptabilities and pair-wise winning indices");
rejracs <- matrix(0, m, m);
rejpwwi <- matrix(0, m, m);
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
    rejracs[thisalt, j] <- rejracs[thisalt, j] + 1;
    # row, col = alt in row wins alt in col
    if (j < length(ranks)) {
      for (k in (j+1):length(ranks)) {
        rejpwwi[thisalt, ranks[k]] <- rejpwwi[thisalt, ranks[k]] + 1;
      }
    }
  }
  if (i %% 1000 == 0) {
    message(i);
  }
}
rejracs <- rejracs * 100 / mainIters;
rejpwwi <- rejpwwi * 100 / mainIters;
rownames(rejracs) <- data[,1]
colnames(rejracs) <- as.character(1:20)
rownames(rejpwwi) <- data[,1]
colnames(rejpwwi) <- data[,1]

# compute min and max ranks
minmax <- apply(rejracs,1, function(x) min(which(x>0)))
minmax <- cbind(minmax, apply(rejracs,1, function(x) max(which(x>0))))
colnames(minmax) <- c("Min", "Max")

# compute min and max chracteristic points
message("Computing min and max of the characteristic points");
rejcmins = matrix(NaN, nrow=4, ncol=20);
rejcmaxs = matrix(NaN, nrow=4, ncol=20);
for (i in 1:n) {
  f <- allValueFuncs[seq(from=i,to=nrow(allValueFuncs),by=n),];
  rejcmins[i,] <- apply(f, 2, min);
  rejcmaxs[i,] <- apply(f, 2, max);
}
rownames(rejcmins) <- colnames(data[2:5])
rownames(rejcmaxs) <- colnames(data[2:5])
colnames(rejcmins) <- as.character(1:20)
colnames(rejcmaxs) <- as.character(1:20)
##### END REJECTION

write.csv(rejracs, "general-racs.csv");
write.csv(rejpwwi, "general-pwwi.csv");
write.csv(rejcmins, "general-char-mins.csv");
write.csv(rejcmaxs, "general-char-maxs.csv");
#pdf("racs.pdf");
barplot(t(rejracs), legend=TRUE, col=heat.colors(20));
dev.off();
