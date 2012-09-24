source('mainCompTests.R');

sampleWeights <- function(n) {
  t <- c(0, sort(runif(n-1)), 1);
  t[2:(n+1)] - t[1:n]
}

# Generate a random value function with n pieces
# For each row, the first column is the x-value, and second the y
generateValueFunc <- function(n) {
  vf <- matrix(nrow=n+1, ncol=2);
  ys = runif(n-1);
  vf[1,1] = 0;
  vf[1,2] = 0;
  vf[n+1,1] = 1;
  vf[n+1,2] = 1;
  if (n > 1) {
    vf[2:n,1] = sort(runif(n-1));
    vf[2:n,2] = sort(runif(n-1));
  }
  vf;
}


# Generate a random value function with n pieces
# For each row, the first column is the x-value, and second the y
# UTA value functions have the characteristic points always equally distributed
generateValueFuncUTA <- function(n) {
  vf <- matrix(nrow=n+1, ncol=2);
  ys = runif(n-1);
  vf[1,1] = 0;
  vf[1,2] = 0;
  vf[n+1,1] = 1;
  vf[n+1,2] = 1;
  if (n > 1) {
    for (i in 2:n) {
      vf[i,1] <- (i-1)/n;
    }
    vf[2:n,2] = sort(runif(n-1));    
  }
  vf;
}


# Apply the value function vf, return vf(val)
applyValueFunction <- function(vf, val) {
  ret <- 0;
  for (i in 2:nrow(vf)) {
    if (val <= vf[i,1]) {
      ivalSize <- (vf[i,2]-vf[i-1,2]);
      ivalStart <- vf[i-1,2]
      offSet <- (val - vf[i-1,1]) / (vf[i,1] - vf[i-1,1]);
      ret <- ivalStart + (ivalSize * offSet);
      break;
    }
  }
  ret;
}
  
rejracs <- matrix(0, m, m);
rejpwwi <- matrix(0, m, m);
vals <- rep(0, m);
##### REJECTION
message("10000 iterations with rejection sampling");
for (i in 0:(mainIters-1)) {
  found <- FALSE;
  w <- c();
  vfStore = c();
  while (!found) {
    vals[] = 0;
    w <- sampleWeights(n);
    for (j in 1:n) {
      vf <- generateValueFuncUTA(2);
      vfStore[[j]] = vf;
      for (k in 1:m) {
        vals[k] <- vals[k] + applyValueFunction(vf, normValues[k,j]) * w[j];
      }
    }   
    found <- checkValues(vals);
  }
  ranks <- sort.int(vals, decreasing=TRUE, index.return=TRUE)$ix;

  if (vals[5] > vals[3]) {
    message("NET > SPA:");
    for (x in 1:n) {
      vfStore[[x]][,2] = vfStore[[x]][,2] * w[x];
    }
    print(vfStore);
    stop();
  }

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
  if (i %% 100 == 0) {
    message(i);
  }
}
rejracs <- rejracs * 100 / mainIters;
rejpwwi <- rejpwwi * 100 / mainIters;
rownames(rejracs) <- data[,1];
colnames(rejracs) <- as.character(1:20);
rownames(rejpwwi) <- data[,1];
colnames(rejpwwi) <- data[,1];

# compute min and max ranks
minmax <- apply(rejracs,1, function(x) min(which(x>0)))
minmax <- cbind(minmax, apply(rejracs,1, function(x) max(which(x>0))))
colnames(minmax) <- c("Min", "Max")

##### END REJECTION

write.csv(rejpwwi, "2piece-pwwi.csv");
write.csv(rejracs, "2piece-racs.csv");
#pdf("2piece-racs.pdf");
#barplot(t(rejracs), legend=TRUE, col=heat.colors(20));
#dev.off();
