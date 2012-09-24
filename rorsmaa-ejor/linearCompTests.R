source('mainCompTests.R');

sampleWeights <- function(n) {
  t <- c(0, sort(runif(n-1)), 1);
  t[2:(n+1)] - t[1:n]
}

rejracs <- matrix(0, m, m);
rejpwwi <- matrix(0, m, m);
vals <- c();
##### REJECTION
message("10000 iterations with rejection sampling");
for (i in 0:(mainIters-1)) {
  found = FALSE;
  while (!found) {
    w <- sampleWeights(n);
    vals <- normValues %*% w;
    found = checkValues(vals);
  }
  ranks <- sort.int(vals, decreasing=TRUE, index.return=TRUE)$ix;

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

write.csv(rejpwwi, "linear-pwwi.csv");
write.csv(rejracs, "linear-racs.csv");
#pdf("linear-racs.pdf");
#barplot(t(rejracs), legend=TRUE, col=heat.colors(20));
#dev.off();
