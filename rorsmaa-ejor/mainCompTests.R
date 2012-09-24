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
mainIters <- 10000;
alternativeValues <- matrix(nrow=m, ncol=n);

## generate index vectors
ind <- c();
for (i in 1:n) {
  uelems <- sort(unique(meas[,i]));
  thisind <- match (meas[,i], uelems);
  ind <- cbind(ind, thisind);
}
colnames(ind) <- colnames(data)[2:5]

## compute normalized values
normValues <- apply(meas, 2, function(x) (x-min(x)) / (max(x)-min(x)));
