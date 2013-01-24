source('etricror.R')

## Variable index tests
stopifnot(getWjIndex(j=2) == 2)
stopifnot(getCjABIndex(j=1, aInd=2, bInd=1, nAlts=3, nCats=1, nCrit=2) == 4)
stopifnot(getCjBAIndex(j=2, aInd=2, bInd=1, nAlts=3, nCats=1, nCrit=2) == 13)
stopifnot(getCjBhBh1Index(nCrit=2, nAlts=2, nCats=3, j=2, h=1) == 29)
stopifnot(getLambdaIndex(nAlts=1, nCrit=1, nCats=1) == 4)
## Threshold function tests
stopifnot(outranking(1, 2, 1, 0, 2, 0, TRUE) == 1)
stopifnot(outranking(2, 1, 1, 0, 2, 0, TRUE) == 1)
stopifnot(outranking(0, 2, 1, 0, 2, 0, TRUE) == 0)
stopifnot(outranking(0, 1.5, 1, 0, 2, 0, TRUE) == 0.5)
stopifnot(outranking(1, 2, 1, 0, 2, 0, FALSE) == 1)
stopifnot(outranking(2, 1, 1, 0, 2, 0, FALSE) == 1)
stopifnot(outranking(1, 1, 1, 0, 2, 0, FALSE) == 1)
stopifnot(outranking(2, 0, 1, 0, 2, 0, FALSE) == 0)
stopifnot(outranking(0, -1.5, 1, 0, 2, 0, FALSE) == 0.5)
