options( java.parameters = c("-Xmx2g") )

### Read arguments
args <- commandArgs(trailingOnly = TRUE)
nalts <- as.integer(args[1])
ncrit <- as.integer(args[2])
npref <- as.integer(args[3])
instance <- as.integer(args[4])
stopifnot(length(args) ==  4)
rm(args)
###

set.seed(1911 + (nalts * 71) + (ncrit * 23) + (npref * 7) + instance)
source('funcs.R')

res <- performTest(nalts, ncrit, npref)
diffCount <- sum(res$uta - (res$fast > 0))
dominanceCount <- sum(res$fast == 2)
th1Count <- sum(res$fast == 3)
lemma1Count <- sum(res$fast == -1)

row <- c(diffCount, dominanceCount, th1Count, lemma1Count, res$restarts)

save('row', file=paste('fastror', nalts, ncrit, npref, instance, sep='-'))
