ncrit <- 5

loadres <- function(nalts) {
  mat <- c()
  for (npref in seq(2, 40, 2)) {
    for (inst in 1:10) {
      load(paste('res/fastror', nalts, ncrit, npref, inst, sep='-'))
      mat <- rbind(mat, c(nalts, ncrit, npref, row))
    }
  }
  colnames(mat) <- c('nalts', 'ncrit', 'npref', 'diff', 'trans', 'th1', 'lemma1', 'restarts')
  return(mat)
}

compAllStats <- function(res, stat) {
  means <- sapply(1:20, function(x) mean(res[(((x-1)*10)+1):(x*10), stat]))
  sds <- sapply(1:20, function(x) sd(res[(((x-1)*10)+1):(x*10), stat]))

  rbind(means, sds)
}

## Compute standard means and standard deviations (not used in the article)
res10 <- loadres(10)
res20 <- loadres(20)
res50 <- loadres(50)

lemma1res10 <- compAllStats(res10, 'lemma1')
lemma1res20 <- compAllStats(res20, 'lemma1')
lemma1res50 <- compAllStats(res50, 'lemma1')

transres10 <- compAllStats(res10, 'trans')
transres20 <- compAllStats(res20, 'trans')
transres50 <- compAllStats(res50, 'trans')

## Compute percentages
transPerc10 <- 100 * apply(res10, 1, function(x) {x['trans'] / (x['nalts'] * x['nalts'] - x['nalts'] - x['npref'])})
transPerc20 <- 100 * apply(res20, 1, function(x) {x['trans'] / (x['nalts'] * x['nalts'] - x['nalts'] - x['npref'])})
transPerc50 <- 100 * apply(res50, 1, function(x) {x['trans'] / (x['nalts'] * x['nalts'] - x['nalts'] - x['npref'])})
lemma1Perc10 <- 100 * apply(res10, 1, function(x) {x['lemma1'] / (x['nalts'] * x['nalts'] - x['nalts'] - x['npref'] - x['trans'])})
lemma1Perc20 <- 100 * apply(res20, 1, function(x) {x['lemma1'] / (x['nalts'] * x['nalts'] - x['nalts'] - x['npref'] - x['trans'])})
lemma1Perc50 <- 100 * apply(res50, 1, function(x) {x['lemma1'] / (x['nalts'] * x['nalts'] - x['nalts'] - x['npref'] - x['trans'])})

labels <- 2 * sort(rep(seq(1, 20), 10))
res10aug <- cbind(res10, labels, transPerc10, lemma1Perc10)
res20aug <- cbind(res20, labels, transPerc20, lemma1Perc20)
res50aug <- cbind(res50, labels, transPerc50, lemma1Perc50)

# Change colnames to be correct ones
names <- c('nalts', 'ncrit', 'npref', 'diff', 'trans', 'th1', 'lemma1', 'restarts', 'labels', 'transperc', 'lemma1perc')
colnames(res10aug) <- names
colnames(res20aug) <- names
colnames(res50aug) <- names

## Make percentage boxplot (used in the article)
pdf('plotlemtrans.pdf')
par(mfrow=c(3,2))
boxplot(transperc ~ labels, data=res10aug, xlab='Preference statements, 10 alternatives', ylab='% inferred transitive')
boxplot(lemma1perc ~ labels, data=res10aug, xlab='Preference statements, 10 alternatives', ylab='% remaining Lemma 1')
boxplot(transperc ~ labels, data=res20aug, xlab='Preference statements, 20 alternatives', ylab='% inferred transitive')
boxplot(lemma1perc ~ labels, data=res20aug, xlab='Preference statements, 20 alternatives', ylab='% remaining Lemma 1')
boxplot(transperc ~ labels, data=res50aug, xlab='Preference statements, 50 alternatives', ylab='% inferred transitive')
boxplot(lemma1perc ~ labels, data=res50aug, xlab='Preference statements, 50 alternatives', ylab='% remaining Lemma 1')
dev.off()
