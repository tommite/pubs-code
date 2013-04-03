ncrit <- 5

loadres <- function(nalts) {
  mat <- c()
  for (npref in seq(2, 40, 2)) {
    for (inst in 1:10) {
      load(paste('res/fastror', nalts, ncrit, npref, inst, sep='-'))
      mat <- rbind(mat, c(nalts, ncrit, npref, row))
    }
  }
  colnames(mat) <- c('nalts', 'ncrit', 'npref', 'diff', 'dom', 'th1', 'lemma1', 'restarts')
  return(mat)
}

plotres <- function(nalts) {
  res <- loadres(nalts)
  x = res[,'npref']
  y1 = res[,'dom']
  y2 = res[,'lemma1']
  plot(x, y1, type='p', pch=1, col='black',
  #ylab='Number of inferences', 
  #xlab='Number of preference statements', 
  xlab='',
  main='',
  ylab='',
  #main=paste('Test results: ', nalts, 'alternatives and 5 criteria'), 
  ylim=c(min(y1, y2), max(y1, y2)))
  points(x, y2, pch=2, col='black')
#  legend('topright',
#         c('with transitivity', 'inferred negative through the exclusion lemma'),
#         col=c('black', 'black'),
#         pch=c(3, 4)
#         )
}

compAllStats <- function(res, stat) {
  means <- sapply(1:20, function(x) mean(res[(((x-1)*10)+1):(x*10), stat]))
  sds <- sapply(1:20, function(x) sd(res[(((x-1)*10)+1):(x*10), stat]))

  rbind(means, sds)
}

## Make full plots (not used in the article)

pdf('res10.pdf', width=10, height=5)
plotres(10)
dev.off()

pdf('res20.pdf', width=10, height=5)
plotres(20)
dev.off()

pdf('res50.pdf', width=10, height=5)
plotres(50)
dev.off()

## Compute standard means and standard deviations (not used in the article)
res10 <- loadres(10)
res20 <- loadres(20)
res50 <- loadres(50)

lemma1res10 <- compAllStats(res10, 'lemma1')
lemma1res20 <- compAllStats(res20, 'lemma1')
lemma1res50 <- compAllStats(res50, 'lemma1')

domres10 <- compAllStats(res10, 'dom')
domres20 <- compAllStats(res20, 'dom')
domres50 <- compAllStats(res50, 'dom')

## Make boxplots (used in the article)
labels <- 2 * sort(rep(seq(1, 20), 10))
res10aug <- cbind(res10, labels)
res20aug <- cbind(res20, labels)
res50aug <- cbind(res50, labels)

pdf('res10lemma.pdf')
boxplot(lemma1 ~ labels, data=res10aug, xlab='Preference statements', ylab='Impossible inferences due to Lemma 1')
dev.off()
pdf('res20lemma.pdf')
boxplot(lemma1 ~ labels, data=res20aug, xlab='Preference statements', ylab='Impossible inferences due to Lemma 1')
dev.off()
pdf('res50lemma.pdf')
boxplot(lemma1 ~ labels, data=res50aug, xlab='Preference statements', ylab='Impossible inferences due to Lemma 1')
dev.off()

pdf('res10trans.pdf')
boxplot(dom ~ labels, data=res10aug, xlab='Preference statements', ylab='Transitive inferences')
dev.off()
pdf('res20trans.pdf')
boxplot(dom ~ labels, data=res20aug, xlab='Preference statements', ylab='Transitive inferences')
dev.off()
pdf('res50trans.pdf')
boxplot(dom ~ labels, data=res50aug, xlab='Preference statements', ylab='Transitive inferences')
dev.off()
