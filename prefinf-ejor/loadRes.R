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

pdf('res10.pdf', width=10, height=5)
plotres(10)
dev.off()

pdf('res20.pdf', width=10, height=5)
plotres(20)
dev.off()

pdf('res50.pdf', width=10, height=5)
plotres(50)
dev.off()
