source('etricror.R')

perfs <- matrix(c(
                  400,	2,
                  250,	3,
                  100,  7
                  ), byrow=TRUE, ncol=2)

rownames(perfs) <- c("a1", "a2", "a3")

profs <- matrix(c(
                  200,	5.5,
                  225,	3.2
                  ), byrow=TRUE, ncol=2)
rownames(profs) <- c("b1", "b2")

assignments <- matrix(c(1, 2,
                        3, 1), byrow=TRUE, ncol=2)

phi <- c(buildPhi(q=0, qMult=0.1, p=0, pMult=0.2, ascending=TRUE),
         buildPhi(q=1, p=2, ascending=FALSE))

pos <- etricror(performances=perfs, profiles=profs, assignments, necessary=FALSE, phi)
nec <- etricror(performances=perfs, profiles=profs, assignments, necessary=TRUE, phi)
print(pos)
print(nec)

