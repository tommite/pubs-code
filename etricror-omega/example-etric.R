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

th <- matrix(c(0, 0.1, 0, 0.2, TRUE,
               1, 0, 2, 0, FALSE), byrow=TRUE, ncol=5)

pos <- etricror(perfs, profs, assignments, FALSE, th)
nec <- etricror(perfs, profs, assignments, TRUE, th)
print(pos)
print(nec)
            
