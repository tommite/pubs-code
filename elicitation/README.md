This directory includes the tests for:

G. van Valkenhoef and T. Tervonen: Entropy-optimal weight constraint elicitation with additive multi-attribute utility models

The tests need the following R packages:

'hitandrun' with the shake and bake modifications, from https://github.com/tommite/hitandrun
'R.basic' from contriburl="http://www.braju.com/R/repos/"
'MASS' (in CRAN)
'smaa' (in CRAN)
'plyr' (in CRAN)

The tests are made for execution on cluster with PBS scheduling system, by running the Makefile
schedule* targets. You can easily execute the tests without massively parallel computation
architecture as well, but be warned; unless you're a researcher 30 years in the future, the tests
might take a few years to complete! After producing the test results in data/, the figures are made
with make all (requires existence of a directory ../graphics, where all the figures will be built in).

In Rotterdam, April 2014
Tommi Tervonen