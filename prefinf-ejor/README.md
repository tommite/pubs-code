These are the tests for:

R. Split, T. Tervonen: Preference inference with general additive value models and holistic pair-wise statements. European Journal of Operational Research 232(3):607-612, 2014. DOI: 10.1016/j.ejor.2013.07.036.

System requirements:
- Python (2.6+)
- Java
- R
- R package 'ror' and its required dependencies (available from CRAN)

The tests are designed to be run in two phases. The first phase is to be run in parallel on a cluster with PBS, 
execute with 'qsub -d . fastrorTest.pbs'. This will produce the results.

The results can be merged with the script loadRes.R.
