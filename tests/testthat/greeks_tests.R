source('~/git/derivmkts/R/greeks.R')

library(testthat)
## we will generally use these parameters:
#kseq <- rep(c(35, 40, 45), times=3)
#Hseq <- rep(c(35, 40, 45), each=3)
#s <- 40; v <- 0.30; r <- 0.08;  tt <- 0.25; d <- 0.02

## The saved file has the parameter values used in the test along with
## the uppercased names of the functions, which have been downcased
## for this version file
#load('~/git/derivmkts/tests/testthat/option_testvalues.Rdata')
#load('option_testvalues.Rdata')
load('~/git/derivmkts/tests/testthat/option_testvalues.Rdata')
## for each function name, we will generate results believed correct
## from options.R. Then we will test against barriers.R

test_that('greeks works bscall', {
              correct <- greeksvals[['bscall']]
              unknown <- greeks(bscall, list(s=35:45, k=40, v=.3,
                                             r=0.08, tt=0.25, d=0))
              expect_equal(correct, unknown)
          }
          )
print('bscall greeks okay')

test_that('greeks works assetuicall', {
              correct <- greeksvals[['assetuicall']]
              unknown <- greeks(assetuicall, list(s=35:45, k=40, v=.3,
                                             r=0.08, tt=0.25, d=0, H=43.1))
              expect_equal(correct, unknown, tol=1e-5)
          }
          )
print('assetuicall greeks okay')

test_that('greeks2 works bscall', {
              correct <- greeksvals2[['bscall']]
              unknown <- greeks2(bscall(s=35:45, k=40, v=.3,
                                             r=0.08, tt=0.25, d=0))
              expect_equal(correct, unknown)
          }
          )
print('bscall greeks2 okay')

test_that('greeks2 works assetuicall', {
              correct <- greeksvals2[['assetuicall']]
              unknown <- greeks2(assetuicall(s=35:45, k=40, v=.3,
                                             r=0.08, tt=0.25, d=0, H=43.1))
              expect_equivalent(correct, unknown, tol=1e-1)
          }
          )
print('assetuicall greeks2 okay')

