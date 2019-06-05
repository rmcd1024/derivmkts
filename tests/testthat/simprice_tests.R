source('~/git/derivmkts/R/simprice.R')

library(testthat)
load('~/git/derivmkts/tests/testthat/option_testvalues.Rdata')


test_that(paste('simprice', 'works'), {
          correct = simprice_S
          unknown = do.call(simprice, simprice_params)
          ## For testing purposes, get rid of "asset" column in unknownq
          expect_equivalent(as.data.frame(correct),  unknown[-1])
}
)
print('simprice okay')

##correct = simprice_S
##unknown = do.call(simprice, simprice_params)
##all.equal(as.data.frame(correct),  unknown[-1],  check.names = FALSE )
##head(as.data.frame(correct) )
