source('~/git/derivmkts/R/compound.R')

library(testthat)
## we will generally use these parameters:
#kseq <- rep(c(35, 40, 45), times=3)
#Hseq <- rep(c(35, 40, 45), each=3)
#s <- 40; v <- 0.30; r <- 0.08;  tt <- 0.25; d <- 0.02

## The saved file has the parameter values used in the test along with
## the uppercased names of the functions, which have been downcased
## for this version file
#load('option_testvalues.Rdata')
load('~/git/derivmkts/tests/testthat/option_testvalues.Rdata')
## for each function name, we will generate results believed correct
## from options.R. Then we will test against compound.R


for (i in compoundtestfns) {
    compoundfns <- tolower(i)
    test_that(paste(compoundfns, 'works'), {
        correct <- compoundvals[, i]
        unknown <- do.call(compoundfns,
                           list(s=s, kuo=kuo, kco=kco, v=v, r=r,
                                t1=t1, t2=t2, d=d,
                                returnscritical=TRUE))
        expect_equivalent(correct, unknown)
    }
    )
    print(paste(i, 'okay'))
}

