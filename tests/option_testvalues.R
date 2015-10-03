## Clear everything so we save only what we need
rm(list=ls())
source('~/inc/R/options.R')
setwd('~/git/derivmkts')

## This program generates test values for different option pricing functions
##
## Here are the values we use:
kseq <- rep(c(35, 40, 45), times=3)
Hseq <- rep(c(35, 40, 45), each=3)
s <- 40; k <- 40; v <- 0.30; r <- 0.08;  tt <- 2; d <- 0.05; H=43.1
nstep=15

############################################################
## Barrier tests
############################################################
## loop through the follwing functions and populate a data frame where
## each column is a set of test values
barriervals <- data.frame(kvals=kseq, Hvals=Hseq)
barriertestfns <- c('CashDICall', 'AssetDICall', 'CashDOCall',
                    'AssetDOCall', 'CashDIPut', 'AssetDIPut',
                    'CashDOPut', 'AssetDOPut',
                    'CashUICall', 'AssetUICall', 'CashUOCall',
                    'AssetUOCall', 'CashUIPut', 'AssetUIPut',
                    'CashUOPut', 'AssetUOPut',
                    'CallUpIn', 'CallUpOut', 'PutUpIn', 'PutUpOut',
                    'CallDownIn', 'CallDownOut', 'PutDownIn', 'PutDownOut')
for (i in barriertestfns) {
    tmp <- do.call(i, list(s=s, k=kseq, v=v, r=r,
                                    tt=tt, d=d, H=Hseq))
    barriervals[, i] <- tmp
}

## following functions are infinitely-lived, hence no tt argument
Hseq2 <- 36:44
barriertestfns2 <- c('DRDeferred', 'URDeferred', 'UR', 'DR')
barriervals2 <- data.frame(Hvals=Hseq2)
#barriervals2 <- data.frame()
for (i in barriertestfns2) {
    tmp <- do.call(i, list(s=s, v=v, r=r, tt=tt, d=d, H=Hseq2))
    barriervals2[, i] <- tmp
}

############################################################
## Black-Scholes tests
############################################################

kseqbs <- 36:44
bstestfns <- c('bscall', 'bsput', 'AssetCall', 'AssetPut',
               'CashCall', 'CashPut')
bsvals <- data.frame(kvals=kseqbs)
for (i in bstestfns) {
    tmp <- do.call(i, list(s=s, k=kseqbs, v=v, r=r, tt=tt, d=d))
    bsvals[, i] <- tmp
}

############################################################
## Implied volatility and price tests
############################################################

## working, but only one test for each calculations are not vectorized
prices <- 4
imptestfns <- c('bscallimpvol', 'bsputimpvol', 'bscallimpS', 'bsputimpS')
bsimpvals <- data.frame(pricevals=prices)
for (i in imptestfns[1:2]) { 
    tmp <- do.call(i, list(s=s, k=k, r=r, tt=tt, d=d, price=prices))
    bsimpvals[, i] <- tmp
}

for (i in imptestfns[3:4]) {
    tmp <- do.call(i, list(s=s, k=k, v=v, r=r, tt=tt, d=d, price=prices))
    bsimpvals[, i] <- tmp
}

############################################################
## Greeks tests
############################################################

## Note that I have reversed the names: "greeks" in the package now
## uses the function call method and hence corresponds to "Greeks2" in
## options.R, while "greeks2" uses the list and hence corresponds to
## "Greeks"

greeksvals <- list()
greeksinputs <- list(s=s, k=kseq, v=v, r=r, tt=tt, d=d)
greeksinputsH <- list(s=s, k=kseq, v=v, r=r, tt=tt, d=d, H=Hseq2)
greeksvals[['bscall']] <- Greeks(bscall, greeksinputs)
greeksvals[['assetuicall']] <- Greeks(AssetUICall, greeksinputsH)

greeksvals2 <- list()
greeksvals2[['bscall']] <- Greeks2(bscall(s=s, k=kseq, v=v,
                                              r=r, tt=tt, d=d))
greeksvals2[['assetuicall']] <- Greeks2(AssetUICall(s=s, k=kseq, v=v,
                                              r=r, tt=tt, d=d, H=Hseq2))

binomvalsEurC <- BinomSimple(s=s, k=k, v=v, r=r, tt=tt, d=d, nstep,
                             putOpt=FALSE, American=FALSE)
binomvalsEurP <- BinomSimple(s=s, k=k, v=v, r=r, tt=tt, d=d,
                             nstep, putOpt=TRUE, American=FALSE)
binomvalsAmC <- BinomSimple(s=s, k=k, v=v, r=r, tt=tt, d=d,
                             nstep, putOpt=FALSE, American=TRUE)
binomvalsAmP <- BinomSimple(s=s, k=k, v=v, r=r, tt=tt, d=d,
                             nstep, putOpt=TRUE, American=TRUE)

keeplist <- c('barriervals', 'barriertestfns',
              'barriervals2', 'barriertestfns2',
              'bstestfns', 'bsvals',
              'imptestfns', 'bsimpvals',
              'greeksvals', 'greeksvals2',
              'greeksinputs', 'greeksinputsH',
              's', 'k', 'v', 'r', 'tt', 'd', 'H',
              'kseq', 'Hseq', 'Hseq2', 'kseqbs', 'prices',
              'nstep', 'binomvalsEurC', 'binomvalsEurP',
              'binomvalsAmC', 'binomvalsAmP'              
              )

rm(list=setdiff(ls(), keeplist))
save.image(file='~/git/derivmkts/tests/testthat/option_testvalues.Rdata')

