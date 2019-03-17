## Clear everything so we save only what we need
rm(list=ls())
source('~/inc/R/options.R')
setwd('~/git/derivmkts')

## This program generates test values for different option pricing functions
##
## Here are the values we use:
kseq0 <- c(35, 40, 45)
kseq <- rep(c(35, 40, 45), times=3)
Hseq <- rep(c(35, 40, 45), each=3)
s <- 40; k <- 40; v <- 0.30; r <- 0.08;  tt <- 2; d <- 0.05; H=43.1
sseq <- c(10, 40, 250)
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

Hseq2 <- 36:44
barriertestfns2 <- c('DRDeferred', 'URDeferred', 'UR', 'DR')
barriervals2 <- data.frame(Hvals=Hseq2)
#barriervals2 <- data.frame()
for (i in barriertestfns2) {
    tmp <- do.call(i, list(s=s, v=v, r=r, tt=tt, d=d, H=Hseq2))
    barriervals2[, i] <- tmp
}

barriertestfns3 <- c('UR', 'DR')
barriervals3 <- data.frame(Hvals=Hseq2)
for (i in barriertestfns3) {
    for (j in 1:length(Hseq2)) {
        tmp <- do.call(i, list(s=s, v=v, r=r, tt=1e10, d=d, H=Hseq2[j]))
        barriervals3[j, i] <- tmp
    }
}

sseq4 <- rep(c(10, 40, 200), each=3)
kseq4 <- rep(c(35, 40, 45), 3)
barriertestfns4 <- c('callPerpetual', 'putPerpetual')
barriervals4 <- data.frame(kvals=kseq4, svals=sseq4)
for (i in barriertestfns4) {
    for (j in 1:length(kseq4)) {
        tmp <- do.call(i, list(sseq4[j], kseq4[j], v=v, r=r, d=d))[1]
        barriervals4[j, i] <- tmp
    }
}


############################################################
## Asian tests
############################################################
Nseq <- c(1, 5, 20)
asiantestfns <- c('')
asianvals <- data.frame(kvals=kseq, Nvals=Nseq)

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
## Merton jump tests
############################################################
## Only test call values for each function
lambda <- 0.5; alphaj <- -0.15; vj <- 0.20;
jumpfns <- c('CashCallJump', 'AssetCallJump', 'MertonJump')
jumpvals <- data.frame(kvals=kseq0)
for (i in jumpfns[1:2]) {
    tmp <- do.call(i, list(s=s, k=kseq0, v=v, r=r, tt=tt, d=d,
                           lambda=lambda, alphaj=alphaj, vj=vj))
    jumpvals[i] <- tmp
}
i <- jumpfns[3]
tmp <- do.call(i, list(s=s, k=kseq0, v=v, r=r, tt=tt, d=d,
                       lambda=lambda, alphaj=alphaj, vj=vj))
jumpvals[i] <- tmp[grep('Call', names(tmp))]

############################################################
## Greeks tests
############################################################

## Note that I have reversed the names: "greeks" in the package now
## uses the function call method and hence corresponds to "Greeks2" in
## options.R, while "greeks2" uses the list and hence corresponds to
## "Greeks"

greeksvals <- list()
greeksinputs <- list(s=s, k=kseq, v=v, r=r, tt=tt, d=d)
greeksinputsnott <- list(s=s, k=k, v=v, r=r, d=d)
greeksinputsH <- list(s=s, k=kseq, v=v, r=r, tt=tt, d=d, H=Hseq2)
greeksvals[['bscall']] <- Greeks(bscall, greeksinputs)
greeksvals[['assetuicall']] <- Greeks(AssetUICall, greeksinputsH)
## for this next test use new version of function and greeks that can
## handle absence of tt input
greeksvals[['callperpetual']] <- greeks2(callperpetual, greeksinputsnott)
greeksvals2 <- list()
greeksvals2[['bscall']] <- Greeks2(bscall(s=s, k=kseq, v=v,
                                              r=r, tt=tt, d=d))
greeksvals2[['assetuicall']] <- Greeks2(AssetUICall(s=s, k=kseq, v=v,
                                              r=r, tt=tt, d=d, H=Hseq2))
bgreeksvals2[['bscalltidy']] <- Greeks2tidy(bscall(s=s, k=kseq, v=v,
                                              r=r, tt=tt, d=d), complete=TRUE)
greeksvals2[['bscalltidylong']] <- Greeks2tidy(bscall(s=s, k=kseq0, v=v,
                                                      r=r, tt=tt, d=d),
                                               long=TRUE, complete=TRUE)
greeksvals2[['assetuicalltidy']] <- Greeks2tidy(AssetUICall(s=s, k=kseq, v=v,
                                              r=r, tt=tt, d=d, H=Hseq2),
                                              complete=TRUE)
binomvalsEurC <- BinomSimple(s=s, k=k, v=v, r=r, tt=tt, d=d, nstep,
                             putOpt=FALSE, American=FALSE)
binomvalsEurP <- BinomSimple(s=s, k=k, v=v, r=r, tt=tt, d=d,
                             nstep, putOpt=TRUE, American=FALSE)
binomvalsAmC <- BinomSimple(s=s, k=k, v=v, r=r, tt=tt, d=d,
                             nstep, putOpt=FALSE, American=TRUE)
binomvalsAmP <- BinomSimple(s=s, k=k, v=v, r=r, tt=tt, d=d,
                             nstep, putOpt=TRUE, American=TRUE)

############################################################
## Compound option tests
############################################################
library(mnormt)
compoundtestfns <- c('CallOnCall', 'CallOnPut', 'PutOnCall',
                     'PutOnPut')
kuo <- 41; kco <- 1.75; t1 <- 0.38; t2 <- 0.75
compoundvals <- data.frame(row.names=c('price', 'scritical'))
for (i in compoundtestfns) {
    tmp <- do.call(i, list(s=s, k=kuo, x=kco, v=v, r=r,
                           t1=t1, t2=t2, d=d))
    compoundvals[, i] <- tmp
}


rm(duration)

keeplist <- c('barriervals', 'barriertestfns',
              'barriervals2', 'barriertestfns2',
              'barriervals3', 'barriertestfns3',
              'barriervals4', 'barriertestfns4',
              'bstestfns', 'bsvals',
              'imptestfns', 'bsimpvals',
              'greeksvals', 'greeksvals2',
              'greeksinputs', 'greeksinputsH',
              'greeksinputsnott',
              's', 'k', 'v', 'r', 'tt', 'd', 'H',
              'kseq0', 'kseq', 'Hseq', 'Hseq2', 'kseqbs',
              'kseq4', 'sseq4',
              'prices', 'jumpfns', 'jumpvals',
              'lambda', 'alphaj', 'vj',
              'nstep', 'binomvalsEurC', 'binomvalsEurP',
              'binomvalsAmC', 'binomvalsAmP',
              'kuo', 'kco', 't1', 't2', 'compoundtestfns',
              'compoundvals'
              )

save(list=keeplist,
     file='~/git/derivmkts/tests/testthat/option_testvalues.Rdata')


