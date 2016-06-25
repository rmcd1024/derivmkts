## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------
library(highlight)
library(knitr)
##homedir <- '/home/rmcd/tex/d67/Rtutorial/'
options(digits=4)
figsize <- 4.5
opts_chunk$set(size='footnotesize', prompt=FALSE, comment=NA,
               fig.align='center', fig.width = figsize,
               fig.height=figsize, out.width='3.75in')

#              , fig.width=4.5*3.75/3.25, fig.height=4.5,
#              , out.width='3.75in', out.height='3.25in'
#               )
opts_knit$set(highlight = TRUE,
              eval.after='fig.cap',
              prompt=TRUE,
              renderer=renderer_latex(document=FALSE),
              size='footnotesize')

## ----echo=FALSE----------------------------------------------------------
library(derivmkts)
library(markdown)

opts_chunk$set(collapse=TRUE)

## ------------------------------------------------------------------------
s <- 100; k <- 100; r <- 0.08; v <- 0.30; tt <- 2; d <- 0
bscall(s, k, v, r, tt, d)
bsput(s, c(95, 100, 105), v, r, tt, d)


## ------------------------------------------------------------------------
H <- 115
bscall(s, c(80, 100, 120), v, r, tt, d)
uicall(s, c(80, 100, 120), v, r, tt, d, H)
bsput(s, c(80, 100, 120), v, r, tt, d)
uoput(s, c(80, 100, 120), v, r, tt, d, H)

## ------------------------------------------------------------------------
s <- 100; k <- 100; r <- 0.08; v <- 0.30; tt <- 2; d <- 0.04
callperpetual(s, c(95, 100, 105), v, r, d)
callperpetual(s, c(95, 100, 105), v, r, d, showbarrier=TRUE)


## ------------------------------------------------------------------------
H <- 105
greeks(uicall(s, k, v, r, tt, d, H))


## ------------------------------------------------------------------------
powercontract <- function(s, v, r, tt, d, a) {
    price <- exp(-r*tt)*s^a* exp((a*(r-d) + 1/2*a*(a-1)*v^2)*tt)
}

## ------------------------------------------------------------------------
greeks(powercontract(s=40, v=.08, r=0.08, tt=0.25, d=0, a=2))

## ------------------------------------------------------------------------
bullspread <- function(s, v, r, tt, d, k1, k2) {
    bscall(s, k1, v, r, tt, d) - bscall(s, k2, v, r, tt, d)
}
greeks(bullspread(39:41, .3, .08, 1, 0, k1=40, k2=45))


## ----bullgamma, fig.cap='Gamma for a 40-45 bull spread.'-----------------
sseq <- seq(1, 100, by=0.5)
x <- greeks(bullspread(sseq, .3, .08, 1, 0, k1=40, k2=45))
plot(sseq, x['Gamma',], type='l')


## ----allgreeks, fig.cap='All option Greeks, computed using the greeks() function', fig.width=7.5, fig.height=6.5----
k <- 100; r <- 0.08; v <- 0.30; tt <- 2; d <- 0
S <- seq(.5, 250, by=.5)
Call <- greeks(bscall(S, k, v, r, tt, d))
Put <- greeks(bsput(S, k, v, r, tt, d))
y <- list(Call=Call, Put=Put)
par(mfrow=c(4, 4))  ## create a 4x4 plot
par(mar=c(2,2,2,2))
for (i in names(y)) {
    for (j in rownames(y[[i]])) {  ## loop over greeks
        plot(S, y[[i]][j, ], main=paste(i, j), ylab=j, type='l')
    }
}


## ------------------------------------------------------------------------
s <- 100; k <- 100; r <- 0.08; v <- 0.30; tt <- 2; d <- 0.03
binomopt(s, k, v, r, tt, d, nstep=3)
binomopt(s, k, v, r, tt, d, nstep=3, returnparams=TRUE)
binomopt(s, k, v, r, tt, d, nstep=3, putopt=TRUE)
binomopt(s, k, v, r, tt, d, nstep=3, returntrees=TRUE, putopt=TRUE)

## ------------------------------------------------------------------------
s <- 100; k <- 100; r <- 0.08; v <- 0.30; tt <- 2; d <- 0.03; m <- 3
geomavgpricecall(s, 98:102, v, r, tt, d, m)
geomavgpricecall(s, 98:102, v, r, tt, d, m, cont=TRUE)
geomavgstrikecall(s, k, v, r, tt, d, m)


## ------------------------------------------------------------------------
arithasianmc(s, k, v, r, tt, d, 3, numsim=5000, printsds=TRUE)


## ------------------------------------------------------------------------
arithavgpricecv(s, k, v, r, tt, d, 3, numsim=5000)


## ------------------------------------------------------------------------
mertonjump(s, k, v, r, tt, d, lambda=0.5, alphaj=-0.2, vj=0.3)
c(bscall(s, k, v, r, tt, d), bsput(s, k, v, r, tt, d))

## ------------------------------------------------------------------------
coupon <- 8; mat <- 20; yield <- 0.06; principal <- 100; 
modified <- FALSE; freq <- 2
price <- bondpv(coupon, mat, yield, principal, freq)
price
bondyield(price, coupon, mat, principal, freq)
duration(price, coupon, mat, principal, freq, modified)
convexity(price, coupon, mat, principal, freq)


## ----quincunx, fig.cap='Output from the Quincunx function'---------------
par(mar=c(2,2,2,2))
quincunx(n=20, numballs=200, delay=0, probright=0.7)

## ----binomplot1, fig.cap='Basic option plot showing stock prices and nodes at which the option is exercised.\\label{fig:binomplot1}'----
binomplot(s, k, v, r, tt, d, nstep=6, american=TRUE, putopt=TRUE)


## ----binomplot2, fig.cap='Same plot as Figure \\ref{fig:binomplot1} except that values and arrows are added to the plot.\\label{fig:binomplot2}'----
binomplot(s, k, v, r, tt, d, nstep=6, american=TRUE, putopt=TRUE,
    plotvalues=TRUE, plotarrows=TRUE)

## ----binomplot3, fig.cap="Binomial plot when nstep is 40.\\label{fig:binomplot3}"----
d <- 0.06
binomplot(s, k, v, r, tt, d, nstep=40, american=TRUE)

## ----binomplot4, fig.cap="Binomial plot when nstep is 40 using the argument ylimval to focus on a subset.\\label{fig:binomplot4}"----
d <- 0.06
binomplot(s, k, v, r, tt, d, nstep=40, american=TRUE, ylimval=c(75, 225))

## ------------------------------------------------------------------------
f = function(a, b, k) a*b + k
f(3, 5, 1) 
f(1:5, 5, 1) 
f(1:6, 1:2, 1) 

## ------------------------------------------------------------------------
cond1 <- function(a, b, k) {
    if (a > b) {
        a*b + k
    } else {
        k
    }
}
cond1(5, 3, 1)
cond1(5, 7, 1)
cond1(3:7, 5, 1)


## ------------------------------------------------------------------------
cond2 <- function(a, b, k) {
    ifelse(a > b, 
           a*b + k,  
           k
           )
}
cond2(5, 3, 1)
cond2(5, 7, 1)
cond2(3:7, 5, 1)


## ------------------------------------------------------------------------
cond2(5, 7, 1:3)

## ------------------------------------------------------------------------
cond2b <- function(a, b, k) {
    agtb <- (a > b)
    agtb*(a*b + k) + (1-agtb)*k
}

cond2b(5, 3, 1)
cond2b(5, 7, 1)
cond2b(3:7, 5, 1)
cond2b(5, 7, 1:3)


## ------------------------------------------------------------------------
cond2c <- function(a, b, k) {
    tmp <- data.frame(a, b, k)
    for (i in names(tmp)) assign(i, tmp[[i]])
    ifelse(a > b, 
           a*b + k,  
           k
           )
}

cond2c(5, 3, 1)
cond2c(5, 7, 1)
cond2c(3:7, 5, 1)
cond2c(5, 7, 1:3)


## ------------------------------------------------------------------------
vectorizeinputs <- function(e) {
    ## e is the result of match.call() in the calling function
    e[[1]] <- NULL
    e <- as.data.frame(as.list(e))
    for (i in names(e)) assign(i, eval(e[[i]]),
                               envir=parent.frame())
}

## ------------------------------------------------------------------------
cond3 <- function(a, b, k) { 
    vectorizeinputs(match.call())
    ifelse(a > b, a*b + k, k)
}
cond3(5, 7, 1:3)
cond3(3:7, 5, 1)
cond3(3:7, 5, 1:5)
cond3(k=1:5, 3:7, 5)


## ------------------------------------------------------------------------
cond4 <- function(a, b, k, multby2=TRUE) {
    vectorizeinputs(match.call())
    ifelse(multby2, 
           2*(a*b + k), 
           a*b + k
           )
}
cond4(5, 7, 1:3)
cond4(3:7, 5, 1)
cond4(3:7, 5, 1:5)
cond4(k=1:5, 3:7, 5)


## ------------------------------------------------------------------------
vectorizeinputs2 <- function(e) {
    funcname <- e[[1]]
    fvals <- formals(eval(funcname))
    fnames <- names(fvals)
    e[[1]] <- NULL
    e <- as.data.frame(as.list(e))
    implicit <- setdiff(fnames, names(e))
    if (length(implicit) > 0) e <- data.frame(e, fvals[implicit])
    for (i in names(e)) assign(i, eval(e[[i]]),
                               envir=parent.frame())
}

cond5 <- function(a, b, k, multby2=TRUE, altmult=1) {
    vectorizeinputs2(match.call())
    ifelse(multby2, 
           2*(a*b + k), 
           altmult*(a*b + k)
           )
}
cond5(5, 7, 1:3)
cond5(3:7, 5, 1)
cond5(3:7, 5, 1:5)
cond5(k=1:5, 3:7, 5)
cond5(k=1:5, 3:7, 5, multby2=FALSE)
cond5(k=1:5, 3:7, 5, multby2=FALSE, altmult=5)


