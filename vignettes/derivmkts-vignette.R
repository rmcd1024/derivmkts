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
callperpetual(s, c(95, 100, 105), v, r, d, priceonly=FALSE)


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
geomavgprice(s, k, v, r, tt, d, 3)
geomavgstrike(s, k, v, r, tt, d, 3)


