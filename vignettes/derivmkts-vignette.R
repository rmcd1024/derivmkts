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
library(knitr)
library(derivmkts)
library(markdown)

opts_chunk$set(collapse=TRUE)

## ------------------------------------------------------------------------
s <- 100; k <- 100; r <- 0.08; v <- 0.30; tt <- 2; d <- 0
bscall(s, k, v, r, tt, d)
bsput(s, k, v, r, tt, d)
bsput(s, c(95, 100, 105), v, r, tt, d)


## ----allgreeks, fig.cap='All option Greeks, plotted using bsopt', fig.width=5.5, fig.height=5.5----
## Plot all Greeks
k <- 100; r <- 0.08; v <- 0.30; tt <- 2; d <- 0
S <- seq(.5, 250, by=.5)
x <- bsopt(S, k, v, r, tt, d)
par(mfrow=c(4, 4))  ## create a 4x4 plot
par(mar=c(2,2,2,2))
for (i in c('Call', 'Put')) {
    for (j in rownames(x[[i]])) {  ## loop over greeks
        plot(S, x[[i]][j, ], main=paste(i, j), ylab=j, type='l')
    }
}


## ------------------------------------------------------------------------
s <- 100; k <- 100; r <- 0.08; v <- 0.30; tt <- 2; d <- 0.03
binomopt(s, k, v, r, tt, d, nstep=4)
binomopt(s, k, v, r, tt, d, nstep=4, returnparams=TRUE)
binomopt(s, k, v, r, tt, d, nstep=4, putopt=TRUE)
binomopt(s, k, v, r, tt, d, nstep=4, returntrees=TRUE, putopt=TRUE)

## ------------------------------------------------------------------------
H <- 105
uicall(c(95, 100, 105), k, v, r, tt, d, H)
bscall(c(95, 100, 105), k, v, r, tt, d)

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
bullspread <- function(s, k1, k2, v, r, tt, d) {
    bscall(s, k1, v, r, tt, d) - bscall(s, k2, v, r, tt, d)
}
greeks(bullspread(40, 40, 45, .3, .08, 1, 0))

## ------------------------------------------------------------------------
sseq <- seq(1, 100, by=0.5)
x <- greeks(bullspread(sseq, 40, 45, .3, .08, 1, 0))
plot(sseq, x['Gamma',], type='l')

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

