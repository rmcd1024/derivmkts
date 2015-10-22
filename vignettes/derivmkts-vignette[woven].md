---
title: "Option Pricing Functions to Accompany *Derivatives Markets*"
author: "Robert McDonald"
date: '2015-10-21'
output:
  html_document:
    toc: yes
    fig_caption: yes
  pdf_document:
    fig_caption: yes
    toc: yes
---


# Introduction

This provides some additional documentation and examples for the R
package `derivmkts`, which is meant as a companion to _Derivatives
Markets_. There are of course other option pricing packages for R, but
this package uses function names corresponding to the usage in the
book.


# Basic Pricing Functions

## Black-Scholes 

The functions `bscall`, `bsput`, and `bsopt` provide basic pricing of
European calls and puts. There are also options with binary payoffs:
cash-or-nothing and asset-or-nothing options. All functions are
vectorized. The function `bsopt` provides option greeks. Here are some
examples:


```r
s <- 100; k <- 100; r <- 0.08; v <- 0.30; tt <- 2; d <- 0
bscall(s, k, v, r, tt, d)
## [1] 24.02467
bsput(s, k, v, r, tt, d)
## [1] 9.239054
bsput(s, c(95, 100, 105), v, r, tt, d)
## [1]  7.487590  9.239054 11.187942
bsopt(s, c(95, 100, 105), v, r, tt, d)$Call
##               bscall_95   bscall_100   bscall_105
## Price      26.533930250 24.024674750 21.712843936
## Delta       0.761196021  0.722155105  0.682341365
## Gamma       0.007307399  0.007904504  0.008402985
## Vega        0.438443163  0.474271156  0.504178762
## Rho         0.991713418  0.963816696  0.930425833
## Theta      -0.019877199 -0.020307673 -0.020556285
## Psi        -1.522392056 -1.444310226 -1.364682749
## Elasticity  2.868764688  3.005889205  3.142570208
```

## Barrier Options

There are pricing functions for a large number of barrier options: 

* down-and-in and down-and-out barrier binary options
* up-and-in and up-and-out barrier binary options
* more standard down- and up- calls and puts, constructed using the barrier binary options

Naming for the barrier options generally follows the convention

`[u|d][i|o][call|put]`

which means that the option is "up" or "down", "in" or "out", and a
call or put.^[The naming convention here differs from that in
_Derivatives Markets_, in which names are `callupin`, `callupout`,
etc. For consistency, but possibly at the cost of confusion, I have
made both names are available for these functions.]  An up-and-in
call, for example, would be denoted by `uicall`. For binary options,
we add the underlying, which is either the asset or \$1:
cash:

`[asset|cash][u|d][i|o][call|put]`


```r
H <- 105
uicall(c(95, 100, 105), k, v, r, tt, d, H)
## [1] 20.51497 24.02383 27.73038
bscall(c(95, 100, 105), k, v, r, tt, d)
## [1] 20.51669 24.02467 27.73038
```

## Option Greeks

Greeks can be computed one of two ways. The `greeks` function takes as arguments the name of the pricing function and then its inputs. The `greeks2` function takes the function call as an input. Here are illustrations of the two functions using the same parameters:


```r
greeks(uicall(s, k, v, r, tt, d, H))
##                  uicall
## Price      24.023834247
## Delta       0.722328367
## Gamma       0.007903118
## Vega        0.474353223
## Rho         0.963837754
## Theta      -0.020309590
## Psi        -1.444314473
## Elasticity  3.006715579
greeks2(uicall, s=s, k=k, v=v, r=r, tt=tt, d=d, H=H)
##                  uicall
## Price      24.023834247
## Delta       0.722328367
## Gamma       0.007903118
## Vega        0.474353223
## Rho         0.963837754
## Theta      -0.020309590
## Psi        -1.444314473
## Elasticity  3.006715579
```

## Binomial Pricing

There are two functions in this section: `binomopt`, which computes
binomial option prices, and `binomplot`, which displays the asset
price tree, the corresponding probability of being at each node, and
whehter or not the option is in exercised at each node.

### `binomopt`

Here are examples of pricing, illustrating the default of just
returning the price, and the ability to return the price plus
parameters, as well as the price, the parameters, and various trees:

```r
s <- 100; k <- 100; r <- 0.08; v <- 0.30; tt <- 2; d <- 0.03
binomopt(s, k, v, r, tt, d, nstep=4)
## [1] 20.28837
binomopt(s, k, v, r, tt, d, nstep=4, returnparams=TRUE)
## $price
## [1] 20.28837
## 
## $params
##           s           k           v           r          tt           d 
## 100.0000000 100.0000000   0.3000000   0.0800000   2.0000000   0.0300000 
##       nstep           p          up          dn           h 
##   4.0000000   0.4471650   1.2676085   0.8293342   0.5000000
binomopt(s, k, v, r, tt, d, nstep=4, putopt=TRUE)
## [1] 12.37422
binomopt(s, k, v, r, tt, d, nstep=4, returntrees=TRUE, putopt=TRUE)
## $price
## [1] 12.37422
## 
## $params
##           s           k           v           r          tt           d 
## 100.0000000 100.0000000   0.3000000   0.0800000   2.0000000   0.0300000 
##       nstep           p          up          dn           h 
##   4.0000000   0.4471650   1.2676085   0.8293342   0.5000000 
## 
## $oppricetree
##          [,1]      [,2]      [,3]     [,4]     [,5]
## [1,] 12.37422  4.150096  0.000000  0.00000  0.00000
## [2,]  0.00000 19.939844  7.813297  0.00000  0.00000
## [3,]  0.00000  0.000000 31.220474 14.70993  0.00000
## [4,]  0.00000  0.000000  0.000000 42.95878 27.69407
## [5,]  0.00000  0.000000  0.000000  0.00000 52.69377
## 
## $stree
##      [,1]      [,2]      [,3]      [,4]      [,5]
## [1,]  100 126.76085 160.68312 203.68329 258.19066
## [2,]    0  82.93342 105.12711 133.26002 168.92152
## [3,]    0   0.00000  68.77953  87.18551 110.51709
## [4,]    0   0.00000   0.00000  57.04122  72.30593
## [5,]    0   0.00000   0.00000   0.00000  47.30623
## 
## $probtree
##      [,1]     [,2]      [,3]       [,4]       [,5]
## [1,]    1 0.447165 0.1999565 0.08941355 0.03998261
## [2,]    0 0.552835 0.4944169 0.33162889 0.19772377
## [3,]    0 0.000000 0.3056266 0.40999649 0.36667214
## [4,]    0 0.000000 0.0000000 0.16896107 0.30221389
## [5,]    0 0.000000 0.0000000 0.00000000 0.09340760
## 
## $exertree
##       [,1]  [,2]  [,3]  [,4]  [,5]
## [1,] FALSE FALSE FALSE FALSE FALSE
## [2,] FALSE FALSE FALSE FALSE FALSE
## [3,] FALSE FALSE  TRUE FALSE FALSE
## [4,] FALSE FALSE FALSE  TRUE  TRUE
## [5,] FALSE FALSE FALSE FALSE  TRUE
```



# Illustrative Functions

Several functions are intended to illustrate some aspects of the
material. 

## Quincunx or Galton Board


## Plotting the Solution to the Binomial Pricing Model

The `binomplot` function calls `binomopt` to compute the option price
and the various trees, which it then uses in plotting:

The first plot, figure \ref{fig:binomplot1}, is basic:


```r
binomplot(s, k, v, r, tt, d, nstep=6, american=TRUE, putopt=TRUE)
```

![Basic option plot showing stock prices and nodes at which the option is exercised.\label{fig:binomplot1}](figure/binomplot1-1.png) 

The second plot, figure \ref{fig:binomplot2}, adds a display of stock
prices and arrows connecting the nodes.


```r
binomplot(s, k, v, r, tt, d, nstep=6, american=TRUE, putopt=TRUE,
    plotvalues=TRUE, plotarrows=TRUE)
```

![Same plot as Figure \ref{fig:binomplot1} except that values and arrows are added to the plot.\label{fig:binomplot2}](figure/binomplot2-1.png) 

Finally, here is a plot for an American call when the dividend yield
is positive and `nstep` has a larger value:


```r
d <- 0.06
binomplot(s, k, v, r, tt, d, nstep=50, american=TRUE)
```

![Binomial plot when nstep=50.\label{fig:binomplot3}](figure/unnamed-chunk-6-1.png) 

Notice that the large value of `nstep` creates a high maximum terminal
stock price, which makes the plot hard to see in the narrow region where
exercise is occurring. We can zoom in on that region by setting
`ylimval=TRUE` and selecting values for `ylimval`:


```r
d <- 0.06
binomplot(s, k, v, r, tt, d, nstep=50, american=TRUE, setylim=TRUE, ylimval=c(75, 225))
```

![Binomial plot when nstep=50, using the argument ylimval to focus on a subset..\label{fig:binomplot4}](figure/unnamed-chunk-7-1.png) 
