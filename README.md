

## Introduction

This is a collection of option pricing functions for a course in
financial derivatives. The names of the functions mostly match those
in my book *Derivatives Markets*, which explains the package name.
This is the initial version; I expect to add and refine functions over
time. Depending upon the level of the course, this package may or may
not be helpful for students. I hope that at a minimum it will be
helpful for instructors.

There are of course other option pricing packages in R, notably
RQuantLib and fOptions. I don't claim to add significant pricing
functionality to those. This package does, however, have a few aspects
that might be unique, which I describe below.

The package includes functions for computing

* Black-Scholes prices and greeks for European options
* Option pricing and plotting using the binomial model
* Barrier options
* Pricing of options with jumps using the Merton model
* Analytical and Monte Carlo pricing of Asian options 

## Things of note

### Calculation of Greeks

I have tried to make calculation of Greeks especially easy. The
function `bsopt()` is vectorized, and simultaneously computes prices
and Greeks for European calls and puts. There are also two generic
functions, `greeks()` and `greeks2()`, which allow vectorized
calculation of greeks for any option pricing calculation based on the
standard Black-Scholes or barrier option inputs.

As an example, the following two calculations will produce identical
output, computing the full complement of greeks for a call, for three
strike prices. You can access the delta values as, for example,
`x['Delta', ]`.



```r
x1 <- greeks(bscall(s=40, k=c(35, 40, 45), v=0.3, r=0.08, tt=0.25, d=0))
x2 <- greeks2(bscall, s=40, k=c(35, 40, 45), v=0.3, r=0.08, tt=0.25, d=0)
x1
```

```
             bscall_35   bscall_40   bscall_45
Price       6.13481997  2.78473666  0.97436823
Delta       0.86401619  0.58251565  0.28200793
Gamma       0.03636698  0.06506303  0.05629794
Vega        0.04364029  0.07807559  0.06755753
Rho         0.07106457  0.05128972  0.02576487
Theta      -0.01340407 -0.01733098 -0.01336419
Psi        -0.08640162 -0.05825156 -0.02820079
Elasticity  5.63352271  8.36726367 11.57705764
```

You can also define a function for a spread position. You can then pass this function to `greeks()` and easily compute greeks for that position.

The function

```r
y <- bsopt(s=40, k=c(35, 40, 45), v=0.3, r=0.08, tt=0.25, d=0)
```

will do the same for both calls and puts simultanteously. The delta
values for the call would be `y[['Call']]['Delta', ]`

My favorite example, which you should run, is this:


```r
k <- 100; v <- 0.30; r <- 0.08; tt <- 2; d <- 0
S <- seq(.5, 250, by=.5)
x <- bsopt(S, k, v, r, tt, d)
par(mfrow=c(4, 4))  ## create a 4x4 plot
for (i in c('Call', 'Put')) {
    for (j in rownames(x[[i]])) {  ## loop over greeks
        plot(S, x[[i]][j, ], main=paste(i, j), ylab=j, type='l')
    }
}
```

This small bit of code computes and plots all call and put Greeks for
500 options. This is 16 plots in all.  It's a great illustration of
how powerful R can be.

### Binomial calculations

#### binomopt

By default the binomopt function returns the price of a American
call. In adddition, with:
* `putopt=TRUE` it returns the price of an American
put.
*  `returngreeks=TRUE` it returns a subset of the Greeks
along with the binomial parameters.
* `returntrees=TRUE` it returns as a list all of the above plus the
  full binomial tree ($stree), the probability of reaching each node
  ($probtree), whether or not the option is exercised at each node
  (exertree), and the replicating portfolio at each node ($deltatree
  and $bondtree).

Here is an example illustrating everything that the function can return:


#### binomplot

This function plots the binomial tree, providing a visual depiction of
the nodes, the probability of reaching each node, and whether exercise
occurs at that node.

### Galton board or quincunx

The [Galton board](http://mathworld.wolfram.com/GaltonBoard.html) is a
pegboard that illustrates the central limit theorem. Balls drop from
the top and randomly fall right or left, providing a physical
simulation of a binomial distribution. (My physicist brother-in-law
tells me that real-life Galton boards don't typically generate a
normal distribution because, among other things, balls acquire
momentum in the direction of their original travel. The distribution
is thus likely to be fatter-tailed than normal.)

You can see the Galton board in action with `quincunx()`.

## Feedback

Please feel free to contact me with bug reports or suggestions. Best
would be to file an issue on Github, but email is fine as well.

I hope you find this helpful!
