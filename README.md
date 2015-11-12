
## Introduction

This is a collection of option pricing functions for a course in
financial derivatives. The names of the functions mostly match those
in my book *Derivatives Markets*, which explains the package name.
This is the first version; I expect to add many functions
over time. Depending upon the level of the course, this package may or
may not be helpful for students. I hope that at a minimum it will be
helpful for instructors.

There are of course other option pricing packages for R, notably
RQuantLib and fOptions.  I don't claim to add significant
functionality to those. This package does, however, have a few aspects
that might be unique, which I describe below.

## Things of note

### Calculation of Greeks

I have tried to make calculation of Greeks especially easy. The
function `bsopt()` is vectorized, and simultaneously computes prices
and Greeks for European calls and puts. There are also two generic
functions, `greeks()` and `greeks2()`, which allow vectorized
calculation of greeks for any option pricing calculation based on the
standard Black-Scholes or barrier option inputs.

As an example,

```r
x <- greeks2(bscall(s=40, k=c(35, 40, 45), v=0.3, r=0.08, tt=0.25, d=0))
```

will compute the full complement of greeks for a call, for three
strike prices. You can access the delta values as, for example,
`x['Delta', ]`.


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
500 options. This is 16 plots in all.  It's a great illustration of how powerful R can be.

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
```r
> x <- binomopt(41, 40, .3, .08, 1, 0, 3, putopt=TRUE, returntrees=TRUE)
> x
$price
   price 
3.292948 

$greeks
       delta        gamma        theta 
-0.331656818  0.037840906 -0.005106465 

$params
         s          k          v          r         tt          d      nstep 
41.0000000 40.0000000  0.3000000  0.0800000  1.0000000  0.0000000  3.0000000 
         p         up         dn          h 
 0.4568067  1.2212461  0.8636926  0.3333333 

$oppricetree
         [,1]      [,2]     [,3]      [,4]
[1,] 3.292948 0.7409412 0.000000  0.000000
[2,] 0.000000 5.6029294 1.400911  0.000000
[3,] 0.000000 0.0000000 9.415442  2.648727
[4,] 0.000000 0.0000000 0.000000 13.584345

$stree
     [,1]     [,2]     [,3]     [,4]
[1,]   41 50.07109 61.14913 74.67813
[2,]    0 35.41139 43.24603 52.81404
[3,]    0  0.00000 30.58456 37.35127
[4,]    0  0.00000  0.00000 26.41565

$probtree
     [,1]      [,2]      [,3]       [,4]
[1,]    1 0.4568067 0.2086723 0.09532291
[2,]    0 0.5431933 0.4962687 0.34004825
[3,]    0 0.0000000 0.2950590 0.40435476
[4,]    0 0.0000000 0.0000000 0.16027409

$exertree
      [,1]  [,2]  [,3]  [,4]
[1,] FALSE FALSE FALSE FALSE
[2,] FALSE FALSE FALSE FALSE
[3,] FALSE FALSE  TRUE  TRUE
[4,] FALSE FALSE FALSE  TRUE

$deltatree
           [,1]        [,2]       [,3]
[1,] -0.3316568 -0.07824964  0.0000000
[2,]  0.0000000 -0.63298582 -0.1712971
[3,]  0.0000000  0.00000000 -1.0000000

$bondtree
         [,1]      [,2]      [,3]
[1,] 16.89088  4.658986  0.000000
[2,]  0.00000 28.017840  8.808828
[3,]  0.00000  0.000000 38.947430
```

#### binomplot

This function plots the binomial tree, providing a visual depiction of
the nodes, the probability of reaching each node, and whether exercise
occurs at that node.

### Galton board or quincunx

The [Galton board](http://mathworld.wolfram.com/GaltonBoard.html) is a
pegboard that illustrates the central limit theorem. Balls drop from
the top and randomly fall right or left, providing a physical
simulation of a binomial
distribution. (My physicist brother-in-law tells me that real-life Galton boards don't typically generate a normal distribution because, among other things, balls acquire momentum in the direction of their original travel. The distribution is thus likely to be fatter-tailed than normal.)

You can see the Galton board in action with `quincunx()`.

## Feedback

Please feel free to contact me with bug reports or suggestions. Best
would be to file an issue on Github, but email is fine as well.

I hope you find this helpful!
