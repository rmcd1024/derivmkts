#' @title Black-Scholes option pricing
#'
#' @description \code{bscall} and \code{bsput} compute Black-Scholes
#' call and put prices. The functions \code{assetcall},
#' \code{assetput}, \code{cashcall}, and \code{cashput} provide the
#' prices of binary options that pay a share (the asset options) or $1
#' (the cash options) if at expiration the asset price exceeds the
#' strike (the calls) or is below the strike (the puts). We have the
#' identities
#'
#' \code{bscall(s, k, v, r, tt, d)
#'   = assetcall(s, k, v, r, tt, d) - k*cashcall(s, k, v, r, tt, d)}
#'
#'
#' @name blksch
#' @aliases bscall bsput assetcall assetput cashcall cashput 
#'
#' @return A Black-Scholes option price. If more than one argument is a
#' vector, the recycling rule determines the handling of the inputs
#'
#' @usage
#' bscall(s, k, v, r, tt, d)
#' bsput(s, k, v, r, tt, d)
#' assetcall(s, k, v, r, tt, d)
#' cashcall(s, k, v, r, tt, d)
#' assetput(s, k, v, r, tt, d)
#' cashput(s, k, v, r, tt, d)
#' 
#'
#' @param s Stock price
#' @param k Strike price of the option
#' @param v Volatility of the stock, defined as the annualized
#' standard deviation of the continuously-compounded return
#' @param r Annual continuously-compounded risk-free interest rate
#' @param tt Time to maturity in years
#' @param d Dividend yield, annualized, continuously-compounded
#' 
#' @details Returns a scalar or vector of option prices, depending on
#' the inputs
#'
#' @note It is possible to specify the inputs either in terms of an
#' interest rate and a "dividend yield" or an interest rate and a
#' "cost of carry". In this package, the dividend yield should be
#' thought of as the cash dividend received by the owner of the
#' underlying asset, \emph{or} (equivalently) as the payment received
#' if the owner were to lend the asset.
#'
#' There are other option pricing packages available for R, and these
#' may use different conventions for specifying inputs. In fOptions,
#' the dividend yield is replaced by the generalized cost of carry,
#' which is the net payment required to fund a position in the
#' underlying asset. If the interest rate is 10\% and the dividend
#' yield is 3\%, the generalized cost of carry is 7\% (the part of the
#' interest payment not funded by the dividend payment). Thus, using
#' the \code{GBS} function from fOptions, these two expressions return
#' the same price:
#'
#' \code{bscall(s, k, v, r, tt, d)}
#'
#' \code{fOptions::GBSOption('c', S=s, K=k, Time=tt, r=r, b=r-d, sigma=v) }
#'
#'
#' @examples
#' s=40; k=40; v=0.30; r=0.08; tt=0.25; d=0;
#' bscall(s, k, v, r, tt, d)
#'
#' ## following returns the same price as previous
#' assetcall(s, k, v, r, tt, d) - k*cashcall(s, k, v, r, tt, d)
#'
#' ## return option prices for different strikes
#' bsput(s, k=38:42, v, r, tt, d)

#' @export
bscall <- function(s, k, v, r, tt, d) {
    callp <- s*exp(-d*tt)*.nd1(s, k, v, r, tt, d) -
        k*exp(-r*tt)*.nd2(s, k, v, r, tt, d)
    return(callp)
}

#' @export
bsput <- function(s, k, v, r, tt, d) {
    putp <- bscall(s, k, v, r, tt, d) + k*exp(-r*tt) -
        s*exp(-d*tt)
    return(putp)
}

#' @export
assetcall <- function(s, k, v, r, tt, d) {
    price <- s*exp(-d*tt)*pnorm(.d1(s, k, v, r, tt, d))
    return(price)
}

#' @export
cashcall <- function(s, k, v, r, tt, d) {
    price <- exp(-r*tt)*pnorm(.d2(s, k, v, r, tt, d))
    return(price)
}

#' @export
assetput <- function(s, k, v, r, tt, d) {
    price <- s*exp(-d*tt)*pnorm(-.d1(s, k, v, r, tt, d))
    return(price)
}

#' @export
cashput <- function(s, k, v, r, tt, d) {
    price <- exp(-r*tt)*pnorm(-.d2(s, k, v, r, tt, d))
    return(price)
}


.d1 <- function(s, k, v, r, tt, d)
    (log(s/k) + (r-d+v^2/2)*tt)/(v*sqrt(tt))

.d2 <- function(s, k, v, r, tt, d)
    .d1(s, k, v, r, tt, d) - v*tt^(0.5)

.nd1 <- function(s, k, v, r, tt, d)
    pnorm(.d1(s, k, v, r, tt, d))

.nd2 <- function(s, k, v, r, tt, d)
    pnorm(.d2(s, k, v, r, tt, d))

##
##greeks <- function(price, ...) {
##    ## Fix handling of inputs with different lengths
##    ## want to modify this function so that
##    if (is.list(c(...))) x <- c(...)
##    else x <- list(...)
##    ## make sure recycling rule will work, stop if not
##    .checkListRecycle(x)
##    prem  <-  do.call(price, x)
##    delta <-  .FirstDer(price, 's', x)
##    vega  <-  .FirstDer(price, 'v', x)/100
##    rho   <-  .FirstDer(price, 'r', x)/100
##    theta <- -.FirstDer(price, 'tt', x)/365
##    psi   <-  .FirstDer(price, 'd', x)/100
##    elast <-  x[['s']]*delta/prem
##    gamma <-  .SecondDer(price, 1, x)
##    numcols <- length(prem)
##    numrows <- 8
##    y <- t(matrix(c(prem,delta,gamma,vega,rho,theta,psi,elast),
##                  nrow=numcols,ncol=numrows))
##    rownames(y) <- c("Price", "Delta", "Gamma", "Vega", "Rho", "Theta",
##                     "Psi", "Elasticity")
##    funcname <- as.character(match.call()[[2]])
##    
##    ## In the following, this tests to see if there is variation in
##    ## any inputs (is xmaxlength > 1). If so, is there variation in
##    ## more than one input (length(maxarg) > 1). The column names are
##    ## constructed as appropriate in each case.
##
##    ## are any parameters input as vectors?
##    xlength <- lapply(x, length) ## how many of each input?
##    xmaxlength <- max(unlist(lapply(x, length))) ## max # of inputs
##    arggt1 <- which(xlength > 1)
##    if (xmaxlength == 1) {
##        colnames(y) <- funcname
##    } else {
##        ## if we get here, there are multiple inputs with length > 1
##        tmp <- NULL
##        for (i in arggt1) {
##            tmp <- paste(tmp, format(x[[i]], digits=3, trim=TRUE), sep='_')
##        }
##        colnames(y) <- paste(funcname, tmp, sep='')
##    }
##    return(y)
##}
##
##greeks2 <- function(f) {
##    ## This version uses a standard function call
##    args <- match.call()[[2]] ## get f and arguments
##    funcname <- as.character(args[[1]]) 
##    args[[1]] <- NULL  ## eliminate function name, leaving only the
##                       ## function arguments
##    argnames <- formals(match.fun(funcname)) ## arguments of function
##    if (sum(names(args)=='') > 0) {
##        fnames <- names(formals(funcname)) ## get arguments to function
##        shared <- intersect(names(args), fnames)
##        names(args)[names(args)==''] <- setdiff(fnames, shared)
##    }
##
##    x <<- as.list(args)
##    ## Issue: When an argument is a vector, the list representation
##    ## stores the values as a language object (an unevaluated
##    ## call). In order to extract the values, need to use "eval
##    ## x[[i]]".  
##    for (i in 1:length(x)) x[[i]] <- eval(x[[i]])
##    .checkListRecycle(x)
##    prem  <-  do.call(funcname, x)
##    delta <-  .FirstDer(funcname, 's', x)
##    vega  <-  .FirstDer(funcname, 'v', x)/100
##    rho   <-  .FirstDer(funcname, 'r', x)/100
##    theta <- -.FirstDer(funcname, 'tt', x)/365
##    psi   <-  .FirstDer(funcname, 'd', x)/100
##    elast <-  x[['s']]*delta/prem
##    gamma <-  .SecondDer(funcname, 's', x)
##    numcols <- length(prem)
##    numrows <- 8
##    y <- t(matrix(c(prem,delta,gamma,vega,rho,theta,psi,elast),
##                  nrow=numcols,ncol=numrows))
##    rownames(y) <- c("Price", "Delta", "Gamma", "Vega", "Rho", "Theta",
##                     "Psi", "Elasticity")
###    funcname <- as.character(match.call()[[2]])
##
##    ## In the following, this tests to see if there is variation in
##    ## any inputs (is xmaxlength > 1). If so, is there variation in
##    ## more than one input (length(maxarg) > 1). The column names are
##    ## constructed as appropriate in each case.
##
##    ## are any parameters input as vectors?
##    xlength <- lapply(x, length) ## how many of each input?
##    xmaxlength <- max(unlist(lapply(x, length))) ## max # of inputs
##    arggt1 <- which(xlength > 1)
##    if (xmaxlength == 1) {
##        colnames(y) <- funcname
##    } else {
##        ## if we get here, there are multiple inputs with length > 1
##        tmp <- NULL
##        for (i in arggt1) {
##            tmp <- paste(tmp, format(x[[i]], digits=3, trim=TRUE),
##                         sep='_')
##        }
##        colnames(y) <- paste(funcname, tmp, sep='')
##    }
##    return(y)
##}
##
##
##.FirstDer <- function(fn, pos, arglist) {
##    ## compute first derivative of function fn
##    ## arglist must be a list
##    epsilon <- 0.000001
##    xup <- xdn <- arglist
##    xup[[pos]] <- xup[[pos]] + epsilon
##    xdn[[pos]] <- xdn[[pos]] - epsilon
##    yup <- do.call(fn, xup)
##    ydn <- do.call(fn, xdn)
##    return((yup-ydn)/(2*epsilon))
##}
##
##.SecondDer <- function(fn, pos, ...) {
##    ## this is original
##    ##   compute second derivative of function fn
##    if (is.list(c(...))) arglist <- c(...)
##    else arglist <- list(...)
##    epsilon <- 0.0005
##    xup <- xdn <- arglist
##    xup[[pos]] <- xup[[pos]] + epsilon
##    xdn[[pos]] <- xdn[[pos]] - epsilon
##    yup <- .FirstDer(fn, pos, xup)
##    ydn <- .FirstDer(fn, pos, xdn)
##    return((yup-ydn)/(2*epsilon))
##}
##
##.checkListRecycle <- function(x) {
##    ## function tests whether list of vectors can work with recylcing
##    ## without throwing a warning. We can do this by unlisting the
##    ## elements, summing them, and checking for an error
##    tryCatch(
##        {tmp <- 0; for (i in seq_along(x)) tmp <- tmp+unlist(x[[i]])},
##        warning = function(c) {
##            c$message <- paste("Input vector lengths are not",
##                               "integer multiples of one another")
##            stop(c)
##        }
##    )
##}
