#' @title Calculate option Greeks
#'
#' @description The functions \code{greeks} and \code{greeks2} provide
#' two different calling conventions for computing a full set of
#' option Greeks. \code{greeks} simply requires entering a pricing function
#' with parameters. \code{greeks2} requires the use of named parameter
#' entries. The function \code{bsopt} calls \code{greeks2} to
#' produce a full set of prices and greeks for calls and puts. These
#' functions are all vectorized, the only restriction being that the
#' functions will produce an error if the recycling rule can not be
#' used safely (that is, if parameter vector lengths are not integer
#' multiples of one another).
#'
#' @name greeks
#' @aliases bsopt greeks greeks2
#'
#' @return A named list of Black-Scholes option prices and Greeks, or
#'     optionally (`complete=TRUE`) a dataframe.
#' @note The pricing function being passed to the greeks function must
#'     return a numeric vector. For example, \code{callperpetual} must
#'     be called with the option \code{showbarrier=FALSE} (the
#'     default). The pricing function call cannot contain a variable
#'     named `z91k25`.
#' @details Numerical derivatives are calculated using a simple
#'     difference. This can create numerical problems in edge
#'     cases. It might be good to use the package numDeriv or some
#'     other more sophisticated calculation, but the current approach
#'     works well with vectorization.
#'
#' @usage
#' greeks(f, complete=FALSE, long=FALSE, initcaps=TRUE)
#' # must used named list entries:
#' greeks2(fn, ...)
#' bsopt(s, k, v, r, tt, d)
#'
#' @param s Price of underlying asset
#' @param k Option strike price
#' @param v Volatility of the underlying asset, defined as the
#'     annualized standard deviation of the continuously-compounded
#'     return
#' @param r Annual continuously-compounded risk-free interest rate
#' @param tt Time to maturity in years
#' @param d Dividend yield of the underlying asset, annualized,
#'     continuously-compounded
#' @param fn Pricing function name, not in quotes
#' @param f Fully-specified option pricing function, including inputs
#'     which need not be named. For example, you can enter
#'     \code{greeks(bscall(40, 40, .3, .08, .25, 0))}
#' @param complete FALSE. If TRUE, return a data frame with columns
#'     equal to input parameters, function name, premium, and greeks
#'     (each greek is a column). This is experimental and the output
#'     may change. Convert to long format using \code{long=TRUE}.
#' @param long FALSE. If \code{complete=TRUE}, then \code{long=TRUE} will
#'     return a long data frame, where each row contains input
#'     parameters, function name, and either the premium or one of the
#'     greeks
#' @param initcaps TRUE. If true, capitalize names (e.g. "Delta" vs
#'     "delta")
#' @param ... Pricing function inputs, must be named, may either be a
#'     list or not
#'
#' @importFrom stats reshape
#'
#' @examples
#' s=40; k=40; v=0.30; r=0.08; tt=0.25; d=0;
#' greeks(bscall(s, k, v, r, tt, d), complete=FALSE, long=FALSE, initcaps=TRUE)
#' greeks2(bscall, list(s=s, k=k, v=v, r=r, tt=tt, d=d))
#' greeks2(bscall, list(s=s, k=k, v=v, r=r, tt=tt, d=d))[c('Delta', 'Gamma'), ]
#' bsopt(s, k, v, r, tt, d)
#' bsopt(s, c(35, 40, 45), v, r, tt, d)
#' bsopt(s, c(35, 40, 45), v, r, tt, d)[['Call']][c('Delta', 'Gamma'), ]
#'
#' ## plot Greeks for calls and puts for 500 different stock prices
#' ##
#' ## This plot can generate a "figure margins too large" error
#' ## in Rstudio
#' k <- 100; v <- 0.30; r <- 0.08; tt <- 2; d <- 0
#' S <- seq(.5, 250, by=.5)
#' Call <- greeks(bscall(S, k, v, r, tt, d))
#' Put <- greeks(bsput(S, k, v, r, tt, d))
#' y <- list(Call=Call, Put=Put)
#' par(mfrow=c(4, 4), mar=c(2, 2, 2, 2))  ## create a 4x4 plot
#' for (i in names(y)) {
#'     for (j in rownames(y[[i]])) {  ## loop over greeks
#'         plot(S, y[[i]][j, ], main=paste(i, j), ylab=j, type='l')
#'     }
#' }
#' \dontrun{
#' ## Using complete option for calls
#' call_long <- greeks(bscall(S, k, v, r, tt, d), complete=TRUE, long=TRUE)
#' ggplot2::ggplot(call_long, aes(x=s, y=value)) +
#'       geom_line() + facet_wrap(~greek, scales='free')
#' }

#' @export
bsopt <- function(s, k, v, r, tt, d) {
    ## Black-Scholes put and call values.
    xc <- greeks2(bscall, list(s=s, k=k, v=v, r=r, tt=tt, d=d))
    xp <- greeks2(bsput, list(s=s, k=k, v=v, r=r, tt=tt, d=d))
    return(list(Call=xc, Put=xp))
}


## the focus so far has been on named vs unnamed parameters. We also
## need to take care of implicit parameters Tue, Jun 21, 2016
#' @export
greeks <- function(f, complete=FALSE, long=FALSE, initcaps=TRUE) {
    ## match.call() returns (I think) a pairlist, where the first
    ## argument is the function and the second is what follows. By
    ## extracting the second, we grab the function argument which in
    ## this case is itself a pairlist (namely the function within
    ## greeks()). By setting the first element of this
    ## (match.call()[[2]][[1]] to NULL we are left with the arguments
    ## in the wrapped function, which are in the form of a list
    args <- match.call()[[2]] ## get f and arguments
    funcname <- as.character(args[[1]])
    args[[1]] <- NULL  ## eliminate function name, leaving function
                       ## arguments as only remaining component
    fnames <- names(formals(funcname)) ## list of defined arguments
    ## following handles case of perpetual options
    includetheta <- ("tt" %in% fnames)
    ## following logic handles the case where some arguments are
    ## named, some are unnamed, and the arguments are out of order,
    ## i.e.  the named arguments are in some arbitrary order.  If the
    ## function is defined as f(a, b, c), if it's called as
    ## f(c=3,5,2), the unused arguments are assigned to 5 and 2 in the
    ## order defined (a then b). The following code fills them in as
    ## such. The code also keeps counts the arguments in the call to
    ## handle the case of implicit parameters
    numargs <- length(args)
    if (length(names(args)) == 0) {
        ## all arguments unnamed, hence in the correct order
        names(args) <- fnames[1:numargs]
    } else {
        numunnamedargs <- sum(names(args) == "")
        ##  restrict range in setdiff to account for implicit
        ##  parameters in fnames but not in function call
        names(args)[which(names(args) == "")] <-
            setdiff(fnames, names(args))[1:numunnamedargs]
    }
    x <- as.list(args)
    ## some elements of x will have had explicit values in the call
    ## (e.g., "s=40"), while others will have inherited values from
    ## the environment (e.g. "s=s0"). The latter are unevaluated
    ## language objects, so need to assign a value using eval().
    ##
    ## need to handle the case when the loop index appears in the
    ## evaluted epxression. So pick a screwy loop index
    for (z91k25 in 1:length(x)) x[[z91k25]] <- eval(x[[z91k25]])
    ## make sure that vectors have lengths appropriate for recycling
    .checkListRecycle(x)
    xlength <- sapply(x, length)
    xmaxlength <- max(xlength)
    includetheta <- rep(includetheta, xmaxlength)
    Premium  <-  do.call(funcname, x)
    Delta <-  .FirstDer(funcname, 's', x)
    Vega  <-  .FirstDer(funcname, 'v', x)/100
    Rho   <-  .FirstDer(funcname, 'r', x)/100
    Theta <- ifelse(includetheta,
                    -.FirstDer(funcname, 'tt', x)/365,
                    NA)
    Psi   <-  .FirstDer(funcname, 'd', x)/100
    Elast <-  ifelse(Premium > 1e-06, x[['s']]*Delta/Premium, NA)
    Gamma <-  .SecondDer(funcname, 's', x)
    if (complete) {
        ## Note: this will not work with a tibble, which doesn't
        ## support recycling for vectors longer than length 1
        tmp <- cbind(as.data.frame(x, stringsAsFactors=FALSE),
                     funcname, Premium, Delta, Vega,
                     Rho, Theta, Psi, Elast, Gamma,
                     stringsAsFactors=FALSE)
        if (long) {
            premcol <- which(colnames(tmp) == 'premium')
            gammacol <- which(colnames(tmp) == 'gamma')
            greekcols <- which(colnames(tmp) %in%
                               c('Premium', 'Delta', 'Vega',
                                 'Rho', 'Theta', 'Psi', 'Elast',
                                 'Gamma'))
            tmp <- stats::reshape(tmp,
                                  direction='long',
                                  ##varying=premcol:gammacol,
                                  varying=greekcols,
                                  v.names='value',
                                  timevar='greek',
                                  ##times=names(tmp)[premcol:gammacol])
                                  times=names(tmp)[greekcols]
                                  )
            }
        row.names(tmp) <- NULL
        tmp[['id']] <- NULL
        if (!initcaps) colnames(tmp) <- tolower(colnames(tmp))
        return(tmp)
    } else {
        numcols <- length(Premium)
        numrows <- 8
        y <- t(matrix(c(Premium,Delta,Gamma,Vega,Rho,Theta,Psi,Elast),
                      nrow=numcols,ncol=numrows))
        rownames(y) <- c("Premium", "Delta", "Gamma", "Vega", "Rho", "Theta",
                         "Psi", "Elasticity")

        ## The following tests to see if there is variation in any inputs
        ## (is xmaxlength > 1). If so, is there variation in more than one
        ## input (length(maxarg) > 1). The column names are constructed
        ## in each case to state the parameter values for that column.
        arggt1 <- which(xlength > 1) # which inputs are vectors
        if (xmaxlength == 1) {
            colnames(y) <- funcname
        } else {
            ## if we get here, there are multiple inputs with length > 1
            tmp <- NULL
            for (i in arggt1) {
                tmp <- paste(tmp, format(x[[i]], digits=3, trim=TRUE),
                             sep='_')
            }
            colnames(y) <- paste(funcname, tmp, sep='')
        }
        if (!initcaps) rownames(y) <- tolower(rownames(y))
        return(y)
    }
}


#' @export
greeks2 <- function(fn, ...) {
    ## Fix handling of inputs with different lengths want to modify
    ## this function so that inputs need not be named
    if (is.list(c(...))) x <- c(...)
    else x <- list(...)
    includetheta <- ('tt' %in% names(x))
    ## make sure recycling rule will work, stop if not
    .checkListRecycle(x)
    premium  <-  do.call(fn, x)
    delta <-  .FirstDer(fn, 's', x)
    vega  <-  .FirstDer(fn, 'v', x)/100
    rho   <-  .FirstDer(fn, 'r', x)/100
    if (includetheta) theta <- -.FirstDer(fn, 'tt', x)/365
    else theta <- NA
    psi   <-  .FirstDer(fn, 'd', x)/100
    elast <-  x[['s']]*delta/premium
    gamma <-  .SecondDer(fn, 1, x)
    numcols <- length(premium)
    numrows <- 8
    y <- t(matrix(c(premium,delta,gamma,vega,rho,theta,psi,elast),
                  nrow=numcols,ncol=numrows))
    rownames(y) <- c("Premium", "Delta", "Gamma", "Vega", "Rho", "Theta",
                     "Psi", "Elasticity")
    funcname <- as.character(match.call()[[2]])

    ## The following tests to see if there is variation in any inputs
    ## (is xmaxlength > 1). If so, is there variation in more than one
    ## input (length(maxarg) > 1)? The column names are constructed as
    ## appropriate in each case, showing varying input values by column.

    ## are any parameters input as vectors?
    xlength <- lapply(x, length) ## how many of each input?
    xmaxlength <- max(unlist(lapply(x, length))) ## max # of inputs
    arggt1 <- which(xlength > 1)
    if (xmaxlength == 1) {
        colnames(y) <- funcname
    } else {
        ## if we get here, there are multiple inputs with length > 1
        tmp <- NULL
        for (i in arggt1) {
            tmp <- paste(tmp, format(x[[i]], digits=3, trim=TRUE), sep='_')
        }
        colnames(y) <- paste(funcname, tmp, sep='')
    }
    return(y)
}



.FirstDer <- function(fn, pos, arglist) {
    ## compute first derivative of function fn
    ## arglist must be a list
    epsilon <- 1e-04
    xup <- xdn <- arglist
    xup[[pos]] <- xup[[pos]] + epsilon
    xdn[[pos]] <- xdn[[pos]] - epsilon
    yup <- do.call(fn, xup)
    ydn <- do.call(fn, xdn)
    return((yup-ydn)/(2*epsilon))
}

.SecondDer <- function(fn, pos, ...) {
    ## this is original
    ##   compute second derivative of function fn
    if (is.list(c(...))) arglist <- c(...)
    else arglist <- list(...)
    epsilon <- 5e-04
    xup <- xdn <- arglist
    xup[[pos]] <- xup[[pos]] + epsilon
    xdn[[pos]] <- xdn[[pos]] - epsilon
    yup <- .FirstDer(fn, pos, xup)
    ydn <- .FirstDer(fn, pos, xdn)
    return((yup-ydn)/(2*epsilon))
}

.checkListRecycle <- function(x) {
    ## function tests whether list of vectors can work with recycling
    ## without throwing a warning. We can do this by unlisting the
    ## elements, summing them, and checking for an error. (The summing
    ## will require recycling to work; if it doesn't, there is a
    ## mismatch in the number of entries.)
    tryCatch(
        {tmp <- 0; for (i in seq_along(x)) tmp <- tmp+unlist(x[[i]])},
        warning = function(c) {
            c$message <- paste("Input vector lengths are not",
                               "integer multiples of one another")
            stop(c)
        }
    )
}
