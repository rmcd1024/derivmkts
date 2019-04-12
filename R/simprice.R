#' @title Simulate asset prices
#'
#' @description \code{simstock} computes simulated stock price paths,
#'     with or without jumps. 
#'
#' \code{bscall(s, k, v, r, tt, d)
#'   = assetcall(s, k, v, r, tt, d) - k*cashcall(s, k, v, r, tt, d)}
#'
#' \code{bsput(s, k, v, r, tt, d)
#'   = k*cashput(s, k, v, r, tt, d) - assetput(s, k, v, r, tt, d)}
#'
#'
#' @name simprice
#' @aliases bscall bsput assetcall assetput cashcall cashput 
#' @importFrom stats pnorm
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
#' @param s Price of the underlying asset
#' @param v Volatility of the asset price, defined as the annualized
#'     standard deviation of the continuously-compounded return
#' @param r Annual continuously-compounded risk-free interest rate
#' @param tt Time to maturity in years
#' @param d Dividend yield, annualized, continuously-compounded
#' @param m number of intermediate prices in each simulated path
#' @param n number of simulated price paths
#' 
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
#' st = simprice(s0, k, v, r, tt, d, 1, 1000)
#' callprice = exp(-r*tt)*mean(pmax(st-k,0))
#'
#' ## following returns the same price as previous
#' assetcall(s, k, v, r, tt, d) - k*cashcall(s, k, v, r, tt, d)
#'
#' ## return option prices for different strikes
#' bsput(s, k=38:42, v, r, tt, d)

#' @export
simprice <- function(s0, v, r, tt, d, m, n, 
                     lambda = 0, alphaj = 0, vj = 0,
                     jump = FALSE, seed = NULL,
                     dataframe = FALSE, long = TRUE) {
    if (!is.null(seed)) set.seed(seed)
    ## set.seed(1)
    ##s0=40; k=40; v=0.30; r=0.08; tt=0.25; d=0; ;m <- 2;
    ##
    ## n <- 3; lambda=2; alphaj=-.2; vj=.4; jump=TRUE
    ##
    ## a row of dimension m is a simulated stock price path
    h <- tt/m
    k <- exp(alphaj) - 1 
    zc <- matrix(rnorm(m*n), nrow = n, ncol = m)
    ## apply function output differs for vectors and dataframes
    if (m != 1) zc <- t(apply(zc, 1, cumsum))
##    if (m != 1) {
##        zc <- apply(zc, 1, cumsum)
##        zc <- t(zc)
##    }
    hmat <- matrix(rep(1:m, times = n), nrow = n, ncol = m, byrow = TRUE)
    log_s <- log(s0) + (r - d - jump*k*lambda - 0.5*v^2)*h*hmat + v*sqrt(h)*zc
    if (jump) {
        lambda <- lambda
        nj <- matrix(rpois(m*n, lambda = lambda*h), nrow = n, ncol = m)
        zj <- matrix(rnorm(m*n), nrow = n, ncol = m)
        jumpfactor <- (alphaj-0.5*vj^2)*nj + vj*sqrt(nj)*zj
        jumpfactor <- apply(jumpfactor, 1, cumsum)
        if (m != 1) jumpfactor <- t(jumpfactor)
        log_s <- log_s + jumpfactor
    }
    s <- exp(log_s)
    s <- data.frame(trial = 1:n, s)
    colnames(s)[2:ncol(s)] <- paste0('h', 1:m)
    if (long) {
        slong <- stats::reshape(s,
                                direction = 'long',
                                varying = colnames(s)[-1],
                                v.names = 'price',
                                timevar = 'period',
                                    idvar = 'trial',
                                new.row.names = NULL)
        return(slong)
    } else return(s)
}

