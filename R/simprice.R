#' @title Simulate asset prices
#'
#' @description \code{simprice} computes simulated lognormal price
#'     paths, with or without jumps.
#'
#' \code{simprice(s0, v, r, tt, d, m, n, jump = FALSE, lambda = 0,
#' alphaj = 0, vj = 0, seed = NULL, long = TRUE)}
#'
#' @name simprice
#' @importFrom stats rnorm rpois
#' @return A dataframe with `n` simulated stock price paths
#'
#' @usage
#' simprice(s, k, v, r, tt, d, m, n, jump=FALSE)
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
#' @param m number of equal-length periods in each simulated path
#' @param n number of simulated price paths
#' @param jump boolean controlling use of jump parameters
#' @param lambda expected number of jumps in one year
#'     (\code{lambda*tt}) is the Poisson parameter
#' @param alphaj Expected continuously compounded jump percentage
#' @param vj lognormal volatility of the jump amount
#' @param seed random number seed
#' @param long if \code{TRUE}, return a long-form dataframe with
#'     columns indicating the price, trial, and period. If
#'     \code{FALSE}, the returned data is wide: each row is a trial
#'     and each column is a period
#' 
#' 
#' @examples
#' # simple Monte Carlo option price example. Since there are two
#' # periods we can compute options prices for \code{tt} and
#' # \code{tt/2}
#' s=40; k=40; v=0.30; r=0.08; tt=0.25; d=0;
#' st = simprice(s0, k, v, r, tt, d, m=2, n=3)
#' callprice1 = exp(-r*tt/2)*mean(pmax(st[st$period==1] - k, 0))
#' callprice2 = exp(-r*tt)*mean(pmax(st[st$period==2] - k, 0))
#' 
#' @inheritParams bscall jumps
#' 
#' 
#' @export
simprice <- function(s0, v, r, tt, d, m, n, jump = FALSE, 
                     lambda = 0, alphaj = 0, vj = 0,
                     seed = NULL, long = TRUE) {
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
    s <- data.frame(trial = 1:n, exp(log_s))
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

