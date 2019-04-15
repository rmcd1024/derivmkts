#' @title Simulate asset prices
#'
#' @description \code{simprice} computes simulated lognormal price
#'     paths, with or without jumps. Saves and restores random number
#'     seed.
#'
#' \code{simprice(s0, v, r, tt, d, trials, periods = 1,  jump = FALSE,
#' lambda = 0, alphaj = 0, vj = 0, seed = NULL, long = TRUE)}
#'
#' @name simprice
#' @importFrom stats rnorm rpois
#' @return A dataframe with \code{trials} simulated stock price paths
#'
#' @usage simprice(s0, v, r, tt, d, trials, periods=1, jump=FALSE,
#'     lambda=0, alphaj=0, vj=0, seed=NULL, long=TRUE)
#'
#' @param s0 Initial price of the underlying asset
#' @param v Volatility of the asset price, defined as the annualized
#'     standard deviation of the continuously-compounded return
#' @param r Annual continuously-compounded risk-free interest rate
#' @param tt Time to maturity in years
#' @param d Dividend yield, annualized, continuously-compounded
#' @param periods number of equal-length periods in each simulated
#'     path
#' @param trials number of simulated price paths
#' @param jump boolean controlling use of jump parameters
#' @param lambda expected number of jumps in one year
#'     (\code{lambda*tt}) is the Poisson parameter
#' @param alphaj Expected continuously compounded jump percentage
#' @param vj lognormal volatility of the jump amount
#' @param seed random number seed
#' @param long if \code{TRUE}, return a long-form dataframe with
#'     columns indicating the price, trial, and period. If
#'     \code{FALSE}, the returned data is wide, containing only
#'     prices: each row is a trial and each column is a period
#' 
#' 
#' @examples
#' # simple Monte Carlo option price example. Since there are two
#' # periods we can compute options prices for \code{tt} and
#' # \code{tt/2}
#' s0=40; k=40; v=0.30; r=0.08; tt=0.25; d=0;
#' st = simprice(s0, k, v, r, tt, d, periods=2, trials=3)
#' callprice1 = exp(-r*tt/2)*mean(pmax(st[st$period==1,] - k, 0))
#' callprice2 = exp(-r*tt)*mean(pmax(st[st$period==2,] - k, 0))
#' 
#' 
#' @export
simprice <- function(s0, v, r, tt, d,  trials, periods = 1,
                     jump = FALSE, lambda = 0, alphaj = 0, vj = 0,
                     seed = NULL, long = TRUE) {
    if (exists(".Random.seed")) {
        oldseed <- .Random.seed
        savedseed <- TRUE
    } else {
        savedseed <- FALSE
    }
    if (!is.null(seed)) set.seed(seed)
    ## set.seed(1)
    ##s0=40; k=40; v=0.30; r=0.08; tt=0.25; d=0; ;periods <- 2;
    ##
    ## trials <- 3; lambda=2; alphaj=-.2; vj=.4; jump=TRUE
    ##
    ## a row of dimension m is a simulated stock price path
    h <- tt/periods
    k <- exp(alphaj) - 1 
    zc <- matrix(rnorm(periods*trials), nrow = trials, ncol = periods)
    ## apply function output differs for vectors and dataframes
    if (periods != 1) zc <- t(apply(zc, 1, cumsum))
    hmat <- matrix(rep(1:periods, times = trials), nrow = trials,
                   ncol = periods, byrow = TRUE)
    log_s <- log(s0) + (r - d - jump*k*lambda - 0.5*v^2)*h*hmat + v*sqrt(h)*zc
    if (jump) {
        lambda <- lambda
        nj <- matrix(rpois(periods*trials, lambda = lambda*h),
                     nrow = trials, ncol = periods)
        zj <- matrix(rnorm(periods*trials), nrow = trials, ncol = periods)
        jumpfactor <- (alphaj-0.5*vj^2)*nj + vj*sqrt(nj)*zj
        jumpfactor <- apply(jumpfactor, 1, cumsum)
        if (periods != 1) jumpfactor <- t(jumpfactor)
        log_s <- log_s + jumpfactor
    } else {
        nj <- matrix(0, nrow = trials,  ncol = periods)
    }
    if (savedseed) .Random.seed <- oldseed
    if (long == FALSE) {
        s <- data.frame(exp(log_s))
        colnames(s) <- paste0('h', 1:periods)
        return(s)
    } else {
        print(paste(dim(nj),  dim(log_s) ))
        s <- data.frame(trial = 1:trials, njump = nj, smat = exp(log_s))
        slong <- stats::reshape(s,
                                direction = 'long',
                                varying = list(grep('njump', names(s), value = TRUE),
                                               grep('smat', names(s), value = TRUE)),
                                ##varying = colnames(s)[-1],
                                v.names = c('numjumps', 'price'),
                                timevar = 'period',
                                idvar = 'trial',
                                new.row.names = NULL)
        return(slong[order(slong$trial, slong$period), ])
    } 
}

