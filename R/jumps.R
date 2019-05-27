#' @title Option pricing with jumps
#'
#' @description The functions \code{cashjump}, \code{assetjump}, and
#'     \code{mertonjump} return call and put prices, as vectors named
#'     "Call" and "Put", or "Call1", "Call2", etc. in case inputs are
#'     vectors. The pricing model is the Merton jump model, in which
#'     jumps are lognormally distributed. 
#'
#'
#' @seealso McDonald, Robert L., \emph{Derivatives Markets}, 3rd Edition
#'     (2013) Chapter 24
#'
#' @name jumps
#'
#' @aliases assetjump cashjump  mertonjump
#'
#' @return A vector of call and put prices computed using the Merton
#'     lognormal jump formula.
#'
#' @usage
#' assetjump(s, k, v, r, tt, d, lambda, alphaj, vj, complete)
#' cashjump(s, k, v, r, tt, d, lambda, alphaj, vj, complete)
#' mertonjump(s, k, v, r, tt, d, lambda, alphaj, vj, complete)
#'
#'
#' @param s Stock price
#' @param k Strike price of the option
#' @param v Volatility of the stock, defined as the annualized
#'     standard deviation of the continuously-compounded return
#' @param r Annual continuously-compounded risk-free interest rate
#' @param tt Time to maturity in years
#' @param d Dividend yield, annualized, continuously-compounded
#' @param lambda Poisson intensity: expected number of jumps per year
#' @param alphaj Mean change in log price conditional on a jump
#' @param vj Standard deviation of change in log price conditional on
#'     a jump
#' @param complete Return inputs along with prices, all in a data frame
#' @details Returns a scalar or vector of option prices, depending on
#' the inputs
#' @importFrom stats dpois ppois
#' 
#' @seealso bscall bsput
#'
#' @examples
#' s <- 40; k <- 40; v <- 0.30; r <- 0.08; tt <- 2; d <- 0;
#' lambda <- 0.75; alphaj <- -0.05; vj <- .35;
#' bscall(s, k, v, r, tt, d)
#' bsput(s, k, v, r, tt, d)
#' mertonjump(s, k, v, r, tt, d, 0, 0, 0)
#' mertonjump(s, k, v, r, tt, d, lambda, alphaj, vj)
#'
#' ## following returns the same price as previous
#' c(1, -1)*(assetjump(s, k, v, r, tt, d, lambda, alphaj, vj) -
#' k*cashjump(s, k, v, r, tt, d, lambda, alphaj, vj))
#'
#' ## return call prices for different strikes
#' kseq <- 35:45
#' cp <- mertonjump(s, kseq, v, r, tt, d, lambda, alphaj,
#'     vj)$Call
#'
#' ## Implied volatilities: Compute Black-Scholes implied volatilities
#' ## for options priced using the Merton jump model
#' vimp <- sapply(1:length(kseq), function(i) bscallimpvol(s, kseq[i],
#'     r, tt, d, cp[i]))
#' plot(kseq, vimp, main='Implied volatilities', xlab='Strike',
#'     ylab='Implied volatility', ylim=c(0.30, 0.50))



#' @export
cashjump <- function(s, k, v, r, tt, d, lambda, alphaj, vj,
                      complete = FALSE) {
    .mertonjump(s = s, k = k, v = v, r = r, tt = tt, d = d,
               lambda = lambda, alphaj = alphaj, vj = vj,
               complete = complete, fn = c(cashcall, cashput))
}

#' @export
assetjump <- function(s, k, v, r, tt, d, lambda, alphaj, vj,
                      complete = FALSE) {
    .mertonjump(s, k, v, r, tt, d, lambda, alphaj, vj, complete,
               fn = c(assetcall, assetput))
}

#' @export
mertonjump <- function(s, k, v, r, tt, d, lambda, alphaj, vj,
                       complete = FALSE) {
    .mertonjump(s, k, v, r, tt, d, lambda, alphaj, vj, complete,
               fn = c(bscall, bsput))
}


.mertonjump <- function(s, k, v, r, tt, d, lambda, alphaj, vj,
                        complete = FALSE, fn = c(bscall, bsput)) {
    Call <- .jumpprice(s, k, v, r, tt, d, lambda, alphaj, vj,
                       fn[[1]])
    Put <- .jumpprice(s, k, v, r, tt, d, lambda, alphaj, vj,
                      fn[[2]])
    if (complete) {
        return(data.frame(s = s, k = k, v = v, r = r, tt = tt, d = d,
                          lambda = lambda, alphaj = alphaj,
                          vj = vj, Call=Call, Put=Put)) 
    } else {
        return(data.frame(Call = Call,  Put = Put))
    }
}



.jumpprice <- function(s, k, v, r, tt, d, lambda, alphaj, vj,
                       pricingfunc) {
    eps <- 1e-12
    w <- expand.grid(s = s, k = k, v = v, r = r, tt = tt, d = d,
                     lambda = lambda, alphaj = alphaj, vj = vj)
    w <- data.frame(s = s, k = k, v = v, r = r, tt = tt, d = d,
                     lambda = lambda, alphaj = alphaj, vj = vj)
    w$lambdap <-  w$lambda * exp(w$alphaj)
    price <-
        with(w,
             if (all(ppois(0, lambdap*tt) > 1-eps)) {
                 price <- pricingfunc(s, k, v, r, tt, d)
             } else {
                 ##print(ppois(0, lambdap*tt))
                 cum <- 0.0
                 i <- 0
                 kappa  <-  exp(alphaj) - 1
                 while (all(ppois(i,lambdap*tt) <= 1-eps)) {
                     p <- dpois(i,lambdap * tt)
                     vp <- (v^2 + i*vj^2 / tt)^(0.5)
                     rp <- r - lambda * kappa + i * alphaj / tt
                     cum <- cum + p * pricingfunc(s, k, vp, rp, tt, d)
                     i <- i + 1
                   ##  print(i)
                 }
                 price <- cum
             })
    return(price)
}
