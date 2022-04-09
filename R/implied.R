#' @title Black-Scholes implied volatility and price
#'
#' @description \code{bscallimpvol} and \code{bsputimpvol} compute
#' Black-Scholes implied volatilties. The functions \code{bscallimps}
#' and \code{bsputimps}, compute stock prices implied by a given
#' option price, volatility and option characteristics.
#'
#' @name implied
#' @aliases bscallimpvol bsputimpvol bscallimps bsputimps
#'
#' @return Implied volatility (for the "impvol" functions) or implied
#' stock price (for the "impS") functions.
#'
#' @importFrom stats uniroot
#'
#' @usage
#' bscallimpvol(s, k, r, tt, d, price, lowvol, highvol,
#' .tol=.Machine$double.eps^0.5)
#' bsputimpvol(s, k, r, tt, d, price, lowvol, highvol,
#' .tol=.Machine$double.eps^0.5)
#' bscallimps(s, k, v, r, tt, d, price, lower=0.0001, upper=1e06,
#' .tol=.Machine$double.eps^0.5)
#' bsputimps(s, k, v, r, tt, d, price, lower=0.0001, upper=1e06,
#' .tol=.Machine$double.eps^0.5)
#'
#'
#' @param s Stock price
#' @param k Strike price of the option
#' @param v Volatility of the stock, defined as the annualized
#' standard deviation of the continuously-compounded return
#' @param r Annual continuously-compounded risk-free interest rate
#' @param tt Time to maturity in years
#' @param d Dividend yield, annualized, continuously-compounded
#' @param price Option price when computing an implied value
#' @param lower minimum stock price in implied price calculation
#' @param upper maximum stock price in implied price calculation
#' @param highvol maximum implied volatility
#' @param lowvol minimum implied volatility
#' @param .tol numerical tolerance for zero-finding function `uniroot`
#' @details Returns a scalar or vector of option prices, depending on
#' the inputs
#'

#' @note Implied volatilties and stock prices do not exist if the
#' price of the option exceeds no-arbitrage bounds. For example, if
#' the interest rate is non-negative, a 40 strike put cannot have a
#' price exceeding $40.
#'
#' @examples
#' s=40; k=40; v=0.30; r=0.08; tt=0.25; d=0;
#' bscallimpvol(s, k, r, tt, d, 4)
#' bsputimpvol(s, k, r, tt, d, 4)
#' bscallimps(s, k, v, r, tt, d, 4, )
#' bsputimps(s, k, v, r, tt, d, 4)
#'

#' @export
bscallimpvol <- function(s, k, r, tt, d, price, lowvol=0.0001,
                         highvol=1e06, .tol=.Machine$double.eps^0.5) {
    ## this function is not vectorized
    if (price <= s*exp(-d*tt)-k*exp(-r*tt)) {
        print("Option price violates minimum bound")
    } else if (price > s*exp(-d*tt)) {
        print("Option price violates maximum bound")
    } else {
        f <- function(v, s, k, r, tt, d, price) {
            return(bscall(s, k, v, r, tt, d) - price)
        }
        x <- uniroot(f, c(lowvol,highvol), s, k, r, tt, d, price, tol=.tol)
        return(x$root)
    }
}

## add tolerance!
#' @export
bsputimpvol <- function(s, k, r, tt, d, price, lowvol=0.0001,
                        highvol=1e06, .tol=.Machine$double.eps^0.5) {
    ## this function is not vectorized
    x <- bscallimpvol(s, k, r, tt, d,
                      price + s*exp(-d*tt) - k*exp(-r*tt), lowvol,
                      highvol, .tol)
    return(x)
}

#' @export
bscallimps <- function(s, k, v, r, tt, d, price, lower=0.0001,
                       upper=1e06, .tol=.Machine$double.eps^0.5) {
    f <- function(s, k, v, r, tt, d, price) {
        return(bscall(s, k, v, r, tt, d) - price)
    }
    x <- uniroot(f, c(lower, upper), k, v, r, tt, d, price,
                 tol=.Machine$double.eps^0.5)
    return(x$root)
}

#' @export
bsputimps <- function(s, k, v, r, tt, d, price, lower=0.0001,
                      upper=1e06, .tol=.Machine$double.eps^0.5) {
    f <- function(s, k, v, r, tt, d, price) {
        return(bsput(s, k, v, r, tt, d) - price)
    }
    x <- uniroot(f, c(lower, upper), k, v, r, tt, d, price, tol=.tol)
    return(x$root)
}

