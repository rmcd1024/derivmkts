#' @title Geometric average asian options
#'
#' @description Pricing functions for European Asian options based on
#'     geometric averages. \code{geomavgpricecall},
#'     \code{geomavgpriceput}, \code{geomavgstrikecall} and
#'     \code{geomavgstrikeput} compute analytical prices of geometric
#'     Asian options using the modified Black-Scholes formula.
#'
#' @family Asian
#' @aliases geomavgprice geomavgpricecall geomavgpriceput
#'     geomavgstrike geomavgstrikecall geomavgstrikeput
#' @name asiangeomavg
#' @param s Price of underlying asset
#' @param k Strike price of the option. In the case of average strike
#'     options, \code{k/s} is the multiplier for the average
#' @param km The strike mutiplier, relative to the initial stock
#'     price, for an average price payoff. If the initial stock price
#'     is \code{s = 120} and \code{km = 115}, the payoff for an
#'     average strike call is \deqn{Payoff = max(ST - km/s*SAvg, 0)}.
#' @param v Volatility of the underlygin asset price, defined as the
#'     annualized standard deviation of the continuously-compounded
#'     return
#' @param r Annual continuously-compounded risk-free interest rate
#' @param tt Time to maturity in years
#' @param d Dividend yield, annualized, continuously-compounded
#' @param m Number of prices in the average calculation
#' @param cont Boolean which when TRUE denotes continuous averaging
#' 
#' @examples
#' s=40; k=40; v=0.30; r=0.08; tt=0.25; d=0; m=3;
#' geomavgpricecall(s, k, v, r, tt, d, m)
#' geomavgpricecall(s, 38:42, v, r, tt, d, m)
#' geomavgpricecall(s, 38:42, v, r, tt, d, m, cont=TRUE)
#' 
#' @usage
#' geomavgprice(s, k, v, r, tt, d, m, cont=FALSE)
#' geomavgpricecall(s, k, v, r, tt, d, m, cont=FALSE)
#' geomavgpriceput(s, k, v, r, tt, d, m, cont=FALSE)
#' geomavgstrike(s, km, v, r, tt, d, m, cont=FALSE)
#' geomavgstrikecall(s, km, v, r, tt, d, m, cont=FALSE)
#' geomavgstrikeput(s, km, v, r, tt, d, m, cont=FALSE)
#' @return Option prices as a vector


#' @export
geomavgprice <- function(s, k, v, r, tt, d, m, cont=FALSE) {
    siga <- cont*v / (3^0.5) +
        (1-cont)*v * ((m + 1) * ((2 * m) + 1) / 6)^0.5 / m
    da <- cont*0.5 * (r + d + v^2 / 6) +
        (1-cont)*0.5 * (r * (m - 1) / m + (d + 0.5 * v^2) *
                        (m + 1) / m - (v / m)^2 *
                                              (m + 1) * (2 * m + 1) / 6)
    return(c(Call=bscall(s, k, siga, r, tt, da),
             Put=bsput(s, k, siga, r, tt, da)))
}

#' @export
geomavgpricecall <- function(s, k, v, r, tt, d, m, cont=FALSE) {
    siga <- .siga(v, m, cont)
    da <- .da(r, d, v, m, cont)
    bscall(s, k, siga, r, tt, da)
}

#' @export
geomavgpriceput <- function(s, k, v, r, tt, d, m, cont=FALSE) {
    siga <- .siga(v, m, cont)
    da <- .da(r, d, v, m, cont)
    bsput(s, k, siga, r, tt, da)
}


#' @export
geomavgstrike <- function(s, km, v, r, tt, d, m, cont=FALSE) {
    siga <- cont * v / (3 ^ 0.5) +
        (1-cont) * v * ((m + 1) * (2 * m + 1) / 6) ^ 0.5 / m
    da <- cont * 0.5 * (r + d + v ^ 2 / 6) +
        (1-cont) * 0.5 * (r * (m - 1) / m + (d + 0.5 * v ^ 2) * (m + 1) / m -
                          (v / m) ^ 2 * (m + 1) * (2 * m + 1) / 6)
    rho <- cont * 0.5 * (3 ^ 0.5) +
                (1-cont) * 0.5 * (6 * (m + 1) / (2 * m + 1)) ^ 0.5
    vol <-  (siga ^ 2 + v ^ 2 - 2 * rho * siga * v) ^ 0.5
    return(c(Call=bscall(s, km, vol, da, tt, d),
           Put=bsput(s, km, vol, da, tt, d)))
}


#' @export
geomavgstrikecall <- function(s, km, v, r, tt, d, m, cont=FALSE) {
    siga <- .siga(v, m, cont)
    da <- .da(r, d, v, m, cont)
    rho <- .rho(m, cont)
    vol <-  (siga ^ 2 + v ^ 2 - 2 * rho * siga * v) ^ 0.5
    bscall(s, km, vol, da, tt, d)
}

#' @export
geomavgstrikeput <- function(s, km, v, r, tt, d, m, cont=FALSE) {
    siga <- .siga(v, m, cont)
    da <- .da(r, d, v, m, cont)
    rho <- .rho(m, cont)
    vol <-  (siga ^ 2 + v ^ 2 - 2 * rho * siga * v) ^ 0.5
    bsput(s, km, vol, da, tt, d)
}


.rho <- function(m, cont) {
    cont * 0.5 * (3 ^ 0.5) +
        (1-cont) * 0.5 * (6 * (m + 1) / (2 * m + 1)) ^ 0.5
}

.da <- function(r, d, v, m, cont) {
    cont * 0.5 * (r + d + v ^ 2 / 6) +
        (1-cont) * 0.5 *
            (r * (m - 1) / m + (d + 0.5 * v ^ 2) * (m + 1) / m -
             (v / m) ^ 2 * (m + 1) * (2 * m + 1) / 6)
}

.siga <- function(v, m, cont) {
    cont*v / (3^0.5) +
        (1-cont)*v * ((m + 1) * ((2 * m) + 1) / 6)^0.5 / m
}
