#' @title Compound options
#'
#' @name compound
#'
#' @description Compound options are options on options. The
#'     definition of a compound option requires specifying the nature
#'     of the options, two dates, and two strikes. Specifically, you
#'     need to know
#'
#' \itemize{
#'
#'    \item{whether you have the right to buy or sell an underlying option}
#'
#'    \item{whether the underlying option (the option upon which there
#'    is an option) is a put or a call}
#'
#'    \item{the price at which you can buy or sell the underlying
#' option (strike price \code{kco} --- the strike on the compound option)}
#'
#'    \item{the price at which you can buy or sell the underlying
#' asset should you exercise the compound option (strike price
#' \code{kuo} --- the strike on the underlying option)}
#'
#'
#'    \item{the date at which you have the option to buy or sell the
#'    underlying option (first exercise date, \code{t1})}
#'
#'    \item{the date at which the underlying option expires, \code{t2}}
#'
#' }
#'
#' Given these possibilities, you can have a call on a call, a put on
#'  a call, a call on a put, and a put on a put. The valuation
#'  procedure require knowing, among other things, the underlying
#'  asset price at which it will be worthwhile to acquire the
#'  underlying option.
#'
#' Given the underlying option, there is a parity relationship: If you
#' buy a call on a call and sell a call on a call, you have acquired
#' the underlying call by paying the present value of the strike,
#' \code{kco}.
#'
#' @aliases binormsdist optionsoncall optionsonput
#' @usage
#' binormsdist(x1, x2, rho)
#' optionsoncall(s, kuo, kco, v, r, t1, t2, d)
#' optionsonput(s, kuo, kco, v, r, t1, t2, d)
#'
#'
#'
#' @param s Price of underlying asset
#' @param v Volatility of the underlying asset, defined as the
#'     annualized standard deviation of the continuously-compounded
#'     return
#' @param r Annual continuously-compounded risk-free interest rate
#' @param d Dividend yield of the underlying asset, annualized,
#'     continuously-compounded
#' @param kuo strike on underlying option
#' @param kco strike on compound option (the price at which you would
#'     buy or sell the underlying option at time t1)
#' @param t1 time until exercise for the compound option
#' @param t2 time until exercise for the underlying option
#' @param x1,x2 values at which the cumulative bivariate normal
#'     distribution will be evaluated
#' @param rho correlation between \code{x1} and \code{x2}
#' @inheritParams bscall
#'
#' @importFrom stats uniroot
#' @importFrom mnormt pmnorm
#'


library(mnormt)

tol <- 1e07

#' @export
binormsdist <- function(x1, x2, rho) {
    pmnorm(c(x1, x2), varcov = matrix(c(1, rho, rho, 1), nrow=2))
}

###' @export
##bscallimps <- function(s, k, v, r, tt, d, price) {
##    tol <- 1e-07
##    f <- function(s, k, v, r, tt, d, price) {
##        return(bscall(s, k, v, r, tt, d) - price)
##    }
##    x <- uniroot(f, c(0.001,1000), k, v, r, tt, d, price, tol=tol)
##    return(x$root)
##}
##
###' @export
##bsputimps <- function(s, k, v, r, tt, d, price) {
##    f <- function(s, k, v, r, tt, d, price) {
##        return(bsput(s, k, v, r, tt, d) - price)
##    }
##    x <- uniroot(f, c(0.001,1000), k, v, r, tt, d, price, tol=tol)
##    return(x$root)
##}
##

#' @export
#' @inheritParams blksch
#'
optionsoncall <- function(s, kuo, kco, v, r, t1, t2, d) {
    SCritical <- bscallimps(s, kuo, v, r, t2 - t1, d, kco)
    a1 <- .d1(s, SCritical, v, r, t1, d)
    a2 <- a1 - v * t1 ^ 0.5
    d1 <- .d1(s, kuo, v, r, t2, d)
    d2 <- d1 - v * t2 ^ 0.5
    cc <- (s * exp(-d * t2) * binormsdist(a1, d1, (t1 / t2) ^ 0.5)
             - kuo * exp(-r * t2) * binormsdist(a2, d2, (t1 / t2) ^ 0.5)
        - kco * exp(-r * t1) * pnorm(a2))
    pc <- cc - bscall(s, kuo, v, r, t2, d) + kco*exp(-r*t1)
    return(c(calloncall=cc, putoncall=pc, scritical=SCritical))
    ## cc - pc = call - pv(kco)
    ## cp - pp = put - pv(kco)
    ## cc - pc - (cp - pp) = call - pv(kco) -put + pv(kco)
    ##
}

#' @export
#' @inheritParams blksch
#'
optionsonput <- function(s, kuo, kco, v, r, t1, t2, d) {
    SCritical <- bsputimps(s, kuo, v, r, t2 - t1, d, kco)
    a1 <- .d1(s, SCritical, v, r, t1, d)
    a2 <- a1 - v * t1 ^ 0.5
    d1 <- d1(s, kuo, v, r, t2, d)
    d2 <- d1 - v * t2 ^ 0.5
    cp <- (-s * exp(-d * t2) * binormsdist(-a1, -d1, (t1 / t2) ^ 0.5)
        + kuo * exp(-r * t2) * binormsdist(-a2, -d2, (t1 / t2) ^ 0.5)
        - kco * exp(-r * t1) * pnorm(-a2))
    pp <- cp - bsput(s, kuo, v, r, t2, d) + kco*exp(-r*t1)
    return(c(callonput=cp, putonput=pp, scritical=SCritical))
}

###' @export
##calloncall <- function(s, kuo, kco, v, r, t1, t2, d) {
##    SCritical <- bscallimps(s, kuo, v, r, t2 - t1, d, kco)
##    a1 <- .d1(s, SCritical, v, r, t1, d)
##    a2 <- a1 - v * t1 ^ 0.5
##    d1 <- .d1(s, kuo, v, r, t2, d)
##    d2 <- d1 - v * t2 ^ 0.5
##    temp <- (s * exp(-d * t2) * binormsdist(a1, d1, (t1 / t2) ^ 0.5)
##             - kuo * exp(-r * t2) * binormsdist(a2, d2, (t1 / t2) ^ 0.5)
##             - kco * exp(-r * t1) * pnorm(a2))
##    return(c(price=temp, scritical=SCritical))
##}
##
###' @export
##putoncall <- function(s, kuo, kco, v, r, t1, t2, d) {
##    SCritical <- bscallimps(s, kuo, v, r, t2 - t1, d, kco)
##    a1 <- .d1(s, SCritical, v, r, t1, d)
##    a2 <- a1 - v * t1 ^ 0.5
##    d1 <- .d1(s, kuo, v, r, t2, d)
##    d2 <- d1 - v * t2 ^ 0.5
##    temp <- (-s * exp(-d * t2) * binormsdist(-a1, d1, -(t1 / t2) ^ 0.5)
##             + kuo * exp(-r * t2) * binormsdist(-a2, d2, -(t1 / t2) ^ 0.5)
##             + kco * exp(-r * t1) * pnorm(-a2))
##    return(c(price=temp, scritical=SCritical))
##}
##
###' @export
##callonput <- function(s, kuo, kco, v, r, t1, t2, d) {
##    SCritical <- bsputimps(s, kuo, v, r, t2 - t1, d, kco)
##    a1 <- .d1(s, SCritical, v, r, t1, d)
##    a2 <- a1 - v * t1 ^ 0.5
##    d1 <- .d1(s, kuo, v, r, t2, d)
##    d2 <- d1 - v * t2 ^ 0.5
##    temp <- (-s * exp(-d * t2) * binormsdist(-a1, -d1, (t1 / t2) ^ 0.5)
##             + kuo * exp(-r * t2) * binormsdist(-a2, -d2, (t1 / t2) ^ 0.5)
##             - kco * exp(-r * t1) * pnorm(-a2))
##    return(c(price=temp, scritical=SCritical))
##}
##
###' @export
##putonput <- function(s, kuo, kco, v, r, t1, t2, d) {
##    SCritical <- bsputimps(s, kuo, v, r, t2 - t1, d, kco)
##    a1 <- .d1(s, SCritical, v, r, t1, d)
##    a2 <- a1 - v * t1 ^ 0.5
##    d1 <- .d1(s, kuo, v, r, t2, d)
##    d2 <- d1 - v * t2 ^ 0.5
##    temp <- (s * exp(-d * t2) * binormSDist(a1, -d1, -(t1 / t2) ^ 0.5)
##             - kuo * exp(-r * t2) * binormSDist(a2, -d2, -(t1 / t2) ^ 0.5)
##             + kco * exp(-r * t1) * pnorm(a2))
##    return(c(price=temp, scritical=SCritical))
##}
##
