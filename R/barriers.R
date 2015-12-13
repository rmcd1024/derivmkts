#' @title Barrier option pricing
#'
#' @description This library provides a set of barrier binary options
#' that are used to construct prices of barrier options. The
#' nomenclature is that
#'
#' \itemize{
#' \item "call" and "put" refer to claims that are exercised when the
#' asset price is above or below the strike;
#'
#' \item "up" and "down" refer to claims for which the barrier is above or
#' below the current asset price; and
#'
#' \item "in" and "out" refer to claims that knock in or out
#' }
#'
#' For example, for standard barrier options, \code{calldownin} refers
#' to a knock-in call for which the barrier is below the current
#' price, while \code{putdownout} refers to a knock-out put for which
#' the barrier is below the current asset price.
#'
#' For binary barrier options, "ui", "di" "uo", and "do" refer to
#' up-and-in, down-and-in, up-and-out, and down-and-out options.
#'
#' Rebate options pay \$1 if a barrier is reached. The barrier can be
#' reached from above ("d") or below ("d"), and the payment can occur
#' immediately ("ur" or "dr") or at expiration ("drdeferred" and
#' "urdeferred")
#' 
#' \code{callupin(s, k, v, r, tt, d, H) = assetuicall(s, k, v, r, tt,
#'   d, H) - k*cashuicall(s, k, v, r, tt, d, H)}
#'
#' @name barriers
#' @aliases callupin callupout putupin putupout calldownin calldownout
#'     putdownin putdownout uicall uocall dicall docall uiput uoput
#'     diput doput cashuicall cashuiput cashdicall cashdiput
#'     assetuicall assetuiput assetdicall assetdiput cashuocall
#'     cashuoput cashdocall cashdoput assetuocall assetuoput
#'     assetdocall assetdoput dr ur drdeferred urdeferred
#'
#' @return The pricing functions return the price of a barrier
#' claim. If more than one argument is a vector, the recycling rule
#' determines the handling of the inputs. 
#'
#' @usage
#' callupin(s, k, v, r, tt, d, H)
#' callupout(s, k, v, r, tt, d, H)
#' putupin(s, k, v, r, tt, d, H)
#' putupout(s, k, v, r, tt, d, H)
#' calldownin(s, k, v, r, tt, d, H)
#' calldownout(s, k, v, r, tt, d, H)
#' putdownin(s, k, v, r, tt, d, H)
#' putdownout(s, k, v, r, tt, d, H)
#' uicall(s, k, v, r, tt, d, H)
#' uocall(s, k, v, r, tt, d, H)
#' dicall(s, k, v, r, tt, d, H)
#' docall(s, k, v, r, tt, d, H)
#' uiput(s, k, v, r, tt, d, H)
#' uoput(s, k, v, r, tt, d, H)
#' diput(s, k, v, r, tt, d, H)
#' doput(s, k, v, r, tt, d, H)
#' cashuicall(s, k, v, r, tt, d, H)
#' cashuiput(s, k, v, r, tt, d, H)
#' cashdicall(s, k, v, r, tt, d, H)
#' cashdiput(s, k, v, r, tt, d, H)
#' assetuicall(s, k, v, r, tt, d, H)
#' assetuiput(s, k, v, r, tt, d, H)
#' assetdicall(s, k, v, r, tt, d, H)
#' assetdiput(s, k, v, r, tt, d, H)
#' cashuocall(s, k, v, r, tt, d, H)
#' cashuoput(s, k, v, r, tt, d, H)
#' cashdocall(s, k, v, r, tt, d, H)
#' cashdoput(s, k, v, r, tt, d, H)
#' assetuocall(s, k, v, r, tt, d, H)
#' assetuoput(s, k, v, r, tt, d, H)
#' assetdocall(s, k, v, r, tt, d, H)
#' assetdoput(s, k, v, r, tt, d, H)
#' dr(s, v, r, tt, d, H)
#' ur(s, v, r, tt, d, H)
#' drdeferred(s, v, r, tt, d, H)
#' urdeferred(s, v, r, tt, d, H)
#'
#' 
#' @param s Stock price
#' @param k Strike price of the option
#' @param v Volatility of the stock, defined as the annualized
#' standard deviation of the continuously-compounded return
#' @param r Annual continuously-compounded risk-free interest rate
#' @param tt Time to maturity in years
#' @param d Dividend yield, annualized, continuously-compounded
#' @param H Barrier
#'
#' @details Returns a scalar or vector  of option prices, depending on
#' the inputs
#'
#'
#'
#' @examples
#' s=40; k=40; v=0.30; r=0.08; tt=0.25; d=0; H=44
#' callupin(s, k, v, r, tt, d, H)
#'
#' ## following returns the same price as previous
#' assetuicall(s, k, v, r, tt, d, H) - k*cashuicall(s, k, v, r, tt, d, H)
#'
#' ## return option prices for different strikes
#' putupin(s, k=38:42, v, r, tt, d, H)

cashdicall <- function(s, k, v, r, tt, d, H) {
    ifelse(s <= H, cashcall(s, k, v, r, tt, d),
           exp(-r*tt)*(.nd2(s, k, v, r, tt, d) -
                           .nd6(s, k, v, r, tt, d, pmax(k, H)) +
                               (H/s)^(2*(r-d)/(v^2)-1)*
                                   .nd8(s, k, v, r, tt, d, H*pmin(H/k, 1))))
}

assetdicall <- function(s, k, v, r, tt, d, H) {
    exp((r-d)*tt)*s*cashdicall(s, k, v, r, tt, d - v^2, H)
}

cashdocall <- function(s, k, v, r, tt, d, H) {
##    tmp <- vectorizesh(s, H)
##    s <- tmp[[1]]
##    h <- tmp[[2]]
    cashcall(s, k, v, r, tt, d) -
        cashdicall(s, k, v, r, tt, d, H)
}

assetdocall <- function(s, k, v, r, tt, d, H) {
    price <- s*exp((r-d)*tt)*
        cashdocall(s, k, v, r, tt, d-v^2, H)
}

cashdoput <- function(s, k, v, r, tt, d, H) {
##> sapply(c(35, 40, 41, 42),
##    function(x) cashdoput(x, 40.5, .3, .08, .25, 0, 38))
##[1] 0.00000000 0.02168404 0.02973113 0.03540291
##    tmp <- vectorizeskh(s, k, H)
##    s <- tmp[[1]]
##    k <- tmp[[2]]
##    h <- tmp[[3]]
    ifelse( (s<=H) | (k<=H), 0,
           cashdocall(s, H, v, r, tt, d, H) -
               cashdocall(s, k, v, r, tt, d, H)
           )
}

cashdiput <- function(s, k, v, r, tt, d, H) {
    cashput(s, k, v, r, tt, d) -
        cashdoput(s, k, v, r, tt, d, H)
}

assetdoput <- function(s, k, v, r, tt, d, H) {
    s*exp((r-d)*tt)*
        cashdoput(s, k, v, r, tt, d-v^2, H)
}

assetdiput <- function(s, k, v, r, tt, d, H) {
    s*exp((r-d)*tt)*
        cashdiput(s, k, v, r, tt, d-v^2, H)
}

calldownin <- function(s, k, v, r, tt, d, H) {
    assetdicall(s, k, v, r, tt, d, H) -
        k*cashdicall(s, k, v, r, tt, d, H)
}

dicall <- function(s, k, v, r, tt, d, H)
    calldownin(s, k, v, r, tt, d, H) 

calldownout <- function(s, k, v, r, tt, d, H) {
    bscall(s, k, v, r, tt, d) -
        calldownin(s, k, v, r, tt, d, H)
}

docall <- function(s, k, v, r, tt, d, H)
    calldownout(s, k, v, r, tt, d, H)

putdownin <- function(s, k, v, r, tt, d, H) {
    k*cashdiput(s, k, v, r, tt, d, H) -
        assetdiput(s, k, v, r, tt, d, H)

}

diput <- function(s, k, v, r, tt, d, H)
    putdownin(s, k, v, r, tt, d, H)

putdownout <- function(s, k, v, r, tt, d, H) {
    bsput(s, k, v, r, tt, d) -
        putdownin(s, k, v, r, tt, d, H)
}

doput <- function(s, k, v, r, tt, d, H)
    putdownout(s, k, v, r, tt, d, H)


#################################################
## up barriers
#################################################

cashuiput <- function(s, k, v, r, tt, d, H) {
    ifelse(s >= H,
           cashput(s, k, v, r, tt, d),
           exp(-r*tt)*(1-.nd2(s, k, v, r, tt, d)
                       - (1 - .nd6(s, k, v, r, tt, d, pmin(k, H))
                          ) +(H/s)^(2*(r-d)/(v^2)-1)*
                              (1 - .nd8(s, k, v, r, tt, d, H*pmax(H/k, 1))))
           )
               
}


cashuoput <- function(s, k, v, r, tt, d, H) {
    cashput(s, k, v, r, tt, d) -
        cashuiput(s, k, v, r, tt, d, H)
}

cashuicall <- function(s, k, v, r, tt, d, H) {
    cashuiput(s, 1e15, v, r, tt, d, H) -
        cashuiput(s, k, v, r, tt, d, H)
}

cashuocall <- function(s, k, v, r, tt, d, H) {
    price <- cashcall(s, k, v, r, tt, d) -
        cashuicall(s, k, v, r, tt, d, H)
}

assetuiput <- function(s, k, v, r, tt, d, H) {
    s*exp((r-d)*tt)*
        cashuiput(s, k, v, r, tt, d-v^2, H)
}

assetuoput <- function(s, k, v, r, tt, d, H) {
    price <- s*exp((r-d)*tt)*
        cashuoput(s, k, v, r, tt, d-v^2, H)
    return(price)
}

assetuicall <- function(s, k, v, r, tt, d, H) {
    s*exp((r-d)*tt)*cashuicall(s, k, v, r, tt, d-v^2, H)
}
assetuocall <- function(s, k, v, r, tt, d, H) {
    s*exp((r-d)*tt)*cashuocall(s, k, v, r, tt, d-v^2, H)
}

callupin <- function(s, k, v, r, tt, d, H) {
    assetuicall(s, k, v, r, tt, d, H) -
        k*cashuicall(s, k, v, r, tt, d, H)
}

uicall <- function(s, k, v, r, tt, d, H)
    callupin(s, k, v, r, tt, d, H)


callupout <- function(s, k, v, r, tt, d, H) {
    assetuocall(s, k, v, r, tt, d, H) -
        k*cashuocall(s, k, v, r, tt, d, H)
}

uocall <- function(s, k, v, r, tt, d, H)
    callupout(s, k, v, r, tt, d, H)

putupin <- function(s, k, v, r, tt, d, H) {
    k*cashuiput(s, k, v, r, tt, d, H) -
        assetuiput(s, k, v, r, tt, d, H)
}

uiput <- function(s, k, v, r, tt, d, H)
    putupin(s, k, v, r, tt, d, H)



putupout <- function(s, k, v, r, tt, d, H) {
    k*cashuoput(s, k, v, r, tt, d, H)-
        assetuoput(s, k, v, r, tt, d, H)
}

uoput <- function(s, k, v, r, tt, d, H)
    putupout(s, k, v, r, tt, d, H)


.d3 <- function(s, k, v, r, tt, d, H) {
    (log(H^2/(s*k))+(r-d+v^2/2)*tt)/(v*sqrt(tt))
}
.d5 <- function(s, k, v, r, tt, d, H) {
    (log(s/H)+(r-d+v^2/2)*tt)/(v*sqrt(tt))
}
.d7 <- function(s, k, v, r, tt, d, H) {
    (log(H/s)+(r-d+v^2/2)*tt)/(v*sqrt(tt))
}


.d4 <- function(s, k, v, r, tt, d, H) {
   .d3(s, k, v, r, tt, d, H) - v*sqrt(tt)
}
.d6 <- function(s, k, v, r, tt, d, H) {
   .d5(s, k, v, r, tt, d, H) - v*sqrt(tt)
}

.d8 <- function(s, k, v, r, tt, d, H) {
    .d7(s, k, v, r, tt, d, H) - v*sqrt(tt)
}


.nd3 <- function(s, k, v, r, tt, d, H) {
    pnorm(.d3(s, k, v, r, tt, d, H))
}

.nd5 <- function(s, k, v, r, tt, d, H) {
    pnorm(.d5(s, k, v, r, tt, d, H))
}

.nd7 <- function(s, k, v, r, tt, d, H) {
    pnorm(.d7(s, k, v, r, tt, d, H))
}


.nd4<- function(s, k, v, r, tt, d, H) {
    pnorm(.d4(s, k, v, r, tt, d, H))
}

.nd6 <- function(s, k, v, r, tt, d, H) {
    pnorm(.d6(s, k, v, r, tt, d, H))
}

.nd8 <- function(s, k, v, r, tt, d, H) {
    pnorm(.d8(s, k, v, r, tt, d, H))
}


drdeferred <- function(s, v, r, tt, d, H) {
    cashdicall(s, 0.00000001, v, r, tt, d, H)
}

urdeferred <- function(s, v, r, tt, d, H) {
    cashuicall(s, 0.00000001, v, r, tt, d, H)
}

ur <-  function(s, v, r, tt, d, H) {
    ifelse(s >= H, 1, {
               g = (((r - d) / v^2 - 0.5)^2 + 2*r /v^2)^0.5
               z1 = (log(H/s) - g*v^2*tt)/(v*sqrt(tt))
               z2 = (log(H/s) + g*v^2*tt)/(v*sqrt(tt))
               h1 = 0.5 - (r - d) / v ^ 2 + g
               h2 = 0.5 - (r - d) / v ^ 2 - g
               (s/H)^h1*pnorm(-z1) + (s/H)^h2*pnorm(-z2)
           }
           )
}

dr <-  function(s, v, r, tt, d, H) {
    ifelse(s <= H,  1, {
               g = (((r - d) / v^2 - 0.5)^2 + 2*r /v^2)^0.5
               z1 = (log(H/s) - g*v^2*tt)/(v*sqrt(tt))
               z2 = (log(H/s) + g*v^2*tt)/(v*sqrt(tt))
               h1 = 0.5 - (r - d) / v ^ 2 + g
               h2 = 0.5 - (r - d) / v ^ 2 - g
               (s/H)^h1*pnorm(z1) + (s/H)^h2*pnorm(z2)
           }
           )
}



##vectorizesh <- function(s, H) {
##    if (length(s) > length(h)) {
##        h <- rep(h, round(length(s)/length(h)))
##    } else if (length(s) < length(h)) {
##        s <- rep(s, round(length(h)/length(s)))
##    }
##    return(list(s, H))
##}

##vectorizeskh <- function(s, k, H) {
##    w <- list(s, k, H)
##    y <- lapply(w, length)
##    maxw <- which(unlist(y)==max(unlist(y)))
##    nonmaxw <- which(unlist(y) < max(unlist(y)))
##    if (length(nonmaxw) > 0) {
##        for (i in nonmaxw) w[[i]] <-
##                rep(w[[i]], length(w[[maxw[1]]])/length(w[[i]]))
##
##    }
##    return(w)
##}
