#' @title Perpetual American options
#'
#' @description \code{callperpetual} and \code{putperpetual} compute
#'     prices of perpetual American options. The functions optionally
#'     return the exercise barriers (the prices at which the options
#'     are optimally exercised).
#'
#' @note If the dividend yield is zero, a perpetual call is never
#'     exercised. The pricing function in this case will return the
#'     stock price, which is the limiting option price as the dividend
#'     yield goes to zero.  Similarly, if the risk-free rate is zero,
#'     a perpetual put is never exercised. The pricing function will
#'     return the strike price in this case, which is the limiting
#'     value of the pricing function as the interest rate approaches
#'     zero.
#'
#' @name perpetual
#' @aliases callperpetual putperpetual
#' @return Option price, and optionally the optimal exercise barrier.
#' 
#' @usage
#' callperpetual(s, k, v, r, d, showbarrier)
#' putperpetual(s, k, v, r, d, showbarrier)
#'
#' @inheritParams blksch
#' 
#' @param showbarrier Boolean (FALSE). If TRUE, the option price and
#'     exercise barrier are returned as a list
#' 
#' @details Returns a scalar or vector of option prices, depending on
#' the inputs
#'
#' \code{callperpetual(s, k, v, r, tt, d)}
#'
#'
#' @examples
#' s=40; k=40; v=0.30; r=0.08;  d=0.02;
#' callperpetual(s, k, v, r, d)
#'
#' putperpetual(s, c(35, 40, 45), v, r, d, showbarrier=TRUE)
#'
#'

#' @export
callperpetual <- function(s, k, v, r, d, showbarrier=FALSE) {
    tmp <- data.frame(s, k, v, r, d)
    for (i in names(tmp)) assign(i, tmp[, i])
    g <- (((r - d) / v^2 - 0.5)^2 + 2*r /v^2)^0.5
    h1 <- 0.5 - (r-d) /v^2 + g
    sbar <- k*h1/(h1-1)
    val <- (pmax(sbar, s) - k)*ur(s=s, v=v, r=r, tt=1, d=d, H=sbar,
                                  perpetual=TRUE)
    val <- ifelse(d < 1e-13 & r > 0, s, val)
    ifelse(showbarrier,
           return(list(price=val, barrier=sbar)),
           return(price=val))
}

#' @export
putperpetual <- function(s, k, v, r, d, showbarrier=FALSE) {
    tmp <- data.frame(s, k, v, r, d)
    for (i in names(tmp)) assign(i, tmp[, i])
    g <- (((r - d) / v^2 - 0.5)^2 + 2*r /v^2)^0.5
    h2 <- 0.5 - (r-d) /v^2 - g
    sbar <- k*h2 / (h2-1)
    val <- (k - pmin(s, sbar))*dr(s=s, v=v, r=r, tt=1, d=d, H=sbar,
                                  perpetual=TRUE)
    val <- ifelse(r < 1e-13 & d > 0, k, val)
    ifelse(showbarrier,
           return(list(price=val, barrier=sbar)),
           return(price=val)
           )
}


