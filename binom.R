#' @title Binomial option pricing
#'
#' @description
#'
#' @name binom
#'
#' @aliases
#'
#' @return
#'
#' @usage
#'
#'
#' @param s Stock price
#' @param k Strike price of the option
#' @param v Volatility of the stock, defined as the annualized
#' standard deviation of the continuously-compounded return
#' @param r Annual continuously-compounded risk-free interest rate
#' @param tt Time to maturity in years
#' @param d Dividend yield, annualized, continuously-compounded
#' 
#' @details Returns a scalar or vector of option prices, depending on
#' the inputs
#'
#' @note
#'
#' @examples
#'
#' 
#' 

BinomSimple <- function(s, k, v, r, tt, d, nStep, American = TRUE,
                        printout = FALSE, putOpt = FALSE) {
    ## set up the binomial tree paramaters
    h <- tt/nStep
    up <- exp((r-d)*h + sqrt(h)*v)
    dn <- exp((r-d)*h - sqrt(h)*v)
    p <- (exp((r-d)*h) - dn)/(up - dn)
    if (printout) {cat("up = ",up,"; dn = ",dn,"; p = ",p,"\n")}
    nn <- 0:nStep
    payoffmult <- ifelse(putOpt,-1,1)
## think about doing this with a vector rather than with a
## matrix. Maybe try both ways to see what happens
    
    A <- matrix(up^nn, nStep+1, nStep+1, byrow=TRUE)
    B <- matrix(dn^nn/up^nn, nStep+1, nStep+1) 
    SMat <- upper.tri(A, diag=TRUE)*(A*B)*s ## stock price matrix
    if (printout) {print("Stock Price Tree");print(SMat,digits=5)}
    Vc <- matrix(0, nStep+1, nStep+1)  ## Initialize opt price matrix
    Vc[, nStep+1] <- pmax(0, (SMat[, nStep+1] - k)*payoffmult)

    for (i in (nStep):1) {
        Vnc <- exp(-r*h)*(p*Vc[1:i,i+1] + (1-p)*Vc[2:(i+1),i+1])
        if (American) {
            Vc[1:i,i] <- pmax(Vnc, (SMat[1:i,i] - k)*payoffmult)
        } else {
            Vc[1:i,i] <- Vnc
        }
    }
    if (printout) {
        print(paste(ifelse(putOpt, "Put", "Call"),"Option Value:"))
        print(Vc,digits=4)
    }
    return(Vc[1,1])
}

##@@


BinomFlexible <- function(s, k, v, r, tt, d, nStep, American = TRUE,
                          printout = FALSE, putOpt = FALSE,
                          giveupdn = FALSE, up = 1.5, dn = 0.5) {
    ## set up the binomial tree paramaters
    h <- tt/nStep
    if (!giveupdn) {
        up <- exp((r-d)*h + sqrt(h)*v)
        dn <- exp((r-d)*h - sqrt(h)*v)
    }
    p <- (exp((r-d)*h) - dn)/(up - dn)
    if (printout) {cat("up = ",up,"; dn = ",dn,"; p = ",p,"\n")}
    nn <- 0:nStep
    payoffmult <- ifelse(putOpt,-1,1)

    A <- matrix(up^nn, nStep+1, nStep+1, byrow=TRUE)
    B <- matrix(dn^nn/up^nn, nStep+1, nStep+1) 
    SMat <- upper.tri(A, diag=TRUE)*(A*B)*s ## stock price matrix
    if (printout) {print("Stock Price Tree");print(SMat,digits=5)}
    Vc <- matrix(0, nStep+1, nStep+1)  ## Initialize opt price matrix
    Vc[, nStep+1] <- pmax(0, (SMat[, nStep+1] - k)*payoffmult)

    for (i in (nStep):1) {
        Vnc <- exp(-r*h)*(p*Vc[1:i,i+1] + (1-p)*Vc[2:(i+1),i+1])
        if (American) {
            Vc[1:i,i] <- pmax(Vnc, (SMat[1:i,i] - k)*payoffmult)
        } else {
            Vc[1:i,i] <- Vnc
        }
    }
    if (printout) {
        print(paste(ifelse(putOpt, "Put", "Call"),"Option Value:"))
        print(Vc,digits=4)
    }
    return(Vc[1,1])
}
