#' @title Asian option pricing
#'
#' @description Pricing functions for European Asian
#'     options. \code{geomAsian} (and \code{geomAsianCall} and
#'     \code{geomAsianPut} compute prices of geometric Asian options
#'     using the Black-Scholes formula. \code{arithAsian}  The functions
#'     \code{assetcall}, \code{assetput}, \code{cashcall}, and
#'     \code{cashput} provide the prices of binary options that pay a
#'     share (the asset options) or $1 (the cash options) if at
#'     expiration the asset price exceeds the strike (the calls) or is
#'     below the strike (the puts). We have the identities
#'
#' \code{bscall(s, k, v, r, tt, d)
#'   = assetcall(s, k, v, r, tt, d) - k*cashcall(s, k, v, r, tt, d)}
#'
#'
#' @name asian
#' @aliases geomAsianCall, geomAsianPut, geomAsian, arithAsian,
#'     arithAsianCV
#'
#' @return An option price. If more than one argument is a
#' vector, the recycling rule determines the handling of the inputs
#'
#' @usage
#' geomAsianCall(s, k, v, r, tt, d)
#' geomAsianPut(s, k, v, r, tt, d)
#' geomAsian(s, k, v, r, tt, d)
#' arithAsian(s, k, v, r, tt, d)
#' arithAsianCV(s, k, v, r, tt, d)
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
#' bscall(s, k, v, r, tt, d)
#'
#' ## following returns the same price as previous
#' assetcall(s, k, v, r, tt, d) - k*cashcall(s, k, v, r, tt, d)
#'
#' ## return option prices for different strikes
#' bsput(s, k=38:42, v, r, tt, d)

geomAsianCall <- function(s, k, v, r, tt, d, numavg) {
    siga <- v*((numavg+1)*((2*numavg)+1)/6)^0.5/numavg
    da <- 0.5*(r*(numavg-1)/numavg+(d+0.5*v^2)*(numavg+1)/numavg-
                   (v/numavg)^2*(numavg+1)*(2*numavg+1)/6)
    return(bscall(s, k, siga, r, tt, da))
}

geomAsianPut <- function(s, k, v, r, tt, d, numavg) {
    siga <- v*((numavg+1)*((2*numavg)+1)/6)^0.5/numavg
    da <- 0.5*(r*(numavg-1)/numavg+(d+0.5*v^2)*(numavg+1)/numavg-
                   (v/numavg)^2*(numavg+1)*(2*numavg+1)/6)
    return(bsput(s, k, siga, r, tt, da))
}

geomAsian <- function(s, k, v, r, tt, d, numavg) {
    siga <- v*((numavg+1)*((2*numavg)+1)/6)^0.5/numavg
    da <- 0.5*(r*(numavg-1)/numavg+(d+0.5*v^2)*(numavg+1)/numavg-
                   (v/numavg)^2*(numavg+1)*(2*numavg+1)/6)
    return(c(Call <- bscall(s, k, siga, r, tt, da),
             Put <- bsput(s, k, siga, r, tt, da)))
}

arithAsian <- function(s, k, v, r, tt, d, numavg, numsim=1000,
                       printSDs=FALSE) {
    ## average price Asian call and put
    ## 
    ## numavg is the number of averages in the function. 
    ## Create a matrix of stock prices, where the numavg stock prices in
    ## each simulation are a row of the matrix. After we compute the
    ## sequence of stock prices, we sum across the rows, then we compute
    ## option payoffs and average to get the price.
    z <- matrix(rnorm(numavg*numsim), numsim, numavg)
    zcum <- t(apply(z, 1, cumsum))
    h <- tt/numavg
    hmat <- matrix((1:numavg)*h,numsim,numavg,byrow=TRUE)
    S <- matrix(0, nrow=numsim, ncol=numavg)
    ## Computing the matrix of stock prices in one step is marginally
    ## faster when performing the experiment using microbenchmark.
    onestep <- TRUE
    if (onestep) {
        S <- s*exp((r-d-0.5*v^2)*hmat +
                   v*sqrt(h)*zcum)
    } else {
        for (i in 1:numavg) {
            S[, i] <- s*exp((r-d-0.5*v^2)*h*i +
                       v*sqrt(h)*zcum[, i])
        }
    }
    ST <- S[,numavg]
    Savg <- apply(S, 1, sum)/numavg
    tmp <- pmax(Savg-k, 0)
    avgpricecall <- mean(tmp)*exp(-r*tt)
    avgpricecallsd <-sd(tmp)*exp(-r*tt)
    tmp <- pmax(k-Savg, 0)
    avgpriceput <- mean(tmp)*exp(-r*tt)
    avgpriceputsd <- sd(tmp)*exp(-r*tt)
    tmp <- pmax(ST-Savg, 0)
    avgstrikecall <- mean(tmp)*exp(-r*tt)
    avgstrikecallsd <- sd(tmp)*exp(-r*tt)
    tmp <- pmax(Savg-ST, 0)
    avgstrikeput <- mean(tmp)*exp(-r*tt)
    avgstrikeputsd <- sd(tmp)*exp(-r*tt)
    tmp <- pmax(ST-k,0)
    bscall <- mean(tmp)*exp(-r*tt)
    bscallsd <- sd(tmp)*exp(-r*tt)
    tmp <- pmax(k-ST,0)
    bsput <- mean(tmp)*exp(-r*tt)
    bsputsd <- sd(tmp)*exp(-r*tt)
    if (printSDs) {
        out <- matrix(c(avgpricecall,avgstrikecall,bscall,
                        avgpriceput,avgstrikeput,bsput,
                        avgpricecallsd,avgstrikecallsd,bscallsd,
                        avgpriceputsd,avgstrikeputsd,bsputsd),
                      3,4)
        colnames(out) <- c("Call", "Put", "sd Call", "sd Put")
        rownames(out) <- c("Avg Price", "Avg Strike","Vanilla")
    } else {
        out <- matrix(c(avgpricecall,avgstrikecall,bscall,
                        avgpriceput,avgstrikeput,bsput),
                      3,2)
        colnames(out) <- c("Call", "Put")
        rownames(out) <- c("Avg Price", "Avg Strike","Vanilla")
    }
    return(out)
}


arithAsianCallCV <- function(s, k, v, r, tt, d, numavg, numsim=1000) {
    ## control variate version
    numsim <- numsim + 250
    truegeom <- geomAsianCall(s, k, v, r, tt, d, numavg)
    z <- matrix(rnorm(numavg*numsim), numsim, numavg)
    h <- tt/numavg
    hmat <- matrix((1:numavg)*h,numsim,numavg,byrow=TRUE)
    S1 <- s*exp((r-d-0.5*v^2)*hmat +
                    v*sqrt(h)*t(apply(z, 1, cumsum)))
    Savg <- apply(S1, 1, sum)/numavg
    Sgeomavg <- apply(S1, 1, prod)^(1/numavg)
    avgpricecall <- pmax(Savg-k, 0)*exp(-r*tt)
    geomprice <- pmax(Sgeomavg-k, 0)*exp(-r*tt)
    betahat <- cov(avgpricecall[1:250], geomprice[1:250])/
        var(geomprice[1:250])
    corrected <- avgpricecall +
        betahat*(rep(truegeom, length(geomprice)) - geomprice)
    return(c(price=mean(corrected), beta=betahat))
}
