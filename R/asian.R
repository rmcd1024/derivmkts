#' @title Asian option pricing
#'
#' @description Pricing functions for European Asian
#'     options. \code{geomavgprice} and \code{geomavgstrike} compute
#'     analytical prices of geometric Asian options using the modified
#'     Black-Scholes formula. \code{arithasianmc} and
#'     \code{geomasianmc} compute Monte Carlo prices for the full
#'     range of average price and average strike call and puts
#'     computes prices of a complete assortment of Arithmetic Asian
#'     options (average price call and put and average strike call and
#'     put)
#'
#' @name Asian 
#' @family Asian options
#' @return An option price. If more than one argument is a vector, the
#'     recycling rule determines the handling of the inputs
#' @export 
#' @param s Stock price
#' @param k Strike price of the option. In the case of average strike
#'     options, \code{k/s} is the multiplier for the average
#' @param km The strike mutiplier, relative to the initial stock
#'     price, for an average price payoff. If the initial stock price
#'     is \code{s = 120} and \code{km = 115}, the payoff for an
#'     average strike call is \deqn{Payoff = max(ST - km/s*SAvg, 0)}.
#' @param v Volatility of the stock, defined as the annualized
#'     standard deviation of the continuously-compounded return
#' @param r Annual continuously-compounded risk-free interest rate
#' @param tt Time to maturity in years
#' @param d Dividend yield, annualized, continuously-compounded
#' @param m Number of prices in the average calculation
#' @param cont Boolean which when TRUE denotes continuous averaging
#' @param numsim Number of Monte Carlo iterations
#' @param printsds Print standard deviation for the particular Monte
#'     Carlo calculation
#' 
#' @details Returns a scalar or vector of option prices, depending on the inputs
#'

#' @export
#' @title Geometric average price
#' @family Asian options
#' @inheritParams Asian
#' @name geomavgprice
#' @examples
#' s=40; k=40; v=0.30; r=0.08; tt=0.25; d=0; m=3;
#' geomavgprice(s, k, v, r, tt, d, m)
#' 
#' @description Prices of geometric average-price call and put options
#' @usage
#' geomavgprice(s, k, v, r, tt, d, m, cont=FALSE)
#' @title Geometric average-price options
#' @return Vector of call and put prices for geometric average price
#'     options
geomavgprice <- function(s, k, v, r, tt, d, m, cont=FALSE) {
    if (cont) {
        siga <- v / (3^0.5)
        da <- 0.5 * (r + d + v^2 / 6)
    } else {
        siga <- v * ((m + 1) * ((2 * m) + 1) / 6)^0.5 / m 
        da <- 0.5 * (r * (m - 1) / m + (d + 0.5 * v^2) *
                     (m + 1) / m - (v / m)^2 *
                    (m + 1) * (2 * m + 1) / 6)
    }
    return(c(Call=bscall(s, k, siga, r, tt, da),
             Put=bsput(s, k, siga, r, tt, da)))
}

#' @export
#' @family Asian options
#' @inheritParams Asian
#' @name geomavgstrike
#' @description Prices of geometric average-strike call and put options
#' @usage
#' geomavgstrike(s, km, v, r, tt, d, m, cont=FALSE)
#' @examples
#' s=40; km=40; v=0.30; r=0.08; tt=0.25; d=0; m=3;
#' geomavgstrike(s, km, v, r, tt, d, m)
#' @title Geometric average-strike options
#' @return Vector of call and put prices for geometric average strike
#'     options
geomavgstrike <- function(s, km, v, r, tt, d, m, cont=FALSE) {
    if (cont) {
        siga <- v / (3 ^ 0.5)
        da <- 0.5 * (r + d + v ^ 2 / 6)
        rho <- 0.5 * (3 ^ 0.5)
    } else {
        siga <- v * ((m + 1) * (2 * m + 1) / 6) ^ 0.5 / m
        da <- 0.5 * (r * (m - 1) / m + (d + 0.5 * v ^ 2) * (m + 1) / m -
                      (v / m) ^ 2 * (m + 1) * (2 * m + 1) / 6)
        rho <- 0.5 * (6 * (m + 1) / (2 * m + 1)) ^ 0.5
    }
    vol <-  (siga ^ 2 + v ^ 2 - 2 * rho * siga * v) ^ 0.5
    return(c(Call=bscall(s, km, vol, da, tt, d),
           Put=bsput(s, km, vol, da, tt, d)))
}

#' @export
#' @title Arithmetic average options computed using Monte Carlo
#' @name arithasianmc
#' @family Asian options
#' @inheritParams Asian
#' @description Arithmetic average Asian option prices
#' @usage
#' arithasianmc(s, k, v, r, tt, d, m, numsim=1000, printsds=FALSE)
#' @examples
#' s=40; k=40; v=0.30; r=0.08; tt=0.25; d=0; m=3; numsim=1e04
#' arithasianmc(s, k, v, r, tt, d, m, numsim, printsds=FALSE)
#' @return Array of arithmetic average option prices, along with
#'     vanilla European option prices implied by the the
#'     simulation. Optionally returns Monte Carlo standard deviations.
arithasianmc <- function(s, k, v, r, tt, d, m, numsim=1000,
                       printsds=FALSE) {
    ## average price Asian call and put
    ##
    ## m is the number of averages in the function.
    ## Create a matrix of stock prices, where the m stock prices in
    ## each simulation are a row of the matrix. After we compute the
    ## sequence of stock prices, we sum across the rows, then we compute
    ## option payoffs and average to get the price.
    z <- matrix(rnorm(m*numsim), numsim, m)
    zcum <- t(apply(z, 1, cumsum))
    h <- tt/m
    hmat <- matrix((1:m)*h,numsim,m,byrow=TRUE)
    S <- matrix(0, nrow=numsim, ncol=m)
    ## Computing the matrix of stock prices in one step is marginally
    ## faster (tested using microbenchmark)
    onestep <- TRUE
    if (onestep) {
        S <- s*exp((r-d-0.5*v^2)*hmat +
                   v*sqrt(h)*zcum)
    } else {
        for (i in 1:m) {
            S[, i] <- s*exp((r-d-0.5*v^2)*h*i +
                       v*sqrt(h)*zcum[, i])
        }
    }
    ST <- S[,m]
    Savg <- apply(S, 1, sum)/m
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
    if (printsds) {
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

#' @export
#' @title Control variate asian call price
#' @family Asian options
#' @description Calculation of arithmetic-average Asian call price
#'     using control variate Monte Carlo valuation
#' @inheritParams Asian
#' @name arithavgpricecv
#' @usage
#' arithavgpricecv(s, k, v, r, tt, d, m, numsim)
#' @examples
#' s=40; k=40; v=0.30; r=0.08; tt=0.25; d=0; m=3; numsim=1e04
#' arithavgpricecv(s, k, v, r, tt, d, m, numsim)
#' @return Vector of the price of an arithmetic-average Asian call,
#'     computed using a control variate Monte Carlo calculation, along
#'     with the regression beta used for adjusting the price.
arithavgpricecv <- function(s, k, v, r, tt, d, m, numsim=1000) {
    ## control variate version
    numsim <- numsim + 250
    truegeom <- geomavgprice(s, k, v, r, tt, d, m)["Call"]
    z <- matrix(rnorm(m*numsim), numsim, m)
    h <- tt/m
    hmat <- matrix((1:m)*h,numsim,m,byrow=TRUE)
    S1 <- s*exp((r-d-0.5*v^2)*hmat +
                    v*sqrt(h)*t(apply(z, 1, cumsum)))
    Savg <- apply(S1, 1, sum)/m
    Sgeomavg <- apply(S1, 1, prod)^(1/m)
    avgpricecall <- pmax(Savg-k, 0)*exp(-r*tt)
    geomprice <- pmax(Sgeomavg-k, 0)*exp(-r*tt)
    betahat <- cov(avgpricecall[1:250], geomprice[1:250])/
        var(geomprice[1:250])
    corrected <- avgpricecall +
        betahat*(rep(truegeom, length(geomprice)) - geomprice)
    return(c(price=mean(corrected), beta=betahat))
}

#' @export
#' @name geomasianmc
#' @family Asian options
#' @inheritParams Asian
#' @description Geometric average Asian option prices
#' @title Geometric Asian option prices computed by Monte Carlo
#' @examples
#' s=40; k=40; v=0.30; r=0.08; tt=0.25; d=0; m=3; numsim=1e04
#' geomasianmc(s, k, v, r, tt, d, m, numsim, printsds=FALSE)
#' @usage 
#' geomasianmc(s, k, v, r, tt, d, m, numsim, printsds=FALSE)
#' @return Array of geometric average option prices, along with
#'     vanilla European option prices implied by the the
#'     simulation. Optionally returns Monte Carlo standard
#'     deviations. Note that exact solutions for these prices exist,
#'     the purpose is to see how the Monte Carlo prices behave.
geomasianmc <- function(s, k, v, r, tt, d, m, numsim=1000,
                       printsds=FALSE) {
    ## average price Asian call and put
    ##
    ## m is the number of averages in the function.
    ## Create a matrix of stock prices, where the m stock prices in
    ## each simulation are a row of the matrix. After we compute the
    ## sequence of stock prices, we sum across the rows, then we compute
    ## option payoffs and average to get the price.
    z <- matrix(rnorm(m*numsim), numsim, m)
    zcum <- t(apply(z, 1, cumsum))
    h <- tt/m
    hmat <- matrix((1:m)*h,numsim,m,byrow=TRUE)
    S <- matrix(0, nrow=numsim, ncol=m)
    ## Computing the matrix of stock prices in one step is marginally
    ## faster (tested using microbenchmark)
    onestep <- TRUE
    if (onestep) {
        S <- s*exp((r-d-0.5*v^2)*hmat +
                   v*sqrt(h)*zcum)
    } else {
        for (i in 1:m) {
            S[, i] <- s*exp((r-d-0.5*v^2)*h*i +
                       v*sqrt(h)*zcum[, i])
        }
    }
    ST <- S[,m]
    ## compute geometric average
    Savg <- apply(S, 1, prod)^(1/m)
    tmp <- pmax(Savg-k, 0)
    avgpricecall <- mean(tmp)*exp(-r*tt)
    avgpricecallsd <-sd(tmp)*exp(-r*tt)
    tmp <- pmax(k-Savg, 0)
    avgpriceput <- mean(tmp)*exp(-r*tt)
    avgpriceputsd <- sd(tmp)*exp(-r*tt)
    tmp <- pmax(ST-k/s*Savg, 0)
    avgstrikecall <- mean(tmp)*exp(-r*tt)
    avgstrikecallsd <- sd(tmp)*exp(-r*tt)
    tmp <- pmax(k/s*Savg-ST, 0)
    avgstrikeput <- mean(tmp)*exp(-r*tt)
    avgstrikeputsd <- sd(tmp)*exp(-r*tt)
    tmp <- pmax(ST-k,0)
    bscall <- mean(tmp)*exp(-r*tt)
    bscallsd <- sd(tmp)*exp(-r*tt)
    tmp <- pmax(k-ST,0)
    bsput <- mean(tmp)*exp(-r*tt)
    bsputsd <- sd(tmp)*exp(-r*tt)
    avgpriceexact <- geomavgprice(s, k, v, r, tt, d, m)
    avgstrikeexact <- geomavgstrike(s, k, v, r, tt, d, m)
    
    if (printsds) {
        out <- matrix(c(avgpricecall,avgstrikecall,bscall,
                        avgpriceexact["Call"], avgstrikeexact["Call"],
                        bscall(s, k, v, r, tt, d),
                        avgpriceput,avgstrikeput,bsput,
                        avgpriceexact["Put"], avgstrikeexact["Put"],
                        bsput(s, k, v, r, tt, d),
                        avgpricecallsd,avgstrikecallsd,bscallsd,
                        avgpriceputsd,avgstrikeputsd,bsputsd),
                      nrow=3,ncol=6)
        colnames(out) <- c("CallMC", "CallExact", "PutMC", "PutExact",
                           "sd Call", "sd Put")
        rownames(out) <- c("Avg Price", "Avg Strike","Vanilla")
    } else {
        out <- matrix(c(avgpricecall,avgstrikecall,bscall,
                        avgpriceexact["Call"], avgstrikeexact["Call"],
                        bscall(s, k, v, r, tt, d),
                        avgpriceput,avgstrikeput,bsput,
                        avgpriceexact["Put"], avgstrikeexact["Put"],
                        bsput(s, k, v, r, tt, d)
                        ),
                      3,4)
        colnames(out) <- c("CallMC", "CallExact", "PutMC", "PutExact")
        rownames(out) <- c("Avg Price", "Avg Strike","Vanilla")
    }
    return(out)
}

