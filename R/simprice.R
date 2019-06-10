#' @title Simulate asset prices
#'
#' @description \code{simprice} computes simulated lognormal price
#'     paths, with or without jumps. Saves and restores random number
#'     seed.
#'
#' \code{simprice(s0 = 100, v = 0.3, r = .08, tt = 1, d = 0, trials =
#' 2, periods = 3, jump = FALSE, lambda = 0, alphaj = 0, vj = 0, seed
#' = NULL, long = TRUE, scalar_v_is_stddev = TRUE)}
#'
#' @name simprice
#' @importFrom stats rnorm rpois
#' @return A dataframe with \code{trials} simulated stock price paths
#'
#' @usage simprice(s0, v, r, tt, d, trials, periods, jump, lambda,
#'     alphaj, vj, seed, long, scalar_v_is_stddev)
#'
#' @param s0 Initial price of the underlying asset
#' @param v If scalar, default is volatility of the asset price,
#'     defined as the annualized standard deviation of the
#'     continuously-compounded return. The parameter
#'     \code{scalar_v_is_stddev} controls this behavior. If \code{v}
#'     is a square \code{n x n} matrix, it is assumed to be the
#'     covariance matrix and \code{simprice} will return \code{n}
#'     simulated price series.
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
#' @param scalar_v_is_stddev if \code{TRUE}, scalar v is interpreted
#'     as the standard devaition; if \code{FALSE}, it is
#'     variance. Non-scalar V is always interpreted as a covariance
#'     matrix
#'
#' @examples
#' # simple Monte Carlo option price example. Since there are two
#' # periods we can compute options prices for \code{tt} and
#' # \code{tt/2}
#' s0=40; k=40; v=0.30; r=0.08; tt=0.25; d=0;
#' st = simprice(s0, k, v, r, tt, d,  trials=3, periods=2, jump=FALSE)
#' callprice1 = exp(-r*tt/2)*mean(pmax(st[st$period==1,] - k, 0))
#' callprice2 = exp(-r*tt)*mean(pmax(st[st$period==2,] - k, 0))
#'
#'

#' @export

simprice <- function(s0 = 100, v = .3, r = 0.08, tt = 1, d = 0,
                     trials = 2, periods = 3,
                     jump = FALSE, lambda = 0, alphaj = 0, vj = 0,
                     seed = NULL, long = TRUE,
                     scalar_v_is_stddev = TRUE) {
    testing <- FALSE
    ##testing <- TRUE
    if (testing) {
        set.seed(1)
        s0=40; k=40; v=0.30; r=0.08; tt=0.25; d=0; periods <- 2;
        trials <- 3; lambda=2; alphaj=-.2; vj=.4; jump=TRUE
        long <- TRUE;
        ## jump <- FALSE
    }
    if (exists(".Random.seed", .GlobalEnv)) {
        oldseed <- .Random.seed
        savedseed <- TRUE
    } else {
        savedseed <- FALSE
    }
    if (!is.null(seed)) set.seed(seed)
    ## a row of dimension m is a simulated stock price path
    ## In this case we need to generate all the random Z's together
    ## We won't have correlation for anything related to jumps
    h <- tt/periods

    ## make sure that vectorizable parameters (vparams) are length 1
    ## or length(v)
    ##r <- .08;d <- 0;alphaj <- c(.2, .4);vj <- .4;lambda <- c(2, 8)
    ##    v <- matrix(c(1, .4, .4, 1), nrow = 2)
    numassets <- ifelse(length(v) > 1, ncol(v), 1)
    if (numassets == 1 & scalar_v_is_stddev) v <- v^2
    vparams <- list(r = r, d = d, alphaj = alphaj, vj = vj, lambda = lambda)
    test <- lapply(vparams, function(x) length(x) %in% c(numassets, 1))
    stopifnot(length(vparams) == sum(test == TRUE))
    for (i in names(vparams)) {
        if (length(get(i)) != numassets) {
            assign(i, rep(vparams[[i]], numassets))
        }
    }
    k <- exp(alphaj) - 1
    zraw <- array(rnorm(periods*trials*numassets),
                  dim = c(trials, periods, numassets))
    zall <- array(NA, dim = c(trials, periods, numassets))
    for (i in 1:periods) {
            zall[, i, ] <- zraw[, i, ] %*% chol(v)
    }
    if (numassets == 1) {
        vi <- sqrt(v)
    } else {
        vi <- sqrt(diag(v))
    }
    ##if (numassets > 1) v <- sqrt(diag(v))
    sall <- list()

    for (i in 1:numassets) {
        log_s <- matrix(NA,  nrow = trials,  ncol = periods+1)
        log_s[, 1] <- log(s0)
        for (j in 1:periods) {
            ##print(j)
            log_s[, j+1] <- log_s[, j] +
                (r[i]-d[i]-jump*k[i]*lambda[i]-0.5*vi[i]^2)*h +
                zall[, j, i]*sqrt(h)
        }
        if (jump) {
            nj <- matrix(rpois(periods*trials, lambda = lambda[i]*h),
                         nrow = trials, ncol = periods)
            zj <- matrix(rnorm(periods*trials), nrow = trials, ncol = periods)
            jumpfactor <- (alphaj[i]-0.5*vj[i]^2)*nj + vj[i]*sqrt(nj)*zj
            jumpfactor <- apply(jumpfactor, 1, cumsum)
            if (periods != 1) jumpfactor <- t(jumpfactor)
            nj <- cbind(0, nj)
            log_s <- log_s + cbind(0, jumpfactor)
        } else {
            nj <- matrix(0, nrow = trials,  ncol = periods + 1)
        }
        if (long == FALSE) {s <- data.frame(exp(log_s))
            colnames(s) <- paste0('h', 0:periods)
            sall[[i]] <- cbind(asset = factor(i), s)
        } else {
            sall[[i]] <- data.frame(asset = i,
                                    trial = as.factor(1:trials),
                                    njump = nj, smat = exp(log_s))
        }
    }

    if (savedseed) .GlobalEnv$.Random.seed <- oldseed
    if (long == FALSE) {
        return(do.call(rbind, sall))
    } else {
        s <- do.call(rbind, sall)
        slong <- stats::reshape(s,
                                direction = 'long',
                                varying = list(
                                    grep('njump', names(s), value = TRUE),
                                    grep('smat', names(s), value = TRUE)),
                                v.names = c('numjumps', 'price'),
                                timevar = 'period',
                                idvar = c('asset','trial'),
                                new.row.names = NULL)
        slong$period <- slong$period - 1
        slong$trial <- as.factor(slong$trial)
        slong$asset <- as.factor(slong$asset)
        return(slong[order(slong$asset, slong$trial, slong$period), ])
    }
}
