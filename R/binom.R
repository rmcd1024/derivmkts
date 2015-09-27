#' @title Binomial option pricing
#'
#' @description \code{binomopt} using the binomial pricing algorithm
#'     to compute prices of European and American calls and puts.
#'
#' @name binom
#'
#' @aliases binomopt binomplot
#'
#' @return \code{binomopt} returns a list where \code{$price} is the
#'     binomial option price and \code{$params} is a vector containing
#'     the inputs and binomial parameters used to compute the option
#'     price. Optionally, the list can contain the complete asset
#'     price and option price trees., \code{binomplot} produces a
#'     visual representation of the binomial tree.
#'
#' @usage
#' binomopt(s, k, v, r, tt, d, nstep = 10, american = TRUE,
#'     putopt=FALSE, specifyupdn=FALSE, crr=FALSE, jarrowrudd=FALSE,
#'     up=1.5, dn=1.5, returntrees=FALSE)
#' 
#' binomplot(s, k, v, r, tt, d, nstep, putopt=FALSE, american=TRUE,
#'     plotvalues=FALSE, plotarrows=FALSE, drawstrike=TRUE,
#'     probs=TRUE, pointsize=4, setylim=FALSE, ylimval=c(0,0),
#'     saveplot = FALSE, saveplotfn='binomialplot.pdf',
#'     printStock=FALSE, crr=FALSE, titles=TRUE, specifyupdn=FALSE,
#'     up=1.5, dn=1.5)
#'
#'
#' @param s Stock price
#' @param k Strike price of the option
#' @param v Volatility of the stock, defined as the annualized
#'     standard deviation of the continuously-compounded return
#' @param r Annual continuously-compounded risk-free interest rate
#' @param tt Time to maturity in years
#' @param d Dividend yield, annualized, continuously-compounded
#' @param nstep Number of binomial steps. Default is \code{nstep = 10}
#' @param american Boolean indicating if option is American
#' @param putopt Boolean \code{TRUE} is the option is a put
#' @param specifyupdn Boolean for manual entry of the binomial
#'     parameters up and down. This overrides the crr and jarrowrudd
#'     flags
#' @param crr \code{TRUE} to use the Cox-Ross-Rubinstein tree
#' @param jarrowrudd \code{TRUE} to use the Jarrow-Rudd tree
#' @param up If \code{specifyupdn=TRUE}, up move on the binomial tree
#' @param dn If \code{specifyupdn=TRUE}, down move on the binomial
#'     tree
#' @param returntrees If \code{returntrees=TRUE}, the list returned by
#'     the function includes four trees: for the price of the
#'     underlying asset (stree), the option price (oppricetree), where
#'     the option is exercised (exertree), and the probability of
#'     being at each node
#' 
#' @details Returns an option price, a vector of the parameters used
#'     to compute the price.  Optionally returns
#'     the following \eqn{(\textrm{nstep}+1)\times (\textrm{nstep}+
#'     1)}{(nstep+1)*(nstep+1)} matrices:
#' 
#' \describe{
#' 
#'   \item{stree}{the binomial tree for the price of the underlying
#'   asset}
#' 
#'   \item{oppricetree}{the binomial tree for the option price at each
#'   node}
#' 
#'   \item{exertree}{the tree of boolean indicators for whether or not
#'  the option is exercisd at each node}
#' 
#'   \item{probtree}{the probability of reaching each node}
#' 
#' }
#' 
#' \code{binomplot} plots the stock price lattice and shows
#' graphically the probability of being at each node (represented as
#' the area of the circle at that price) and whether or not the option
#' is optimally exercised there (green if yes, red if no), and
#' optionally, ht, depending on the inputs
#'
#' @note By default, \code{binomopt} computes the binomial tree using
#'     up and down moves of \deqn{u=\exp((r-d)*h + \sigma\sqrt{h})}{u
#'     = exp((r-d)*h + v*h^(0.5))} and \deqn{d=\exp((r-d)*h -
#'     \sigma\sqrt{h})}{d = exp((r-d)*h - v*h^(0.5))} You can use
#'     different trees: There is a boolean variable \code{CRR} to use
#'     the Cox-Ross-Rubinstein pricing tree, and you can also supply
#'     your own up and down moves with \code{specifyupdn=TRUE}. It's
#'     important to realize that if you do specify the up and down
#'     moves, you are overriding the volatility parameter.
#'
#' @examples
#' s=40; k=40; v=0.30; r=0.08; tt=0.25; d=0; nstep=15
#' binomopt(s, k, v, r, tt, d, nstep, american=TRUE, putopt=TRUE)
#' binomplot(s, k, v, r, tt, d, nstep, american=TRUE, putopt=TRUE)
#' binomplot(s, k, v, r, tt, d, nstep, american=FALSE, putopt=TRUE)
#' 
#' 


binomopt <- function(s, k, v, r, tt, d,
                     nstep=10, american = TRUE, putopt=FALSE,
                     specifyupdn=FALSE, crr=FALSE, jarrowrudd=FALSE,
                     up=1.5, dn=1.5, returntrees=FALSE) {
    ## set up the binomial tree parameters
    
    h <- tt/nstep
    if (!specifyupdn) {
        if (crr) {
            up <- exp(sqrt(h)*v)
            dn <- exp(-sqrt(h)*v)
        } else if (jarrowrudd) {
            up <- exp((r-d-0.5*v^2)*h + sqrt(h)*v)
            dn <- exp((r-d-0.5*v^2)*h - sqrt(h)*v)
        } else {
            up <- exp((r-d)*h + sqrt(h)*v)
            dn <- exp((r-d)*h - sqrt(h)*v)
        }
    }
    p <- (exp((r-d)*h) - dn)/(up - dn)
    nn <- 0:nstep
    payoffmult <- ifelse(putopt,-1,1)
    ##
    ## Construct stock price matrix
    ##
    stree <- matrix(0, nstep+1, nstep+1)
    stree[1, ] <- s*up^nn
    for (i in 2:(nstep+1))
        stree[i, i:(nstep+1)] <- stree[i-1, (i-1):nstep]*dn
    Vc <- matrix(0, nstep+1, nstep+1)  ## Initialize opt price matrix
    Vc[, nstep+1] <- pmax(0, (stree[, nstep+1] - k)*payoffmult)
    ##
    ## recurse
    ##
    for (i in (nstep):1) {
        Vnc <- exp(-r*h)*(p*Vc[1:i,i+1] + (1-p)*Vc[2:(i+1),i+1])
        if (american) {
            Vc[1:i,i] <- pmax(Vnc, (stree[1:i,i] - k)*payoffmult)
        } else {
            Vc[1:i,i] <- Vnc
        }
    }
    params=c(s=s, k=k, v=v, r=r, tt=tt, d=d,
             nstep=nstep, p=p, up=up, dn=dn, h=h)
    if (returntrees) {
        probtree <- matrix(0, nstep+1, nstep+1)
        A <- matrix(p^nn, nstep+1, nstep+1, byrow=TRUE)
        B <- matrix((1-p)^nn/p^nn, nstep+1, nstep+1)
        for (i in 0:nstep) { probtree[,i+1] <- choose(i,nn) }
        exertree <- (payoffmult*(stree - k) == Vc)
        probtree <- A*B*probtree
        return(list(price=Vc[1,1], params=params, oppricetree=Vc,
                    stree=stree, probtree=probtree, exertree=exertree)
               )
    } else {
        return(list(price=Vc[1,1], params=params))
    }
}

binomplot <- function(s, k, v, r, tt, d, nstep, putopt=FALSE,
                      american=TRUE, plotvalues=FALSE,
                      plotarrows=FALSE, drawstrike=TRUE, probs=TRUE,
                      pointsize=4, setylim=FALSE, ylimval=c(0,0),
                      saveplot = FALSE, saveplotfn='binomialplot.pdf',
                      printStock=FALSE, crr=FALSE, titles = TRUE,
                      specifyupdn=FALSE, up=1.5, dn=1.5) {
    ## see binomopt for more details on tree
    ## construction. "plotvalues" shows stock price values;
    ## "drawstrike" if true draws a line at the strike price; "probs"
    ## makes pointsizes proportional to probability of that point,
    ## times "pointsize"
    ##
    ## If no value given for ylimval and setylim=TRUE, there will be
    ## an error
    ## 
    
    if (setylim && ylimval==c(0,0))
        return(
            'Error: if setylim==TRUE, must enter values for ylimval')
    
    y <- binomopt(s, k, v, r, tt, d, nstep, american, putopt,
                  specifyupdn, crr, up=1.5, dn=1.5, returntrees=TRUE)
    for (i in c('h', 'up', 'dn', 'p')) assign(i, y$params[i])
    for (i in c('stree', 'exertree', 'oppricetree', 'probtree'))
        assign(i, y[['i']])
    nn <- 0:nstep
    payoffmult <- ifelse((putopt),-1,1)
    stree <- y$stree
    exertree <- y$exertree
    oppricetree <- y$oppricetree
    probtree <- y$probtree
    
    ## The rep command replicates each entry in nn a different number of
    ## times (1st entry once, second, entry twice, etc. Need to add 1
    ## because the first entry in nn is zero, which implies zero reps. The
    ## point of the stree restriction is not to plot zeros.
    plotcolor <- ifelse(exertree,"green3","red")
    if (saveplot) pdf(saveplotfn)
    plot(rep(nn, nn+1)*h, stree[stree>0]
        ,ylim=ifelse(c(setylim, setylim),ylimval,
                     c(min(stree[stree>1]-2),max(stree)*1.03))
        ,col=plotcolor[stree>0]
        ,pch=21
         ## ifelse returns an object with the size of the first
         ## argument. So in order for it to pass an array the first
         ## argument is an array filled with the boolean "probs". 
        ,cex=ifelse(stree[stree>0], sqrt(probtree[stree>0])*pointsize, 1)
        ,bg=plotcolor[stree>0] ## only matters for pch 21-25
        ,xlab=ifelse(titles, "Binomial Period", "")
        ,ylab=ifelse(titles,  "Stock Price", "")
        ,main=if (titles) paste(ifelse(american,"American","European"),
                                ifelse(putopt,"Put","Call"))
         )
    if (titles)
        mtext(paste0("Stock = ",format(s, digits=3),
                     ", Strike = ",format(k, digits=3),
                     ", Time = ",format(tt,digits=4),
                     ifelse(tt==1," year,"," years,")
                    ," Price = ",format(oppricetree[1,1],digits=5)))
    if (drawstrike) abline(h=k)
    yoffset <- 0.03*max(stree)
    if (plotarrows) {
        for (i in 1:nstep) {
            for (j in 1:i) {
                arrows((i-1)*h, stree[j,i],c(i,i)*h,
                       c(stree[j,i+1],stree[j+1,i+1]), length=0.06)
            }
        }
    }
    if (plotvalues) {
        for (i in 1:(nstep+1)) {
            text((i-1)*h,stree[1:i,i]+yoffset,format(stree[1:i,i], digits=4))
        }
    }
    if (saveplot) dev.off()
}
