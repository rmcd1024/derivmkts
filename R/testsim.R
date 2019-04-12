library(derivmkts)
library(dplyr)
source('~/git/derivmkts/R/simprice.R')

## set.seed(1);
s0=40; k=40; v=0.30; r=0.08; tt=0.25; d=0; m <- 2;n <- 3
lambda=2; alphaj=-.2; vj=.4; n <- 100000

mj1 <- mertonjump(s0, k,  v = .3, 0.08,  c(.25),  d, lambda, alphaj, vj)[1]
mj11 <- mertonjump(s0, k,  v = .3, 0.08,  c(.25/3),  d, lambda, alphaj, vj)[1]
mj13 <- mertonjump(s0, k,  v = .3, 0.08,  c(.25),  d, lambda, alphaj, vj)[1]

cp <- bscall(s0, k,  v,  r,  tt = c(.25/3,  .25),  d)
st1 = simprice(40, v, r, tt, d, m = 1, n = n, jump = FALSE, lambda = lambda,
               alphaj = alphaj,  vj = vj)
st3 = simprice(40, v, r, tt, d, m = 3, n = n, jump = FALSE, lambda = lambda,
               alphaj = alphaj,  vj = vj)

mc <- exp(-r*tt )*mean(pmax(0, st1$price - k ))
print(paste(cp[2],  mc))
mc <- exp(-r*(tt/3) )*mean(pmax(0, st3$price[st3$period == 1] - k ))
print(paste(cp[1],  mc))
mc <- exp(-r*tt )*mean(pmax(0, st3$price[st3$period == 3] - k ))
print(paste(cp[2],  mc))


stj1 = simprice(40, v, r, tt, d, m = 1, n = n, jump = TRUE, lambda = 2,
                alphaj = -0.20, vj = .4)


stj3 = simprice(40, v, r, tt, d, m = 3, n = n, jump = TRUE, lambda = 2,
                alphaj = -0.20,  vj = .4)


mc <- exp(-r*tt )*mean(pmax(0, stj1$price - k ))
print(paste(mj13, mc))
mc <- exp(-r*(tt/3))*mean(pmax(0, stj3$price[stj3$period == 1] - k ))
print(paste(mj11, mc) )
mc <- exp(-r*tt )*mean(pmax(0, stj3$price[stj3$period == 3] - k ))
print(paste(mj13, mc) )

mc <- exp(-r*tt)*mean(pmax(0,  (st3$price[st3$period == 1] +
                                st3$price[st3$period == 2] +
                                st3$price[st3$period == 3])/3 - k ))
ac3 <- arithasianmc(s0,  k,  v,  r,  tt,  d, 3,  numsim = 20000)[1, 1]
print(paste(ac3, mc))
