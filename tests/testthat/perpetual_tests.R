kseq0 <- c(35, 40, 45)
kseq <- rep(c(35, 40, 45), times=3)
Hseq <- rep(c(35, 40, 45), each=3)
s <- 40; k <- 40; v <- 0.30; r <- 0.08;  tt <- 2; d <- 0.05; H=43.1
sseq <- c(10, 40, 250)
sseq4 <- rep(c(10, 40, 200), each=3)
kseq4 <- rep(c(35, 40, 45), 3)

for (i in barriertestfns4) {
    testfn <- tolower(i)
    test_that(paste(testfn, 'perpetual call and put work'), {
        correct <- barriervals4[, i]
        ##        print(correct)
        unknown <- do.call(testfn,
                           list(s=sseq4, k=kseq4, v=v, r=r, d=d)
                           )
        expect_equal(correct, unknown)
    }
    )
    print(paste(testfn, 'okay'))
}

test_that('Greeks for perpetual call work', {
    correct <-  greeksvals[["callperpetual"]]
    for (i in names(greeksinputsnott)) assign(i, greeksinputsnott[i])
    unknown <-  greeks(callperpetual(s, k, v, r, d), initcaps=TRUE)
    expect_equal(correct, unknown)
}
)

