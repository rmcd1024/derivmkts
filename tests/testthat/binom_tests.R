

test_that('binom EC works', {
              correct <- binomvalsEurC
              unknown <- binomopt(s=s, k=k, v=v, r=r, tt=tt, d=d,
                             nstep, putopt=FALSE, american=FALSE)$price
              expect_equal(correct, unknown, tolerance=tol)
          }
          )
test_that('binom EP works', {
              correct <- binomvalsEurP
              unknown <- binomopt(s=s, k=k, v=v, r=r, tt=tt, d=d,
                             nstep, putopt=TRUE, american=FALSE)$price
              expect_equal(correct, unknown, tolerance=tol)
          }
          )
test_that('binom EP works', {
              correct <- binomvalsAmP
              unknown <- binomopt(s=s, k=k, v=v, r=r, tt=tt, d=d,
                             nstep, putopt=TRUE, american=TRUE)$price
              expect_equal(correct, unknown, tolerance=tol)
          }
          )
test_that('binom EP works', {
              correct <- binomvalsAmC
              unknown <- binomopt(s=s, k=k, v=v, r=r, tt=tt, d=d,
                             nstep, putopt=FALSE, american=TRUE)$price
              expect_equal(correct, unknown, tolerance=tol)
          }
          )


print('binom okay')

