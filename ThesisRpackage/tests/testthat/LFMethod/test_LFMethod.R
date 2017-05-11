library(testthat)
context("LFMethod")


test_that("qqplot", {

  s <- NormalSampler2(10, 100, 3)
  dat <- sampl(s)
  m <- finalPcaLm(3)
  m <- run(m, dat)

  qqplott(m, dat$outlier)

})

test_that("calibrate", {

  s <- NormalSampler2(100, 1000, 3)
  dat <- sampl(s)
  dat$X <- cbind(dat$X, rnorm(10))
  m <- finalPcaLm(3)
  m <- run(m, dat)

  m <- calibrate(m)
  qqplott(m, dat$outlier)

})
