library(testthat)
context("LFMethod")


test_that("qqplot", {

  s <- NormalSampler2(10, 100, 3)
  dat <- sampl(s)
  m <- finalPcaLm(3)
  m <- run(m, dat)

  qqplott(m, dat$outlier)

})
