library(testthat)
context("Hypothesis Testing Zscore")

test_that("LocfdrCalibratedZscore", {
  d <- 3
  L <- 1000
  score <- matrix(rnorm(L * d, 1, 3), d, L)

  func <- FdrtoolsCalibratedZscore()

  pval <- func$fun(score)

  # test if pvalue are well unfiform
  expect_true(ks.test(pval, rnorm(L * d))$p.value < 1e-10)
})
