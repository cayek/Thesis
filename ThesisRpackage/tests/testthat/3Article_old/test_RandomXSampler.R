library(testthat)
library(Article3Package)
context("RandomXSampler")

test_that("RandomXSampler", {
  s <- RandomXSampler(NormalSampler(10, 100, 3), d = 2)
  dat <- sampl(s)
  plot(dat)
})
