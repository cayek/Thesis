library(testthat)
library(Article3Package)
context("GenerativeDataSet")

test_that("generative constructor", {
  dat <- GenerativeDataSet(n = 1, L = 20)
  plot(dat)
})
