library(testthat)
library(Article3Package)
context("tidyer")

test_that("tidyer", {

  # on dataset
  s <- NormalSampler(50, 500, 5)
  dat <- sampl(s)
  expect_equal(dim(getTidy_MethodOutput(dat)), c(3300, 4))

})
