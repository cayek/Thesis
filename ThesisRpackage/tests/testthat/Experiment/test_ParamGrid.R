library(testthat)
library(Article3Package)
context("param grid")

################################################################################
# classic Linear method

test_that("paramGrid", {

  method.constructor <- finalLfmmRdigeMethod
  listOfMethod <- paramGrid(method.constructor = method.constructor,
                            nickname.base = "RidgeLfmm",
                            lambda = c(1,10,100), K = c(1, 2, 3))

  expect_equal(length(listOfMethod), 9)
  expect_equal(class(listOfMethod), "list")


  expect_equal(listOfMethod[[1]]$K, 1)
  expect_equal(listOfMethod[[1]]$lambda, 1)
  expect_equal(listOfMethod[[1]]$nickname, "RidgeLfmm|lambda=1|K=1")

  expect_equal(listOfMethod[[9]]$K, 3)
  expect_equal(listOfMethod[[9]]$lambda, 100)
  expect_equal(listOfMethod[[9]]$nickname, "RidgeLfmm|lambda=100|K=3")

})
