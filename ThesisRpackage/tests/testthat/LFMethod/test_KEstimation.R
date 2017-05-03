library(testthat)
context("CATE method")

################################################################################
# sva

test_that("K estimation with sva", {
  skip_if_not_installed("sva")

  dat <- NormalSampler2(100,
                        1000,
                        3) %>%
    sampl()

  m <- finalSVAMethod(3)

  expect_equal(numLatentVarEstimation(m, dat),3)

})

################################################################################
# FAMT

test_that("K estimation with famt", {
  skip_if_not_installed("FAMT")

  dat <- NormalSampler2(100,
                        1000,
                        3) %>%
    sampl()

  m <- finalFamtMethod(3)

  expect_equal(numLatentVarEstimation(m, dat),4) ## return 4 ? why ?

})

################################################################################
# cate

test_that("K estimation with cate", {

  skip_if_not_installed("cate")

  dat <- NormalSampler2(100,
                        500,
                        3) %>%
    sampl()

  m <- finalcateMethod(3)

  expect_equal(numLatentVarEstimation(m, dat),4) ## return 4 ? why ? long ! 

})

