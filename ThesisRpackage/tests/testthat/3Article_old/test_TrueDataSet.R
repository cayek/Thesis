library(testthat)
library(Article3Package)
context("TrueDataSet")

test_that("TrueDataSet Case3", {

  skip("run it manualy")

  n <- 100
  L <- 3000
  s <- TrueSampler(G.file = "../../Data2016_2017/SSMPG2015/Case3/Case3.lfmm",
                   X.file = "../../Data2016_2017/SSMPG2015/Case3/Case3.env",
                   outlier.file = "../../Data2016_2017/SSMPG2015/Case3/Case3.outlier",
                   n = n,
                   L = L)

  dat <- sampl(s)
  expect_equal(dim(dat$G), c(n, L))
  expect_equal(dim(dat$X), c(n, 1))
})


test_that("TrueDataSet Case2", {

  skip("run it manualy")

  s <- TrueSampler(G.file = "../../Data2016_2017/SSMPG2015/Case2/Case2.lfmm",
                   X.file = "../../Data2016_2017/SSMPG2015/Case2/Case2.env",
                   outlier.file = "../../Data2016_2017/SSMPG2015/Case2/Case2.outlier")

  dat <- sampl(s)
  expect_equal(dim(dat$G), c(517, 4542))
  expect_equal(dim(dat$X), c(517, 1))
  expect_equal(length(dat$outlier), 12)
})
