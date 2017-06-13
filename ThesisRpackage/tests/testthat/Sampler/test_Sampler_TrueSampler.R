library(testthat)
context("TrueDataSet")

test_that("TrueDataSet ind.clumping", {

  s <- TrueSampler(G.file = matrix(1,3,3),
                   X.file = matrix(2,3,1),
                   outlier.file = NULL,
                   ind.clumping = c(2,3),
                   n = NULL,
                   L = NULL)

  dat <- sampl(s)
})

test_that("TrueDataSet reference", {

  s <- TrueSampler(G.file = matrix(1,3,3),
                   X.file = matrix(2,3,1),
                   outlier.file = NULL,
                   n = NULL,
                   L = NULL,
                   reference = TRUE)

  dat <- sampl(s)
  expect_equal(class(dat)[1], "TrueDataSet")
  expect_equal(typeof(dat), "S4") ## this is a RC object ! 
})


test_that("TrueDataSet Case3", {

  skip("Work only au labo")

  n <- 100
  L <- 3000
  s <- TrueSampler(G.file = "./Data/SSMPG2015/Case3/Case3.lfmm",
                   X.file = "../Data/SSMPG2015/Case3/Case3.env",
                   outlier.file = "./Data/SSMPG2015/Case3/Case3.outlier",
                   n = n,
                   L = L)

  dat <- sampl(s)
  expect_equal(dim(dat$G), c(n, L))
  expect_equal(dim(dat$X), c(n, 1))
})


test_that("TrueDataSet Case2", {

  skip("Work only au labo")

  s <- TrueSampler(G.file = "../../Data2016_2017/SSMPG2015/Case2/Case2.lfmm",
                   X.file = "../../Data2016_2017/SSMPG2015/Case2/Case2.env",
                   outlier.file = "../../Data2016_2017/SSMPG2015/Case2/Case2.outlier")

  dat <- sampl(s)
  expect_equal(dim(dat$G), c(517, 4542))
  expect_equal(dim(dat$X), c(517, 1))
  expect_equal(length(dat$outlier), 12)
})
