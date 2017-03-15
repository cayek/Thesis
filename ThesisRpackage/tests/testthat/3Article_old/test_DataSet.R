library(testthat)
library(Article3Package)
context("DataSet")

test_that("default constructor", {
  n <- 100
  d <- 2
  dat <- DataSet(G = NULL, X = MASS::mvrnorm(n = n,
                                             Sigma = 1 * diag(1,d,d),
                                             mu = rep(0,d)))
  plot(dat)
})

test_that("dataset plot", {

  K <- 3
  s <- NormalSampler(n = 200,
                     L = 1000,
                     K = K,
                     prop.outlier = 0.02,
                     sigma = 0.2,
                     c = 0.6,
                     mean.B = 0.0,
                     sd.mu = 1.0,
                     mean.mu = 0.5)
  dat <- sampl(s)



  # run method
  m <- PCAClassicLinearMethod(K = K)

  m <- fit(m, dat)

  p <- plot(dat,m)

})
