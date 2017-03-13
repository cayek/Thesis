library(testthat)
library(Article3Package)
context("test of gplot_confunding")

test_that("with lm", {

  K = 6
  L = 1000
  n = 100
  # sample data
  s <- NormalSampler(n = n,
                       L = L,
                       K = K,
                       prop.outlier = 0.02,
                       sigma = 0.2,
                       c = 0.6,
                       mean.B = 0.0,
                       sd.mu = 1.0,
                       mean.mu = 0.5)
  dat <- sampl(s)

  m <- ClassicLinearMethod()
  m <- run(m, dat)

  gplot_confunding(dat, m, i = 1, j = 2, d = 1)
})

test_that("with ridge lfmm", {

  K = 2
  L = 1000
  n = 100
  # sample data
  s <- NormalSampler(n = n,
                     L = L,
                     K = K,
                     prop.outlier = 0.02,
                     sigma = 0.2,
                     c = 0.6,
                     mean.B = 0.0,
                     sd.mu = 1.0,
                     mean.mu = 0.5)
  dat <- sampl(s)

  m <- RidgeLFMMMethod(K = K,
                       lambda = 1e1)
  m <- run(m, dat)

  gplot_confunding(dat, m, i = 1, j = 2, d = 1)
})



test_that("with alternated ridge lfmm", {

  K = 2
  L = 1000
  n = 100
  # sample data
  s <- NormalSampler(n = n,
                     L = L,
                     K = K,
                     prop.outlier = 0.02,
                     sigma = 0.2,
                     c = 0.6,
                     mean.B = 0.0,
                     sd.mu = 1.0,
                     mean.mu = 0.5)
  dat <- sampl(s)

  m <- AlternatedSvdLFMMMethod(K = K,
                               lambda = 1e1,
                               err.max = 1e-10)
  m <- run(m, dat)

  gplot_confunding(dat, m, i = 1, j = 2, d = 1)
})
