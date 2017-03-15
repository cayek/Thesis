library(testthat)
library(Article3Package)
context("Normal sampler 2")



################################################################################
# NuclearLFMMMethod on generative model

test_that("Test of the normal sampler 2", {

  cs <- c(0.6, 0.3, 0.0)
  K <- 3
  s <- NormalSampler2(n = 3000, # large to be sure of the correlation diff.
                      L = 1000,
                      K = 3,
                      cs = cs)

  dat <- sampl(s)

  expect_lte(abs(cor(dat$X, dat$U[,1]) - cs[1]), 1e-1)
  expect_lte(abs(cor(dat$X, dat$U[,2]) - cs[2]), 1e-1)
  expect_lte(abs(cor(dat$X, dat$U[,3]) - cs[3]), 1e-1)


  cs <- c(0.6, 0.3, 0.0)
  K <- 3
  s <- NormalSampler2(n = 100, # large to be sure of the correlation diff.
                      L = 1000,
                      K = 3,
                      prop.outlier = 0.2,
                      cs = cs)

  samplerTest(s, K = K + 4, lfmm.lambda = 1e-5)

})
