library(testthat)
library(Article3Package)
context("NuclearLFMMMethod")


################################################################################
# NuclearLFMMMethod on generative model

test_that("NuclearLFMMMethod on normal generative model", {

  K = 4
  # sample data
  s <- NormalSampler(n = 100,
                     L = 1000,
                     K = K,
                     prop.outlier = 0.2,
                     sigma = 0.2,
                     c = 0.6,
                     mean.B = 0.0,
                     sd.mu = 1.0,
                     mean.mu = 0.5)
  dat <- sampl(s)

  # method
  m.nuclear <- NuclearLFMMMethod(K = NULL, gamma = 100)

  expect_equal(name(m.nuclear), "NuclearLFMMMethod|Zscore|NormalZscore|AnalyticSigma2")

  # test fit
  m.nuclear <- fit(m.nuclear, dat)
  # test run
  m.nuclear <- run(m.nuclear, dat)


  # plot and compare
  plotAndCompare(m.nuclear, dat, K)

})


test_that("NuclearLFMMMethod on binary generative model", {

  K = 4
  # sample data
  s <- LogisticSampler(n = 100,
                       L = 1000,
                       K = K,
                       prop.outlier = 0.2,
                       sigma = 0.2,
                       c = 0.6,
                       mean.B = 0.0,
                       sd.mu = 1.0,
                       mean.mu = 0.5)
  dat <- sampl(s)

  # method
  m.nuclear <- NuclearLFMMMethod(K = 30)
  # test run
  m.nuclear <- run(m.nuclear, dat)



  # plot and compare
  plotAndCompare(m.nuclear, dat, K)


})
