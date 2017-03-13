library(testthat)
library(Article3Package)
context("AlternatedLFMMMethod")



################################################################################
# NuclearLFMMMethod on generative model

test_that("AlternatedLFMMMethod on normal generative model", {

  K = 4
  # sample data
  s <- NormalSampler(n = 100,
                     L = 1000,
                     K = K,
                     prop.outlier = 0.2,
                     sigma = 0.3,
                     c = 0.8,
                     mean.B = 0.0,
                     sd.mu = 1.0,
                     mean.mu = 0.5)
  dat <- sampl(s)

  # method
  m.alt <- AlternatedLFMMMethod(K = K + 20,
                                alpha = 10,
                                beta = 10)
  # to debug m = m.alt

  expect_equal(name(m.alt), "AlternatedLFMMMethod|Zscore|NormalZscore|AnalyticSigma2")

  # test fit
  m.alt <- fit(m.alt, dat)
  # test run
  m.alt <- run(m.alt, dat)


  # plot and compare
  plotAndCompare(m.alt, dat, K)

})


test_that("NuclearLFMMMethod on binary generative model", {

  K = 4
  # sample data
  s <- LogisticSampler(n = 100,
                       L = 1000,
                       K = K,
                       prop.outlier = 0.02,
                       sigma = 0.2,
                       c = 0.6,
                       mean.B = 0.0,
                       sd.mu = 1.0,
                       mean.mu = 0.5)
  dat <- sampl(s)

  m.alt <- AlternatedLFMMMethod(K = K + 20,
                                alpha = 10,
                                beta = 10)
  # to debug m = m.alt

  # run
  m.alt <- run(m.alt, dat)



  # plot and compare
  plotAndCompare(m.alt, dat, K)


})
