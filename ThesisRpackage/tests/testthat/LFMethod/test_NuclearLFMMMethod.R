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
  m.nuclear <- NuclearLFMMMethod(K = NULL, gamma = 1.2e2, lambda = 0.8e0, lasso = TRUE, soft = TRUE,
                                 hypothesis.testing.method = lm_zscore(correctionByC = FALSE))

  expect_equal(name(m.nuclear), "NuclearLFMMMethod|lm+zscore|calibrate=FALSE")

  # test fit
  m.nuclear <- fit(m.nuclear, dat)
  # test run
  m.nuclear <- run(m.nuclear, dat)


  # plot and compare
  # m.ridge <- RidgeLFMMMethod(K = K)
  # m.ridge <- run(m.ridge, dat)
  # m.lm <- ClassicLinearMethod()
  # m.lm <- run(m.lm, dat)
  # m.pca <- PCAClassicLinearMethod(K = K)
  # m.pca <- run(m.pca, dat)
  # gplot_stat(m.nuclear$score[1,],
  #            m.ridge$score[1,],
  #            m.pca$score[1,],
  #            m.lm$score[1,],
  #            outlier = dat$outlier) +
  #   geom_point(aes(x = index, y = stat, color = outlier))

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
  # plotAndCompare(m.nuclear, dat, K)


})

