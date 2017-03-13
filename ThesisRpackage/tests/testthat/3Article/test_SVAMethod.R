library(testthat)
library(Article3Package)
context("SVA method")

################################################################################
# LEA

test_that("SVA method", {

  skip_if_not_installed("sva")

  K = 2
  # sample data
  s <- NormalSampler(n = 100,
                     L = 1000,
                     K = K,
                     prop.outlier = 0.02,
                     sigma = 0.2,
                     c = 0.6,
                     mean.B = 0.0,
                     sd.mu = 1.0,
                     mean.mu = 0.5)
  dat <- sampl(s)

  # method
  m <- SVAMethod(K = K, nickname = "SVAMethod")

  expect_equal(name(m), "SVAMethod")

  # test fit
  m <- fit(m, dat)
  # test run
  m <- run(m, dat)

  # plot
  gplot_stat(m$score[1,],
             outlier = dat$outlier) +
    geom_histogram(aes(stat, fill = outlier, y = ..density..))
  # RMK : the score is a F score

  gplot_stat(m$pvalue[1,],
             outlier = dat$outlier) +
    geom_histogram(aes(stat, fill = outlier, y = ..density..))

  gplot_stat(m$pvalue[1,],
             outlier = dat$outlier) +
    geom_point(aes(x = index, y = -log10(stat), color = outlier))
  # C'est mal calibré non ?

})
