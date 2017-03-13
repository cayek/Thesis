library(testthat)
library(Article3Package)
context("Lea lfmm method")

################################################################################
# LEA

test_that("lea lfmm method", {

  skip_if_not_installed("LEA")

  K = 2
  # sample data
  s <- RandomXSampler(NormalSampler(n = 10,
                                    L = 100,
                                    K = K,
                                    prop.outlier = 0.02,
                                    sigma = 0.2,
                                    c = 0.6,
                                    mean.B = 0.0,
                                    sd.mu = 1.0,
                                    mean.mu = 0.5), d = 1)
  dat <- sampl(s)

  # method
  m <- LeaLFMMMethod(K = K, verbose = FALSE)

  expect_equal(name(m), "LeaLFMMMethod|NULL")

  # test fit
  m <- fit(m, dat)
  # test run
  m <- run(m, dat)

  # plot
  gplot_stat(m$score[1,],
             outlier = dat$outlier) +
    geom_histogram(aes(stat, fill = outlier, y = ..density..)) +
    stat_function(fun = dnorm)


  gplot_stat(m$pvalue[1,],
             outlier = dat$outlier) +
    geom_histogram(aes(stat, fill = outlier, y = ..density..))

})
