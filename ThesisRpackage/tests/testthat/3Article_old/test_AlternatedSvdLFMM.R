library(testthat)
library(Article3Package)
library(purrr)
context("alternated svd lfmm method")

test_that("test of alternated svd lfmm method on logistic random dataset with missing value", {

  K = 6
  L = 1000
  d = 2
  n = 100
  # sample data
  s <- LogisticSampler(n = n,
                     L = L,
                     K = K,
                     ploidy = 2,
                     prop.outlier = 0.02,
                     sigma = 0.2,
                     c = 0.6,
                     mean.B = 0.0,
                     sd.mu = 1.0,
                     mean.mu = 0.5) %>%
    RandomXSampler(d = d) %>%
    MissingValueSampler(missing.prop = 0.1)
  dat <- sampl(s)



  # method
  m <- AlternatedSvdLFMMMethod(K = K,
                               lambda = 10,
                               it.max = 100,
                               err.max = 1e-6,
                               hypothesis.testing.method =
                                 Zscore(NormalZscore(),
                                        Bootstrap(
                                          residualBootstrapLFMMWithMissing,
                                          30)))

  # run
  m <- fit(m, dat)
  m <- run(m, dat)


  # plot
  gplot_stat(m$score[1,], m$score[2,],
             outlier = dat$outlier) +
    geom_histogram(aes(stat, fill = outlier, y = ..density..)) +
    stat_function(fun = dnorm)

  gplot_stat(m$B.sigma2[1,], m$B.sigma2[2,],
             m$B[1,], m$B[2,],
             outlier = dat$outlier) +
    geom_point(aes(x = index, color = outlier, y = stat))

  gplot_stat(m$pvalue[1,], m$pvalue[2,],
             outlier = dat$outlier) +
    geom_histogram(aes(stat, fill = outlier, y = ..density..))
})

test_that("test of alternated svd lfmm method on logistic random dataset with missing value BUT we impute first :D", {

  K = 6
  L = 1000
  d = 2
  n = 100
  # sample data
  s <- LogisticSampler(n = n,
                       L = L,
                       K = K,
                       ploidy = 2,
                       prop.outlier = 0.02,
                       sigma = 0.2,
                       c = 0.6,
                       mean.B = 0.0,
                       sd.mu = 1.0,
                       mean.mu = 0.5) %>%
    RandomXSampler(d = d) %>%
    MissingValueSampler(missing.prop = 0.5)
  dat <- sampl(s) %>% imputeMeanDataSet()



  # method
  m <- AlternatedSvdLFMMMethod(K = K,
                               lambda = 10,
                               it.max = 100,
                               err.max = 1e-6,
                               hypothesis.testing.method =
                                 Zscore(NormalZscore(),
                                        Bootstrap(
                                          residualBootstrapLFMMWithMissing,
                                          30)))

  # run
  m <- fit(m, dat)
  m <- run(m, dat)


  # plot
  gplot_stat(m$score[1,], m$score[2,],
             outlier = dat$outlier) +
    geom_histogram(aes(stat, fill = outlier, y = ..density..)) +
    stat_function(fun = dnorm)


  cat("Ok, we have a good N(0,1) so standard deviation is well approximated...")
})
