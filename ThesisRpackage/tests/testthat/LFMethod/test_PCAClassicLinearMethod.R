library(testthat)
context("PCA linear method")

test_that("classic linear regression with PCA on random X", {

  K = 4
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
    RandomXSampler(d = d)
  dat <- sampl(s)

  # run method
  m <- PCAClassicLinearMethod(K = K,
                              center = TRUE)


  # run
  m <- fit(m, dat, light = FALSE)
  m <- run(m, dat)

  ## plot of bootstrap.zscore
  gplot_stat(m$score[1,], m$score[2,], outlier = dat$outlier) +
    geom_histogram(aes(stat, fill = outlier, y = ..density..)) +
    stat_function(fun = dnorm)


})

test_that("classic linear regression with PCA on not random X", {

  K = 4
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
                       mean.mu = 0.5)
  dat <- sampl(s)

  # run method
  m <- PCAClassicLinearMethod(K = K,
                              center = TRUE)


  # run
  m <- fit(m, dat, light = FALSE)
  m <- run(m, dat)

  # plot of bootstrap.zscore
  gplot_stat(m$pvalue[1,], outlier = dat$outlier) +
    geom_point(aes(x = index, y = -log(stat), color = outlier))


})

test_that("light param with finalPcaLm", {

  K = 4
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
                       mean.mu = 0.5)
  dat <- sampl(s)

  ## fit method
  m <- finalPcaLm(K = K)
  m <- fit(m, dat, light = FALSE)
  expect_equal(as.numeric(object.size(m)), 1753272)

  m <- finalPcaLm(K = K)
  m <- fit(m, dat, light = TRUE)
  expect_equal(as.numeric(object.size(m)), 136096)

})

