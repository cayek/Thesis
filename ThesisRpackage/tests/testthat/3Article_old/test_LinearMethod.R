library(testthat)
library(Article3Package)
context("linear method")


################################################################################
# classic Linear method

test_that("classic linear regression with computed B.sigma2 hypothesis testing", {
  set.seed(465)
  # sample data
  s <- RandomXSampler(NormalSampler(n = 50,
                                    L = 1000,
                                    K = 2,
                                    prop.outlier = 0.02,
                                    sigma = 0.2,
                                    c = 0.6,
                                    mean.B = 0.0,
                                    sd.mu = 1.0,
                                    mean.mu = 0.5), d = 2)
  dat <- sampl(s)



  # run method
  m <- ClassicLinearMethod(center = TRUE)
  print(m)
  expect_equal(name(m), "ClassicLinearMethod|Zscore|NormalZscore|AnalyticSigma2")

  m <- fit(m, dat)
  m <- run(m, dat)

  # plot of analytic.sigma2.zscore
  gplot_stat(m$score[1,], m$score[2,], outlier = dat$outlier) +
    geom_histogram(aes(stat, fill = outlier, y = ..density..)) +
    stat_function(fun = dnorm)

  # plot of G B.sigma2
  gplot_stat(m$B.sigma2[1,], m$B.sigma2[2,],
             m$B[1,], m$B[2,],
             outlier = dat$outlier) +
    geom_point(aes(x = index, color = outlier, y = stat))

  # analytic.sigma2.pvalue
  gplot_stat(m$pvalue[1,], m$pvalue[2,],
             outlier = dat$outlier) +
    geom_histogram(aes(stat, fill = outlier, y = ..density..))

  cat("It does not work, is not it ?? :-D, I think because G_i are not iid")

})

test_that("classic linear regression with pairedbootstrap hypothesis testing", {
  set.seed(465432)
  # sample data
  s <- RandomXSampler(NormalSampler(n = 100,
                                    L = 1000,
                                    K = 3,
                                    prop.outlier = 0.02,
                                    sigma = 0.2,
                                    c = 0.6,
                                    mean.B = 0.0,
                                    sd.mu = 1.0,
                                    mean.mu = 0.5), d = 2)
  dat <- sampl(s)



  # run method
  m <- ClassicLinearMethod(center = TRUE,
                           hypothesis.testing.method =
                             Zscore(NormalZscore(),
                                    Bootstrap(PairedBoostrap,
                                              100)))
  print(m)
  expect_equal(name(m), "ClassicLinearMethod|Zscore|NormalZscore|Bootstrap")

  m <- fit(m, dat)
  m <- run(m, dat)

  # plot of bootstrap.zscore
  gplot_stat(m$score[1,], m$score[2,],
             outlier = dat$outlier) +
    geom_histogram(aes(stat, fill = outlier, y = ..density..)) +
    stat_function(fun = dnorm)

  # plot of G B.sigma2
  gplot_stat(m$B.sigma2[1,], m$B.sigma2[2,],
             m$B[1,], m$B[2,],
             outlier = dat$outlier) +
    geom_point(aes(x = index, color = outlier, y = stat))

  # analytic.sigma2.pvalue
  gplot_stat(m$pvalue[1,], m$pvalue[2,],
             outlier = dat$outlier) +
    geom_histogram(aes(stat, fill = outlier, y = ..density..))

  cat("It does not work when K > 0, because individual are correlated !")

})


test_that("classic linear regression with residualBootstrapLM hypothesis testing", {
  set.seed(46543)
  # sample data
  s <- RandomXSampler(NormalSampler(n = 100,
                                    L = 1000,
                                    K = 1,
                                    prop.outlier = 0.02,
                                    sigma = 0.2,
                                    c = 0.6,
                                    mean.B = 0.0,
                                    sd.mu = 1.0,
                                    mean.mu = 0.5), d = 2)
  dat <- sampl(s)



  # run method
  m <- ClassicLinearMethod(center = TRUE,
                           hypothesis.testing.method =
                             Zscore(NormalZscore(),
                                    Bootstrap(residualBootstrapLM,
                                              100)))
  print(m)
  expect_equal(name(m), "ClassicLinearMethod|Zscore|NormalZscore|Bootstrap")

  m <- fit(m, dat)
  m <- run(m, dat)

  # plot of bootstrap.zscore
  gplot_stat(m$score[1,], m$score[2,],
             m$B[1,], m$B[2,], outlier = dat$outlier) +
    geom_histogram(aes(stat, fill = outlier, y = ..density..)) +
    stat_function(fun = dnorm)

  # plot of G B.sigma2
  gplot_stat(m$B.sigma2[1,], m$B.sigma2[2,],
             m$B[1,], m$B[2,],
             outlier = dat$outlier) +
    geom_point(aes(x = index, color = outlier, y = stat))

  # analytic.sigma2.pvalue
  gplot_stat(m$pvalue[1,], m$pvalue[2,],
             outlier = dat$outlier) +
    geom_histogram(aes(stat, fill = outlier, y = ..density..))

  cat("It does not work when K > 0, because individual are correlated !")

})

test_that("classic linear regression with missing value", {

  # sample data
  s <- NormalSampler(n = 70,
                     L = 1000,
                     K = 0, # otherwise indiv are correlated and we need a linear model with correlated noise ;D. Here we want observe a normal distribution.
                     prop.outlier = 0.02,
                     sigma = 0.2,
                     c = 0.6,
                     mean.B = 0.0,
                     sd.mu = 1.0,
                     mean.mu = 0.5) %>%
    RandomXSampler(d = 2) %>%
    MissingValueSampler(missing.prop = 0.5)
  dat <- sampl(s)

  # run method
  m <- ClassicLinearMethod()
  options(Article3Package.debug = "TRUE")
  expect_message(m <- run(m, dat), "Missing values detected")
  missing.index <- which(is.na(dat$G))
  expect_equal(missing.index, m$missing.index)
  expect_equal(length(m$imputed.values), length(missing.index))

  # plot of analytic.sigma2.zscore
  dat <- sampl(s)
  m <- run(m, dat)
  gplot_stat(m$score[1,], m$score[2,], outlier = dat$outlier) +
    geom_histogram(aes(stat, fill = outlier, y = ..density..)) +
    stat_function(fun = dnorm)
  ## ok this work !

  ## what happen if we impute data first
  dat <- sampl(s)
  dat <- imputeZeroDataSet(dat)
  expect_false(anyNA(dat$G))
  m <- run(m, dat)
  gplot_stat(m$score[1,], m$score[2,], outlier = dat$outlier) +
    geom_histogram(aes(stat, fill = outlier, y = ..density..)) +
    stat_function(fun = dnorm)
  ## we also have normal distribution, I think because data are still i.i.d and
  ## we only need that for this property.

})


