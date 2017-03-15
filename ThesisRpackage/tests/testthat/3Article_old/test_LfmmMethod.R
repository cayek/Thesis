library(testthat)
library(Article3Package)
context("lfmm method")

################################################################################
# with the residual boostrap

test_that("linear ridge lfmm mode with residualBootstrapLFMM hypothesis testing", {

  K = 6
  # sample data
  s <- RandomXSampler(NormalSampler(n = 100,
                                    L = 1000,
                                    K = K,
                                    prop.outlier = 0.02,
                                    sigma = 0.2,
                                    c = 0.6,
                                    mean.B = 0.0,
                                    sd.mu = 1.0,
                                    mean.mu = 0.5), d = 2)
  dat <- sampl(s)

  # method
  m <- RidgeLFMMMethod(K = K, # better K is greater or equal than the true K otherwise we have bad zscore because individual are correlated.
                       hypothesis.testing.method =
                         Zscore(NormalZscore(),
                                Bootstrap(residualBootstrapLFMM,
                                          100)))
  expect_equal(name(m), "RidgeLFMMMethod|Zscore|NormalZscore|Bootstrap")

  # test fit
  m <- fit(m, dat)
   # test run
  m <- run(m, dat)

  # normality test
  expect_gte(shapiro.test(m$score[1,])$p.value, 0.05) # If score was under normal distribution, what we observe hapen more than one time on 10
  expect_gte(shapiro.test(m$score[2,])$p.value, 0.05) # If score was under normal distribution, what we observe hapen more than one time on 10

    # plot
  gplot_stat(m$score[1,], m$score[2,],
             outlier = dat$outlier) +
    geom_histogram(aes(stat, fill = outlier, y = ..density..)) +
    stat_function(fun = dnorm)

  gplot_stat(m$B.sigma2[1,], m$B.sigma2[2,],
             m$B[1,], m$B[2,],
             outlier = dat$outlier) +
    geom_point(aes(x = index, color = outlier, y = stat)) # grosse variance à la fin car c'est la qu'il y a les outlier.

  gplot_stat(m$pvalue[1,], m$pvalue[2,],
             outlier = dat$outlier) +
    geom_histogram(aes(stat, fill = outlier, y = ..density..))

})



test_that("linear ridge lfmm mode with residualBootstrapLFMM hypothesis testing on 0,1,2 dataset", {

  set.seed(88)
  K = 3
  # sample data
  s <- RandomXSampler(LogisticSampler(n = 100,
                                      L = 1000,
                                      K = K,
                                      prop.outlier = 0.02,
                                      sigma = 0.2,
                                      c = 0.6,
                                      mean.B = 0.0,
                                      sd.mu = 1.0,
                                      mean.mu = 0.5), d = 2)
  dat <- sampl(s)

  # method
  m <- RidgeLFMMMethod(K = K, # better K is greater or equal than the true K otherwise we have bad zscore because individual are correlated.,
                       lambda = 10, # to add numerical stability
                       hypothesis.testing.method =
                         Zscore(NormalZscore(),
                                Bootstrap(
                                  residualBootstrapLFMM,
                                  100)))
  expect_equal(name(m), "RidgeLFMMMethod|Zscore|NormalZscore|Bootstrap")

  # test fit
  m <- fit(m, dat)
  # test run
  m <- run(m, dat)

  # normality test
  expect_gte(shapiro.test(m$score[1,])$p.value, 0.25) # If score was under normal distribution, what we observe hapen more than one time on 2
  expect_gte(shapiro.test(m$score[2,])$p.value, 0.05)

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

################################################################################
# with the paired boostrap


test_that("linear ridge lfmm mode with pairedBootstrap hypothesis testing on 0,1,2 dataset", {

  set.seed(88)
  K = 3
  # sample data
  s <- RandomXSampler(LogisticSampler(n = 100,
                                      L = 1000,
                                      K = K,
                                      prop.outlier = 0.02,
                                      sigma = 0.2,
                                      c = 0.6,
                                      mean.B = 0.0,
                                      sd.mu = 1.0,
                                      mean.mu = 0.5), d = 2)
  dat <- sampl(s)

  # method
  m <- RidgeLFMMMethod(K = K, # better K is greater or equal than the true K otherwise we have bad zscore because individual are correlated.,
                       lambda = 10, # to add numerical stability
                       hypothesis.testing.method =
                         Zscore(NormalZscore(),
                                Bootstrap(
                                  PairedBoostrap,
                                  100)))
  expect_equal(name(m), "RidgeLFMMMethod|Zscore|NormalZscore|Bootstrap")

  # test fit
  m <- fit(m, dat)
  # test run
  m <- run(m, dat)

  # normality test
  expect_gte(shapiro.test(m$score[1,])$p.value, 0.25)
  expect_gte(shapiro.test(m$score[2,])$p.value, 0.05)

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


################################################################################
# with the calculated B.sigma2



test_that("linear ridge lfmm calculated B.sigma2 hypothesis testing on 0,1,2 dataset without association", {

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


  # method
  m <- RidgeLFMMMethod(K = K,
                       lambda = 10, # to add numerical stability
                       hypothesis.testing.method =
                         Zscore(NormalZscore(),
                                AnalyticSigma2Functor()))


  m <- fit(m, dat)
  m <- run(m, dat)

  # plot of zscore
  gplot_stat(m$score[1,], m$score[2,], outlier = dat$outlier) +
    geom_histogram(aes(stat, fill = outlier, y = ..density..)) +
    stat_function(fun = dnorm)
  # normality test
  expect_gte(shapiro.test(m$score[1,])$p.value, 0.05)
  expect_gte(shapiro.test(m$score[2,])$p.value, 0.05)


  # plot of G B.sigma2
  gplot_stat(m$B.sigma2[1,], m$B.sigma2[2,],
             m$B[1,], m$B[2,],
             outlier = dat$outlier) +
    geom_point(aes(x = index, color = outlier, y = stat))

  # analytic.sigma2.pvalue
  gplot_stat(m$pvalue[1,], m$pvalue[2,],
             outlier = dat$outlier) +
    geom_histogram(aes(stat, fill = outlier, y = ..density..))

})


