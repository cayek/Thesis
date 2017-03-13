library(testthat)
library(Article3Package)
context("likelyhood ratio")


test_that("test of loglr.ClassicLinearMethod", {

  K = 6
  L = 1000
  d = 2
  # sample data
  s <- RandomXSampler(LogisticSampler(n = 100,
                                      L = L,
                                      K = K,
                                      prop.outlier = 0.02,
                                      sigma = 0.2,
                                      c = 0.6,
                                      mean.B = 0.0,
                                      sd.mu = 1.0,
                                      mean.mu = 0.5), d = d)
  dat <- sampl(s)

  # method
  m <- ClassicLinearMethod()
  m <- fit(m, dat)

  # loglr
  log.lr <- loglr(m, dat)
  expect_equal(dim(log.lr), c(d, L))
  expect_equal(mean(log.lr > 0), 0) # it must be negative
})


test_that("test of log likelyhood ratio test with ClassicLinearMethod", {

  skip("Too long ! and likelyhood not realy implemented ;-) see labnotebook")

  K = 6
  L = 2000
  d = 2
  # sample data
  s <- RandomXSampler(LogisticSampler(n = 100,
                                      L = L,
                                      K = K,
                                      prop.outlier = 0.02,
                                      sigma = 0.2,
                                      c = 0.6,
                                      mean.B = 0.0,
                                      sd.mu = 1.0,
                                      mean.mu = 0.5), d = d)
  dat <- sampl(s)

  # method
  m <- ClassicLinearMethod(hypothesis.testing.method = LikelyhoodRatio())
  m <- run(m, dat)

  # plot of lr.score
  gplot_stat(m$score[1,], m$score[2,],
             outlier = dat$outlier) +
    geom_histogram(aes(stat, fill = outlier, y = ..density..)) +
    stat_function(fun = dchisq, args = list(df = 1), xlim = c(0.5,10))

  # plot of pvalue
  gplot_stat(m$pvalue[1,], m$pvalue[2,],
             outlier = dat$outlier) +
    geom_histogram(aes(stat, fill = outlier, y = ..density..))

  # test of Kolmogorov-Smirnov
  ks.test(m$score[1,], y = pchisq, df = 1)
  ks.test(m$score[2,], y = pchisq, df = 1)
  ## we can reject H0 : the score ~ chisq(df = 1) ... I think this is because G_i are not i.i.d ...

})
