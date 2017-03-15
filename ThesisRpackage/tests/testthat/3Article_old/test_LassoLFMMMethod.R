library(testthat)
library(Article3Package)
context("lasso lfmm method")


test_that("lasso lfmm on binomial dataset with outlier", {

  # sample data
  K <- 5
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

  m <- LassoLFMMMethod(K = K + 2,
                       lambda = 1e-2,
                       it.max = 100,
                       err.max = 1e-6)
  m <- run(m, dat)

  gplot_stat(m$B[1,], dat$B[1,], outlier = dat$outlier) +
    geom_point(aes(x = index, y = stat, color = outlier))

})

################################################################################
# regularization path

test_that("lasso Regularization path lfmm on binomial dataset with outlier", {

  # sample data
  K <- 5
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

  m <- LassoLFMMMethod(K = K + 2, # ;D
                       lambda = NULL,
                       lambda.K = 20,
                       lambda.eps = 0.001,
                       it.max = 100,
                       err.max = 1e-6)
  # m <- run(m, dat)
  # gplot_stat(m$score[1,], dat$B[1,], outlier = dat$outlier) +   geom_point(aes(x = index, y = stat, color = outlier))

  # FdrControl exp
  exp <- FDRControlExperiment(nb.rep = 1, s = s,
                              m)
  exp <- runExperiment(exp)
  p <- plot(exp, plot.type = "precision.recall", summary_bin = TRUE, geom = "point")
  print(p)
})

################################################################################
# sparse proportion

test_that("Test of sparse proportion feature", {

  # sample data
  K <- 5
  prop <- 0.1
  s <- LogisticSampler(n = 100,
                       L = 1000,
                       K = K,
                       prop.outlier = prop,
                       c = 0.6,
                       mean.B = 0.0,
                       sd.mu = 1.0,
                       mean.mu = 0.5)
  dat <- sampl(s)

  m <- LassoLFMMMethod(K = K,
                       sparse.prop = prop,
                       lambda = NULL,
                       lambda.K = 100,
                       lambda.eps = 0.001,
                       it.max = 100,
                       err.max = 1e-6)
  m <- run(m, dat)

  expect_lte(abs(mean(m$B != 0.0) - prop), 0.01)
  gplot_stat(m$B[1,], dat$B[1,], outlier = dat$outlier) +
    geom_point(aes(x = index, y = stat, color = outlier))

})

################################################################################
# SSMPG dataset

test_that("lasso lfmm on case3", {

  skip_if_not("run it manualy")

  K <- 3
  s <- TrueSampler(G.file = "../../Data2016_2017/SSMPG2015/Case3/Case3.lfmm",
                   X.file = "../../Data2016_2017/SSMPG2015/Case3/Case3.env",
                   outlier.file = "../../Data2016_2017/SSMPG2015/Case3/Case3.outlier")
  dat <- sampl(s)

  # lambda = 0.0
  m <- LassoLFMMMethod(K = K,
                       lambda = 1e-2,
                       it.max = 10,
                       err.max = 1e-6)
  m <- run(m, dat)

  gplot_stat(m$B[1,], outlier = dat$outlier) +
    geom_point(aes(x = index, y = stat, color = outlier))

})


test_that("lasso lfmm on case2", {

  skip_if_not("run it manualy")

  K <- 6
  s <- TrueSampler(G.file = "../../Data2016_2017/SSMPG2015/Case2/Case2.lfmm",
                   X.file = "../../Data2016_2017/SSMPG2015/Case2/Case2.env",
                   outlier.file = "../../Data2016_2017/SSMPG2015/Case2/Case2.outlier")
  dat <- sampl(s)

  # lambda = 0.0
  m <- LassoLFMMMethod(K = K,
                       lambda = 1e-2,
                       it.max = 10,
                       err.max = 1e-6)
  m <- run(m, dat)

  gplot_stat(m$B[1,], outlier = dat$outlier) +
    geom_point(aes(x = index, y = stat, color = outlier))

})
