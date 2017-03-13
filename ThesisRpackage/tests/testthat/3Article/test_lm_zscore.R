library(testthat)
library(Article3Package)
context("lm_score")

# the test used here
test <- function(s, K, lambda, nb.rep, sparse.prop = NULL, lasso = FALSE, gif = TRUE) {
  LfmmRidge <- RidgeLFMMMethod(K = K,
                               hypothesis.testing.method = lm_zscore(gif = gif),
                               lambda = lambda,
                               nickname = "lfmm ridge")

  LfmmLasso <- LassoLFMMMethod(K = K,
                               it.max = 200,
                               err.max = 1e-6,
                               lambda = NULL,
                               lambda.K = 100,
                               lambda.eps = 0.001,
                               sparse.prop = sparse.prop,
                               hypothesis.testing.method = lm_zscore(gif = gif),
                               nickname = "lfmm lasso")

  lm <- AnalyticSigma2Functor() %>%
    Zscore(B.sigma2.functor = .) %>%
    ClassicLinearMethod(hypothesis.testing.method = .,
                        nickname = "lm")

  lmPca <- PCAClassicLinearMethod(K = K,
                                  #hypothesis.testing.method = lm_zscore(),
                                  nickname = "PCA+lm")

  #### Hypothesis testing comparison
  cat("Hypothesis testing comparison \n")
  if (lasso) {
    exp <- FDRControlExperiment(nb.rep = nb.rep, s = s,
                                LfmmRidge,
                                lm,
                                lmPca,
                                LfmmLasso)
  } else {
    exp <- FDRControlExperiment(nb.rep = nb.rep, s = s,
                                LfmmRidge,
                                lm,
                                lmPca)
  }

  exp <- runExperiment(exp)
  p <- plot(exp, plot.type = "pvalue.grid", summary_bin = TRUE)
  print(p)
  p <- plot(exp, plot.type = "precision.recall", summary_bin = TRUE, geom = "line")
  print(p)
}


test_that("test on normal dataset", {

  K = 6
  # sample data
  s <- NormalSampler(n = 100,
                     L = 1000,
                     K = K,
                     prop.outlier = 0.1,
                     sigma = 0.2,
                     c = 0.6,
                     mean.B = 0.0,
                     sd.mu = 1.0,
                     mean.mu = 0.5)
  dat <- sampl(s)
  m <- RidgeLFMMMethod(K = K,
                       hypothesis.testing.method = lm_zscore(),
                       lambda = 10,
                       nickname = "lfmm ridge")
  m <- run(m, dat)

  gplot_stat(m$score[1,],
             outlier = dat$outlier) +
    geom_point(aes(x = index, color = outlier, y = stat))


})

test_that("comparison on normal dataset", {

  skip("run it manualy")

  K = 3
  # sample data
  s <- NormalSampler(n = 100,
                     L = 1000,
                     K = K,
                     prop.outlier = 0.1,
                     sigma = 0.2,
                     c = 0.6,
                     mean.B = 0.0,
                     sd.mu = 1.0,
                     mean.mu = 0.5)
  test(s, K, lambda = 0.01, nb.rep = 3)
  test(s, K, lambda = 10, nb.rep = 3)
  test(s, K, lambda = 1e5, nb.rep = 3)

})


test_that("comparison on logistic dataset", {

  skip("run it manualy")

  K = 3
  # sample data
  s <- LogisticSampler(n = 100,
                       L = 1000,
                       K = K,
                       prop.outlier = 0.2,
                       c = 0.6,
                       mean.B = 0.0,
                       sd.mu = 1.0,
                       mean.mu = 0.5)
  test(s, K, lambda = 0.01, nb.rep = 2, gif = TRUE, lasso = TRUE, sparse.prop = 0.3)
  test(s, K, lambda = 10, nb.rep = 5, gif = TRUE)
  test(s, K, lambda = 1e5, nb.rep = 5, gif = TRUE)


})

