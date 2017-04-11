library(testthat)
context("FDRControlExperiment")

flog.threshold(2)

test_that("FDRControlExperiment on normal dataset without outlier", {

  s <- NormalSampler(50, 500, 5) %>% RandomXSampler(d = 2)
  m1 <- ClassicLinearMethod()
  m2 <- RidgeLFMMMethod(K = 6,
                        lambda = 10)
  m3 <- PCAClassicLinearMethod(K = 6)

  exp <- FDRControlExperiment(10, s, m1, m2, m3)
  #exp

  exp <- runExperiment(exp)
  expect_length(exp$result$df.pvalue,11)
  plot(exp)
})


test_that("FDRControlExperiment on normal dataset with outlier", {

  K <- 2
  s <- NormalSampler(100, 1000, K,
                     c = 0.5)
  m1 <- ClassicLinearMethod()
  m2 <- RidgeLFMMMethod(K = K,
                        lambda = 10)
  m3 <- PCAClassicLinearMethod(K = K, center = TRUE)

  exp <- FDRControlExperiment(5, s, m1, m2, m3)
  #exp

  exp <- runExperiment(exp)
  expect_length(exp$result$df.pvalue,11)
  plot(exp, plot.type = "pvalue.grid", summary_bin = TRUE)
  plot(exp, plot.type = "precision.recall", summary_bin = TRUE)
  plot(exp, plot.type = "qqplot")
})

test_that("FDRControlExperiment on logistic dataset with outlier", {

  K <- 2
  s <- LogisticSampler(100, 1000, K)
  m1 <- ClassicLinearMethod()
  m2 <- RidgeLFMMMethod(K = K,
                        lambda = 10)
  m3 <- PCAClassicLinearMethod(K = K, center = TRUE)
  # m4 <- AlternatedSvdLFMMMethod(K = K,
  #                               lambda = 10,
  #                               it.max = 100,
  #                               err.max = 1e-6,
  #                               hypothesis.testing.method =
  #                                 Zscore())

  # exp
  exp <- FDRControlExperiment(5, s, m1, m2, m3)
  exp <- runExperiment(exp)
  expect_length(exp$result$df.pvalue,11)
  plot(exp, plot.type = "pvalue.grid")
  plot(exp, plot.type = "precision.recall")
  plot(exp, plot.type = "precision.recall", summary_bin = TRUE, geom = "line")
  plot(exp, plot.type = "precision.recall", summary_bin = FALSE, geom = "point", rep.indices = 1)
  plot(exp, plot.type = "qqplot")
})

test_that("FDRControlExperiment on logistic dataset with missing value", {

  K <- 5
  s <- NormalSampler(100, 1000, K,
                     prop.outlier = 0.02,
                     c = 0.8) %>%
    MissingValueSampler(missing.prop = 0.1)

  m1 <- ClassicLinearMethod()
  m2 <- RidgeLFMMMethod(K = K,
                        lambda = 100)
  m3 <- PCAClassicLinearMethod(K = K, center = TRUE)

  exp <- FDRControlExperiment(5, s, m1, m2, m3)
  #exp

  exp <- runExperiment(exp)
  expect_length(exp$result$df.pvalue,11)
  plot(exp, plot.type = "pvalue.grid")
  plot(exp, plot.type = "precision.recall")
  plot(exp, plot.type = "qqplot")
})

test_that("FDRControlExperiment case3", {

  skip_if_not("run it manualy")
  K <- 3
  s <- TrueSampler(G.file = "../../Data2016_2017/SSMPG2015/Case3/Case3.lfmm",
                   X.file = "../../Data2016_2017/SSMPG2015/Case3/Case3.env",
                   outlier.file = "../../Data2016_2017/SSMPG2015/Case3/Case3.outlier")

  m1 <- MadBSigma2Functor() %>%
    Zscore(B.sigma2.functor = .) %>%
    ClassicLinearMethod(hypothesis.testing.method = .)
  m2 <- MadBSigma2Functor() %>%
    Zscore(B.sigma2.functor = .) %>%
    RidgeLFMMMethod(K = K,
                    lambda = 10,
                    hypothesis.testing.method = .)
  m3 <- MadBSigma2Functor() %>%
    Zscore(B.sigma2.functor = .) %>%
    PCAClassicLinearMethod(K = K,
                           hypothesis.testing.method = .)

  exp <- FDRControlExperiment(1, s, m1, m2, m3)
  #exp

  exp <- runExperiment(exp)

  plot(exp, plot.type = "pvalue.grid")
  plot(exp, plot.type = "precision.recall", summary_bin = TRUE, geom = "line")
  plot(exp, plot.type = "qqplot")
})


test_that("FDRControlExperiment case2", {

  skip_if_not("run it manualy")
  K <- 2
  s <- TrueSampler(G.file = "../../Data2016_2017/SSMPG2015/Case2/Case2.lfmm",
                   X.file = "../../Data2016_2017/SSMPG2015/Case2/Case2.env",
                   outlier.file = "../../Data2016_2017/SSMPG2015/Case2/Case2.outlier")

  m1 <- ClassicLinearMethod()
  m2 <- RidgeLFMMMethod(K = K,
                    lambda = 10)
  m3 <- PCAClassicLinearMethod(K = K)

  exp <- FDRControlExperiment(1, s, m1, m2, m3)
  #exp

  exp <- runExperiment(exp)

  plot(exp, plot.type = "pvalue.grid")
  plot(exp, plot.type = "precision.recall", summary_bin = TRUE, geom = "line")
  plot(exp, plot.type = "qqplot")
})

