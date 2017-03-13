library(testthat)
library(Article3Package)
context("ComparisonExperiment")

test_that("ComparisonExperiment on normal dataset", {

  K <- 5
  s <- NormalSampler(100, 1000, K,
                     prop.outlier = 0.02,
                     c = 0.8) %>%
    MissingValueSampler(missing.prop = 0.0)

  m1 <- ClassicLinearMethod()
  m2 <- RidgeLFMMMethod(K = K,
                        lambda = 0.01)
  m3 <- PCAClassicLinearMethod(K = K, center = TRUE)


  exp <- ComparisonExperiment(s, m1, m2, m3)
  exp <- runExperiment(exp)
  plot(exp)

})


test_that("ComparisonExperiment on Case3", {

  skip("run it manualy")

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


  exp <- ComparisonExperiment(s, m1, m2, m3)
  exp <- runExperiment(exp)
  plot(exp, variable.name.regexp = c("score"))
  plot(exp, variable.name.regexp = c("B"))

  # plot histo of pvalue
  p <- plot(exp, variable.name.regexp = c("pvalue"))
  p$layers <- list()
  p + geom_histogram(aes(estimate, fill = outlier))
})


test_that("ComparisonExperiment on Case3", {

  skip("run it manualy")

  K <- 2
  s <- TrueSampler(G.file = "../../Data2016_2017/SSMPG2015/Case2/Case2.lfmm",
                   X.file = "../../Data2016_2017/SSMPG2015/Case2/Case2.env",
                   outlier.file = "../../Data2016_2017/SSMPG2015/Case2/Case2.outlier")

  m1 <- AnalyticSigma2Functor() %>%
    Zscore(B.sigma2.functor = .) %>%
    ClassicLinearMethod(hypothesis.testing.method = .)
  m2 <- AnalyticSigma2Functor() %>%
    Zscore(B.sigma2.functor = .) %>%
    RidgeLFMMMethod(K = K,
                    lambda = 10,
                    hypothesis.testing.method = .)
  m3 <- AnalyticSigma2Functor() %>%
    Zscore(B.sigma2.functor = .) %>%
    PCAClassicLinearMethod(K = K,
                           hypothesis.testing.method = .)


  exp <- ComparisonExperiment(s, m1, m2, m3)
  exp <- runExperiment(exp)
  plot(exp, variable.name.regexp = c("score"))
  plot(exp, variable.name.regexp = c("B"))

  # plot histo of pvalue
  p <- plot(exp, variable.name.regexp = c("pvalue"))
  p$layers <- list()
  p + geom_histogram(aes(estimate, fill = outlier))
})
