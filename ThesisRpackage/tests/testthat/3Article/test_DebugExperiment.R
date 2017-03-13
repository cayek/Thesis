library(testthat)
library(Article3Package)
context("DebugExperiment")

test_that("DebugExperiment", {

  m <- ClassicLinearMethod()
  s1 <- NormalSampler(50, 1000, 10)
  s2 <- RandomXSampler(NormalSampler(10, 1000, 3), d = 2)

  exp <- DebugExperiment(method = m,
                         sampler.list = list(s1 = s1, s2 = s2),
                         parameter.list = list(center = c(TRUE, FALSE)))

  exp <- runExperiment(exp)
  # expect_equal(dim(exp$result$res.df), c(12000, 6))
  plot(exp, filter.exp = "center == TRUE")
  plot(exp, filter.exp = 'center == FALSE & variable.name == "pvalue1"',
             geom = function() ggplot2::geom_histogram(bins = 30, aes(estimate, fill = outlier)))
})

