library(testthat)
library(Article3Package)
context("refactor method")

test_that("refactor method on normal dataset", {

  K = 3
  L = 1000
  n = 100
  # sample data
  s <- NormalSampler(n = n,
                     L = L,
                     K = K,
                     prop.outlier = 0.6, # ;)
                     sigma = 0.2,
                     c = 0.5,
                     mean.B = 0.0,
                     sd.mu = 1.0,
                     mean.mu = 0.5)

  ref.method <- refractorMethod(K = K,
                                verbose = FALSE,
                                t = 500)
  lfmmridge.method <- RidgeLFMMMethod(K = K)

  exp <- FDRControlExperiment(nb.rep = 1, s = s,
                              ref.method,
                              lfmmridge.method)
  exp <- runExperiment(exp)
  p <- plot(exp, plot.type = "precision.recall", summary_bin = TRUE, geom = "point")
  print(p)

})

test_that("refactor method on logistic dataset", {

  K = 3
  L = 1000
  n = 100
  # sample data
  s <- LogisticSampler(n = n,
                     L = L,
                     K = K,
                     prop.outlier = 0.6,
                     c = 0.6)

  ref.method <- refractorMethod(K = K,
                                verbose = FALSE,
                                t = 500)
  lfmmridge.method <- RidgeLFMMMethod(K = K)

  exp <- FDRControlExperiment(nb.rep = 1, s = s,
                              ref.method,
                              lfmmridge.method)
  exp <- runExperiment(exp)
  p <- plot(exp, plot.type = "precision.recall", summary_bin = TRUE, geom = "point")
  print(p)

})

test_that("refactor method on there demo dataset", {
  G.file <- "~/Projects/Data2016_2017/refractorDemo/betanormalized_metylationlvl.rds"
  X.file <- "~/Projects/Data2016_2017/refractorDemo/phenotype.rds"

  skip_if_not(file.exists(G.file))
  skip("too long")

  s <- TrueSampler(G.file = G.file,
                   X.file = X.file,
                   outlier.file = NULL,
                   n = NULL,
                   L = NULL)

  dat <- sampl(s)
  # some tests (to test truesampler)
  expect_equal(dim(dat$G), c(200, 25000))
  expect_equal(dim(dat$X), c(200, 1))

  # some plot
  hist(dat$G)
  hist(dat$X)

  # run refactor
  m <- refractorMethod(K = 5,# see https://github.com/cozygene/refactor/blob/master/R/demo.R
                       verbose = TRUE,
                       t = 500)

  # some tests
  expect_equal(class(m), c("refractorMethod", "Method"))

  # run
  m <- run(m, dat)

  # manhattan plot
  gplot_stat(m$pvalue[1,], outlier = dat$outlier) +
    geom_point(aes(x = index, y = -log(stat), color = outlier))

  # qqplot
  gplot_stat(m$pvalue[1,], outlier = dat$outlier) +
    stat_qq(aes(sample = -log10(stat)),
            distribution = stats::qexp, dparams = list(rate = log(10))) +
    geom_abline(slope = 1, intercept = 0) +
    ggtitle("-log10(pvalue) qqplot")

  # test of lfmm ridge on this dataset
  LfmmRidge <- RidgeLFMMMethod(K = 5,
                               hypothesis.testing.method = phenotypeWayReg_lm_score(gif = FALSE),
                               lambda = 10,
                               nickname = "lfmm ridge")
  LfmmRidge <- run(LfmmRidge, dat)

  # qqplot
  gplot_stat(LfmmRidge$pvalue[1,], outlier = dat$outlier) +
    stat_qq(aes(sample = -log10(stat)),
            distribution = stats::qexp, dparams = list(rate = log(10))) +
    geom_abline(slope = 1, intercept = 0) +
    ggtitle("-log10(pvalue) qqplot")

})
