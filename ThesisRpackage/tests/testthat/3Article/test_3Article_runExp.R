library(testthat)
context("3Article_runExp")

flog.threshold(2)

test_that("Article3_runExp", {

  s <- NormalSampler2(n = 100,
                      L = 1000,
                      K = 3)
  dat <- sampl(s)

  methods <- paramGrid(finalLfmmRdigeMethod,
                       nickname.base = "RidgeLfmm",
                       K = c(2,3,4),
                       lambda = c(1e-5, 1e0, 1e5))

  exp <- Article3_runExp(dat = dat,
                         dat.name = "NormalSampler2",
                         methods = methods,
                         cluster.nb = NULL,
                         save = FALSE, bypass = TRUE)
  expect_equal(exp$name, "Article3_runExp")
  expect_equal(exp$description,
               "Article3_runExp with methods=RidgeLfmm lambdas=1e-05|1|1e+05 Ks=2|3|4 sparse.prop=NA dat.name=NormalSampler2 ")

  Article3_runExp_plotB(exp, threshold = 0.05, method = "RidgeLfmm")
})

test_that("Article3_runExp on several method", {

  s <- NormalSampler2(n = 100,
                      L = 1000,
                      K = 3)
  dat <- sampl(s)
  methods <- paramGrid(finalLfmmRdigeMethod,
                       nickname.base = "RidgeLfmm",
                       K = c(2,3,4),
                       lambda = c(1e-5, 1e0, 1e5))
  methods <-  c(methods, list(finalLfmmLassoMethod(K = 3, sparse.prop = 0.1)))
  methods <- c(methods, list(finalRefactorMethod(K = 3)))
  exp <- Article3_runExp(dat = dat,
                         dat.name = "NormalSampler2",
                         methods = methods,
                         cluster.nb = NULL,
                         save = FALSE, bypass = TRUE)
  expect_equal(exp$name, "Article3_runExp")

  ## lasso plot
  Article3_runExp_manhattan(exp, threshold = 0.05, method.name = "LassoLfmm")
  Article3_runExp_plotB(exp, threshold = 0.05, method.name = "LassoLfmm")

  ## refactor plot
  Article3_runExp_manhattan(exp, threshold = 0.05, method.name = "Refactor")
  Article3_runExp_plotB(exp, threshold = 0.05, method.name = "Refactor")

  ## lfmm plot
  Article3_runExp_manhattan(exp, threshold = 0.05, method.name = "RidgeLfmm")
  Article3_runExp_plotB(exp, threshold = 0.05, method.name = "RidgeLfmm")


})

test_that("Article3_runExp_calibrate", {

  s <- NormalSampler2(n = 100,
                      L = 1000,
                      K = 3)
  dat <- sampl(s)
  methods <- list()
  ## methods <- paramGrid(finalLfmmRdigeMethod,
  ##                      nickname.base = "RidgeLfmm",
  ##                      K = c(2,3,4),
  ##                      lambda = c(1e-5, 1e0, 1e5))
  ## methods <-  c(methods, list(finalLfmmLassoMethod(K = 3, sparse.prop = 0.1)))
  methods <- c(methods, list(finalRefactorMethod(K = 1)))
  exp <- Article3_runExp(dat = dat,
                         dat.name = "NormalSampler2",
                         methods = methods,
                         cluster.nb = NULL,
                         save = FALSE, bypass = TRUE)

  Article3_runExp_hist(exp, 0.05, "Refactor")
  calibration.function <- function(score) {
    res <- locfdr::locfdr(as.numeric(score), df = 8, plot = TRUE)
    res$fdr
  }
  exp.calibrated <- Article3_runExp_calibrate(exp, calibration.function = calibration.function )
  Article3_runExp_hist(exp.calibrated, 0.05, "Refactor")
})

