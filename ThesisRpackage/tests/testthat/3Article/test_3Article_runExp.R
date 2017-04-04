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

  Article3_runExp_plotB(exp, threshold = 0.05, lambda = 1e-5)
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

  exp <- Article3_runExp(dat = dat,
                         dat.name = "NormalSampler2",
                         methods = methods,
                         cluster.nb = NULL,
                         save = FALSE, bypass = TRUE)
  expect_equal(exp$name, "Article3_runExp")

  # plot(exp, threshold = 0.05, 1e5)
})
