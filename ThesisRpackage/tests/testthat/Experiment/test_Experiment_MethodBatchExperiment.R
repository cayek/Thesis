library(testthat)
context("MethodBatchExperiment")


test_that("MethodBatchExperiment run", {

  s <- NormalSampler(50, 500, 5)
  method.batch <- list()
  method.batch$m1 <- finalLm()
  method.batch$m2 <- finalLfmmRdigeMethod(K = 6,
                                          1e-1)
  expr <- MethodBatchExperiment("normal",
                                s,
                                method.batch,
                                cluster.nb = NULL)
  expect_equal(expr$description, "Run on normal with lm|RidgeLfmm|")


  expr <- runExperiment(expr)


  ## qqplot
  MethodBatchExperiment_qqplot(expr)
})

test_that("MethodBatchExperiment calibrate", {

  s <- NormalSampler(50, 500, 5)
  method.batch <- list()
  method.batch$m1 <- finalLm()
  method.batch$m2 <- finalLfmmRdigeMethod(K = 6,
                                          1e-1)
  expr <- MethodBatchExperiment("normal",
                                s,
                                method.batch,
                                cluster.nb = NULL)

  expr <- runExperiment(expr)


  MethodBatchExperiment_qqplot(expr)
  ## calibrate
  expr <- MethodBatchExperiment_calibrate(expr)
  ## qqplot
  MethodBatchExperiment_qqplot(expr)
  
})
