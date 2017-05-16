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


test_that("MethodBatchExperiment list", {

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

  ## qvalue
  res.df <- MethodBatchExperiment_qvalue(expr, 0.05)

  ## candidate top
  top <- 30 
  res.df <- MethodBatchExperiment_candidates(expr, top = top, print = TRUE)
  expect_equal(dim(res.df), c(top * 2, 6))

  ## candidate fdr
  fdr.threshold <- 0.05
  res.df <- MethodBatchExperiment_candidates(expr, fdr.threshold = fdr.threshold)

  ## coutn intersect
  fdr.threshold <- 0.01
  res.df <- MethodBatchExperiment_count_intersect(expr, fdr.threshold = fdr.threshold,
                                                  plot = NULL)

  top <- 30
  MethodBatchExperiment_count_intersect(expr, top = top, plot = "point")
  MethodBatchExperiment_count_intersect(expr, top = top, plot = "tile")
  MethodBatchExperiment_count_intersect(expr, fdr.threshold = 0.05, plot = "point")

})
