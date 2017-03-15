library(testthat)
context("tess3_noisyCoord")

test_that("long_tess3_noisyCoord", {

  exp <- long_tess3_noisyCoord(n = 10,
                        cluster.nb = NULL,
                        nb.rep = 1,
                        noise.signal = c(0.0, 1.0, 3.0),
                        save = FALSE, bypass = TRUE)
  expect_equal(dim(exp$df.res), c(5, 5))

  ## plot
  plot_tess3_noisyCoord(exp)

})
