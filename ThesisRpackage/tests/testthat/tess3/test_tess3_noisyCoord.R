library(testthat)
context("tess3_noisyCoord")

test_that("long_tess3_noisyCoord", {

  options(tess3.ms = "~/BiocompSoftware/msdir/ms")
  exp <- long_tess3_noisyCoord(ns = c(50),
                               nsites.neutral = c(1.5 * 1e4),
                               m.neutral =  c(0.25 * 1 * 1e-6,
                                              0.25 * 5 * 1e-6),
                               noise.signal = c(0.0, 1.0),
                               nb.rep = 2,
                               compute.vario = FALSE,
                               cluster.nb = NULL,
                               save = FALSE, bypass = TRUE)
  expect_equal(dim(exp$df.res), c(8, 12))

  ## plot
  plot_tess3_noisyCoord(exp)

}) p

test_that("long_tess3_noisyCoord variogram", {

  options(tess3.ms = "~/BiocompSoftware/msdir/ms")
  exp <- long_tess3_noisyCoord(ns = c(50),
                               nsites.neutral = c(1.5 * 1e4),
                               m.neutral =  c(0.25 * 1 * 1e-6),
                               noise.signal = c(0.0, 1.0, 10.0),
                               nb.rep = 1,
                               compute.vario = TRUE,
                               cluster.nb = NULL,
                               save = FALSE, bypass = TRUE)

  ## plot
  plot_tess3_noisyCoord_vario(exp)

})
