library(testthat)
context("tess3_tess3RImplementation")

test_that("tess3_tess3RImplementation", {

  skip("Abort, bad idea")
  skip_if_not_installed("tess3r")

  ## data
  s <- Tess3Sampler(n = 50,
                    nsites.neutral = 500,
                    nsites.selected = 0,
                    min.maf = 0.05)
  dat <- sampl(s)

  ## run tess3r
  set.seed(324024)
  m.tess3r <- tess3Method(K = 2,
                   lambda = 1e0)
  m.tess3r <- fit(m.tess3r, dat)

  ## run tess3RImplementation
  set.seed(324024)
  m.tess3RImplementation <- tess3RImplementationMethod(K = 2,
                          lambda = 1e0)
  m.tess3RImplementation <- fit(m.tess3RImplementation, dat)

  ## comp
  expect_equal(mean(m.tess3r$Q - m.tess3RImplementation$Q), 0)
  expect_equal(mean(m.tess3r$G - m.tess3RImplementation$G), 0)

})

test_that("computeXBin", {

  skip_if_not_installed("tess3r")

  ## data
  s <- Tess3Sampler(n = 100,
                    nsites.neutral = 10000,
                    nsites.selected = 0,
                    min.maf = 0.05)
  dat <- sampl(s)
  n <- nrow(dat$G)
  L <- ncol(dat$G)
  ploidy <- computePloidy(dat$G)

  XBin.tess3r <- matrix(0, n, L * (ploidy + 1))
  tess3r::X2XBin(dat$G, ploidy, XBin.tess3r)

  XBin.tess3R <- computeXBin(dat$G, ploidy)

  expect_equal(mean(as.numeric(XBin.tess3R) - as.numeric(XBin.tess3r)), 0)
})
