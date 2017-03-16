library(testthat)
context("test_Sampler")

test_that("Tess3Sampler", {
  options(tess3.ms = "~/BiocompSoftware/msdir/ms")
  s <- Tess3Sampler(n = 100,
                    nsites.neutral = 1000,
                    min.maf = 0.0)
  dat <- sampl(s)
  dim(dat$G)

  plot(dat$coord, col = rep(1:2, each = 50))

})

test_that("Tess3Sampler with selected loci", {

  skip("Too long")
  options(tess3.ms = "~/BiocompSoftware/msdir/ms")
  s <- Tess3Sampler(n = 100,
                    nsites.neutral = 1 * 1e5,
                    nsites.selected = 1 * 1e2,
                    crossover.proba = 0.25 * 1e-8,
                    m.neutral = 0.25 * 10 * 1e-6,
                    m.selected = 0.25 * 0.1 * 1e-6,
                    mutation.rate.per.site = 0.25 * 1e-7,
                    N0 = 1e6,
                    k = 0.5,
                    min.maf = 0.05)
  dat <- sampl(s)
  dim(dat$G)

  plot(dat$coord, col = rep(1:2, each = 50))

})


