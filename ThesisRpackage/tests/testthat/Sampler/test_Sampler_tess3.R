library(testthat)
context("test_Sampler")

test_that("Tess3Sampler", {

  s <- Tess3Sampler(n = 100,
                    nsites.neutral = 1000,
                    min.maf = 0.0)
  dat <- sampl(s)
  dim(dat$G)

  plot(dat$coord, col = rep(1:2, each = 50))

})

