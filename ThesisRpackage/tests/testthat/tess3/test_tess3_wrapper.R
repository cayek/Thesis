library(testthat)
context("tess3_wrapper")

test_that("tess3Method", {
  s <- Tess3Sampler(n = 100,
                    nsites.neutral = 10000,
                    nsites.selected = 0,
                    min.maf = 0.05)
  dat <- sampl(s)

  m <- tess3Method(K = 2,
                   lambda = 1e1)
  m <- fit(m, dat)

  expect_equal(class(m)[1], "tess3Method")

  plot(m, dat)

})

test_that("sNMFMethod", {
  s <- Tess3Sampler(n = 100,
                    nsites.neutral = 10000,
                    nsites.selected = 0,
                    min.maf = 0.05)
  dat <- sampl(s)

  m <- sNMFMethod(K = 2)
  expect_equal(class(m)[1], "sNMFMethod")
  m <- fit(m, dat)

  plot(m, dat)

})

test_that("tess3Method with selected loci", {

  skip("Too long")
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

  m <- tess3Method(K = 2,
                   lambda = 1e1)
  m <- fit(m, dat)

  plot(m, dat)

})

test_that("tess3Method with fig2 param", {

  skip("Too long")
  s <- Tess3Sampler(n = 500,
                    nsites.neutral = 1.5 * 1e4,
                    nsites.selected = 0,
                    crossover.proba = 0.25 * 1e-8,
                    m.neutral = 0.25 * 0.05 * 1e-6,
                    m.selected = NULL,
                    mutation.rate.per.site = 0.25 * 1e-7,
                    N0 = 1e6,
                    k = 0.5,
                    min.maf = 0.05)
  dat <- sampl(s)

  m <- tess3Method(K = 2,
                   lambda = 1e0)
  m <- fit(m, dat)

  plot(m, dat)

})

