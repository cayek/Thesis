library(testthat)
library(Article3Package)
context("sample missing value")

test_that("uniform missing value sampling", {

  K = 6
  L = 1000
  d = 2
  n = 100
  # sample data
  s <- LogisticSampler(n = n,
                       L = L,
                       K = K,
                       ploidy = 2,
                       prop.outlier = 0.02,
                       sigma = 0.2,
                       c = 0.6,
                       mean.B = 0.0,
                       sd.mu = 1.0,
                       mean.mu = 0.5) %>%
    RandomXSampler(d = d) %>%
    MissingValueSampler(missing.prop = 0.1)
  dat <- sampl(s)

  expect_equal(mean(is.na(dat$G)), 0.1)

})


test_that("uniform missing value sampling", {

  K = 6
  L = 1000
  d = 2
  n = 100
  # sample data
  s <- LogisticSampler(n = n,
                       L = L,
                       K = K,
                       ploidy = 2,
                       prop.outlier = 0.02,
                       sigma = 0.2,
                       c = 0.6,
                       mean.B = 0.0,
                       sd.mu = 1.0,
                       mean.mu = 0.5) %>%
    RandomXSampler(d = d) %>%
    SparseMissingValueSampler(missing.prop = 0.1,
                              missing.prop.by.loci = 0.5)
  dat <- sampl(s)

  expect_equal(mean(is.na(dat$G)), 0.1)
  expect_equal(mean(is.na(dat$G[,dat$missing.loci])), 0.5)
  expect_equal(mean(is.na(dat$G[,-dat$missing.loci])), 0.0)
})
