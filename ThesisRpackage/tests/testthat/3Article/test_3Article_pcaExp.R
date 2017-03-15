library(testthat)
context("3Article_pcaExp")

test_that("Article3_pcaExp", {

  # dat
  s <- NormalSampler2(n = 100,
                      L = 1000,
                      K = 3) %>%
    SparseMissingValueSampler(missing.prop = 0.01, missing.prop.by.loci = 0.1)

  exp <- Article3_pcaExp(s = s,
                         s.name = "NormalSampler2+missing",
                         save = FALSE,
                         bypass = TRUE)
  expect_equal(exp$name, "Article3_pcaExp")
  expect_equal(exp$description, "Run PCA on dataset on NormalSampler2+missing")
  plot(exp)
})
