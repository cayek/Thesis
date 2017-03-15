library(testthat)
context("3Article_cvExp")

test_that("Article3_cvExp with missing", {

  s <- NormalSampler2(n = 100,
                      L = 1000,
                      K = 3, prop.outlier = 0.1) %>%
    SparseMissingValueSampler(missing.prop = 0.01, missing.prop.by.loci = 0.1)

  exp <- Article3_cvExp(s = s,
                        Ks = c(2,3,4,5),
                        lambdas = c(1e-5,1e0,1e5),
                        s.name = "NormalSampler2+missing",
                        save = FALSE,
                        bypass = TRUE)
  expect_equal(exp$name, "Article3_cvExp")
  expect_equal(exp$description,
               "Article3_cvExp with s.name=NormalSampler2+missing lambdas=1e-05|1|1e+05 Ks=2|3|4|5 ")

  plot(exp$cv, color = "K")
  plot(exp$cv, color = "lambda")
})

test_that("Article3_cvExp", {

  s <- NormalSampler2(n = 100,
                      L = 1000,
                      K = 3, prop.outlier = 0.1)

  exp <- Article3_cvExp(s = s,
                        Ks = c(2,3,4,5),
                        lambdas = c(1e-5,1e0,1e5),
                        s.name = "NormalSampler2+missing",
                        save = FALSE,
                        bypass = TRUE)
  expect_equal(exp$name, "Article3_cvExp")
  expect_equal(exp$description,
               "Article3_cvExp with s.name=NormalSampler2+missing lambdas=1e-05|1|1e+05 Ks=2|3|4|5 ")

  plot(exp$cv, color = "K")
  plot(exp$cv, color = "lambda")
})
