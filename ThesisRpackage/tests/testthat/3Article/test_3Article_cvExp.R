library(testthat)
context("3Article_cvExp")

flog.threshold(2)

################################################################################
## with lfmmRidge

test_that("Article3_cvExp with missing with lfmmRidge", {

  s <- NormalSampler2(n = 100,
                      L = 1000,
                      K = 3, prop.outlier = 0.1) %>%
    SparseMissingValueSampler(missing.prop = 0.01, missing.prop.by.loci = 0.1)

  dat <- sampl(s)

  exp <- Article3_cvExp(dat = dat,
                        Ks = c(2,3,4,5),
                        lambdas = c(1e-5,1e0,1e5),
                        dat.name = "NormalSampler2+missing",
                        save = FALSE,
                        bypass = TRUE)
  expect_equal(exp$name, "Article3_cvExp")
  expect_equal(exp$description,
               "Article3_cvExp with dat.name=NormalSampler2+missing lambdas=1e-05|1|1e+05 Ks=2|3|4|5 m=RidgeLfmm ")

  plot(exp$cv, color = "K")
  plot(exp$cv, color = "lambda")
})

test_that("Article3_cvExp with lfmmRidge", {

  s <- NormalSampler2(n = 100,
                      L = 1000,
                      K = 3, prop.outlier = 0.1)
  dat <- sampl(s)

  exp <- Article3_cvExp(dat = dat,
                        Ks = c(2,3,4,5),
                        lambdas = c(1e-5,1e0,1e5),
                        dat.name = "NormalSampler2+missing",
                        save = FALSE,
                        bypass = TRUE)
  expect_equal(exp$name, "Article3_cvExp")
  expect_equal(exp$description,
               "Article3_cvExp with dat.name=NormalSampler2+missing lambdas=1e-05|1|1e+05 Ks=2|3|4|5 m=RidgeLfmm ")

  plot(exp$cv, color = "K")
  plot(exp$cv, color = "lambda")
})

################################################################################
## with lfmmLasso


test_that("Article3_cvExp with lfmmLasso", {

  skip("too long")

  s <- NormalSampler2(n = 100,
                      L = 1000,
                      K = 3, prop.outlier = 0.1)
  dat <- sampl(s)

  ## to find a lambda
  m <- finalLfmmLassoMethod(s$K, 0.1)
  m <- run(m, dat)

  exp <- Article3_cvExp(dat = dat,
                        Ks = c(2, 3, 4, 5),
                        lambdas = m$lambda,
                        dat.name = "NormalSampler2+missing",
                        m = m,
                        save = FALSE,
                        bypass = TRUE)
  expect_equal(exp$name, "Article3_cvExp")

  plot(exp$cv, color = "K") ## do not work because I must CV over Gamma not K !
  plot(exp$cv, color = "lambda")
})
