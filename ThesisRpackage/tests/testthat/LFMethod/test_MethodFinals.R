library(testthat)
context("finals method")

test <- function(m, nickname, classname) {

  expect_equal(m$nickname, nickname)
  expect_true(classname %in% class(m))

  # without missing value
  K <- 3
  s <- NormalSampler(n = 10,
                     L = 100,
                     K = K,
                     prop.outlier = 0.02,
                     sigma = 0.2,
                     c = 0.6,
                     mean.B = 0.0,
                     sd.mu = 1.0,
                     mean.mu = 0.5)
  dat <- sampl(s)
  fit(m, dat)
  run(m, dat)

  # with missing values
  K <- 3
  s <- NormalSampler(n = 10,
                     L = 100,
                     K = K,
                     prop.outlier = 0.02,
                     sigma = 0.2,
                     c = 0.6,
                     mean.B = 0.0,
                     sd.mu = 1.0,
                     mean.mu = 0.5) %>%
    MissingValueSampler(missing.prop = 0.1)

  dat <- sampl(s)
  missing.index <- which(is.na(dat$G))
  fit(m, dat)
  m.res <- run(m, dat)

  expect_equal(missing.index, m.res$missing.index)
  expect_equal(length(m.res$imputed.values), length(missing.index))

  return(NULL)
}

test_that("lfmm lasso", {
  m <- finalLfmmLassoMethod(1, 1)
  test(m, "LassoLfmm", "finalLfmmLassoMethod")
})

test_that("lfmm ridge", {
  m <- finalLfmmRdigeMethod(1, 1)
  test(m, "RidgeLfmm", "finalLfmmRdigeMethod")
})

test_that("famt", {
  m <- finalFamtMethod(1)
  test(m, "FAMT", "finalFamtMethod")
})

test_that("sva", {
  skip_if_not_installed("sva")
  m <- finalSVAMethod(1)
  test(m, "SVA", "finalSVAMethod")
})

test_that("refactor", {
  m <- finalRefactorMethod(K = 1)
  test(m, "Refactor", "finalRefactorMethod")
})

test_that("LEA", {
  m <- finalLEAMethod(1)
  test(m, "LEA", "finalLEAMethod")
})

test_that("lm", {
  m <- finalLm()
  test(m, "lm", "finalLm")
})

test_that("PCA+lm", {
  m <- finalPcaLm(1)
  test(m, "PcaLm", "finalPcaLm")
})

test_that("Oracle", {
  m <- finalOracle(3)
  test(m, "Oracle", "finalOracle")
})

