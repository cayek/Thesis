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

  return(m.res)
}

test_that("lfmm lasso", {
  m <- finalLfmmLassoMethod(3, 0.1)
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

test_that("cate", {
  skip_if_not_installed("cate")
  m <- finalcateMethod(1)
  test(m, "cate", "finalcateMethod")
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
  m <- finalOracle(4)
  m <- test(m, "Oracle", "finalOracle")
  expect_equal(dim(m$U), c(10, 3))
  expect_equal(dim(m$V), c(100, 3))
  
  m <- finalOracle(2)
  m <- test(m, "Oracle", "finalOracle")
  expect_equal(dim(m$U), c(10, 3))
  expect_equal(dim(m$V), c(100, 3))


})

test_that("clumped data") {

  K <- 3
  n <- 100
  L <- 1000
  s <- TrueSampler(G.file = matrix(rnorm(n  * L), n, L),
                   X.file = matrix(rnorm(n), n, 1),
                   outlier.file = 1:10,
                   ind.clumping = sample.int(L, 0.5 * L))
  dat <- sampl(s)

  ms <- finalBench(K = K,
                   lambda = 1e-5,
                   calibrate = FALSE,
                   sparse.prop = 0.1,
                   fast.only = FALSE)
  ms$oracle <- NULL

  ## test with 1
  m <- run(ms$L, dat)

  ## test with all
  expr <- MethodBatchExperiment("test",
                                s = s,
                                method.batch = ms)

  expr <- runExperiment(expr)

  m <- expr$method.batch[[1]] ## to test
  for (m in expr$method.batch) {
    if(!is.null(ms$U)) {
      expect_equal(dim(ms$V), c(n, K))
    }
    if(!is.null(ms$V)) {
      expect_equal(dim(ms$V), c(0.5 * L, K))
    }
    if(!is.null(ms$B)) {
      expect_equal(dim(ms$B), c(1, L))
    }
    if(!is.null(ms$pvalue)) {
      expect_equal(dim(ms$pvalue), c(1, L))
    }
    
  }

})
