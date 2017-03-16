library(testthat)
context("CrossValidation rowwise")

test_that("CrossValidation_rowwise", {

  # dat
  s <- NormalSampler2(n = 100,
                      L = 1000,
                      K = 3)
  dat <- sampl(s)

  # method
  m <- finalLfmmRdigeMethod(K = 3, lambda = 1e-1)

  # crossvalid lambda
  cv <- CrossValidation_rowwise(m, dat,
                                row.left.out.func = left.out.kfold(5),
                                col.left.out.func = left.out.sample(5, 0.2),
                                lambdas = c(1e-10, 1e0, 1e10),
                                Ks = 3)
  expect_equal(dim(cv$errs), c(3 * 5, 4))

  # crossvalid K
  cv <- CrossValidation_rowwise(m, dat,
                                row.left.out.func = left.out.kfold(5),
                                col.left.out.func = left.out.sample(5, 0.2),
                                lambdas = c(1e-10),
                                Ks = c(1,2,3,4))
  expect_equal(dim(cv$errs), c(4 * 5, 4))

})

test_that("left.out.kfold", {
  folds <- left.out.kfold(5)(20)
  expect_equal(length(folds), 5) # kfolds folds
  expect_equal(sort(purrr::simplify(folds)), 1:20) # all indices
})

test_that("left.out.kfold", {
  folds <- left.out.sample(5, .2)(20)
  expect_equal(length(folds), 5) # kfolds folds
  expect_equal(mean(sapply(folds, length)), .2 * 20) # all indices
})
