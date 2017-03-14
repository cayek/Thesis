library(testthat)
context("CrossValidation plot")

test_that("plot.CrossValidation with NormalSampler2", {
  ## dat
  s <- NormalSampler2(n = 100,
                      L = 1000,
                      K = 3,
                      prop.outlier = 0.10)
  dat <- sampl(s)

  ## method
  m <- finalLfmmRdigeMethod(K = 3, lambda = 1e-1)

  ## crossvalid lambda
  cv <- CrossValidation_rowwise(m, dat,
                                row.left.out.func = left.out.kfold(5),
                                col.left.out.func = left.out.kfold(20),
                                lambdas = c(1e-10, 1e-2, 1e2, 1e20),
                                Ks = c(3))
  plot(cv)

  ## crossvalid K
  cv <- CrossValidation_rowwise(m, dat,
                                row.left.out.func = left.out.kfold(10),
                                col.left.out.func = left.out.sample(10, 0.1),
                                lambdas = c(1e-10),
                                Ks = c(2, 3, 4, 5))
  plot(cv, color = "K")

  ## crossvalid both
  cv <- CrossValidation_rowwise(m, dat,
                                row.left.out.func = left.out.kfold(5),
                                col.left.out.func = left.out.sample(5, 0.1),
                                lambdas = c(1e-10, 1e-2,1e2, 1e20),
                                Ks = c(3, 4, 5))
  plot(cv)

})

test_that("plot.CrossValidation with LogisticSampler", {
  ## dat
  s <- LogisticSampler(n = 100,
                      L = 1000,
                      K = 3,
                      c = 0.8,
                      prop.outlier = 0.10)
  dat <- sampl(s)

  ## method
  m <- finalLfmmRdigeMethod(K = 3, lambda = 1e-1)

  ## crossvalid lambda
  cv <- CrossValidation_rowwise(m, dat,
                                row.left.out.func = left.out.kfold(5),
                                col.left.out.func = left.out.kfold(20),
                                lambdas = c(1e-10, 1e-2,1e2,1e4, 1e20, 1e40),
                                Ks = c(4))
  plot(cv)

  ## crossvalid K
  cv <- CrossValidation_rowwise(m, dat,
                                row.left.out.func = left.out.kfold(10),
                                col.left.out.func = left.out.sample(10, 0.1),
                                lambdas = c(1e-2),
                                Ks = c(2, 3, 4, 5))
  plot(cv, color = "K")

})

