library(testthat)
context("Predict_row")

test_that("Predict_row.RidgeLFMMMethod", {

  # dat
  n <- 100
  L <- 1000
  s <- NormalSampler2(n = n,
                      L = L,
                      K = 3)
  dat <- sampl(s)
  # method
  m <- finalLfmmRdigeMethod(K = 3, lambda = 1e-1)
  m <- fit(m, dat)

  # predict
  ## diff J
  is <- sample.int(n, 0.1 * n)
  G_i <- dat$G[is, ]
  X_i <- dat$X[is,, drop  = FALSE]
  j.assumed <- sample.int(L, 0.9 * L)
  j.unknow <- (1:L)[-j.assumed]
  expect_equal(mean(j.unknow %in% j.assumed), 0)
  G_i_pred <- Predict_row(m, X_i, G_i, j.assumed, j.unknow)
  err <- mean((G_i_pred - G_i[,j.unknow]) ^2)
  expect_equal(dim(G_i_pred), c(length(is), length(j.unknow)))


  ## same J
  is <- sample.int(n, 0.1 * n)
  G_i <- dat$G[is, ]
  X_i <- dat$X[is,, drop  = FALSE]
  j.assumed <- 1:L
  j.unknow <- 1:L
  G_i_pred <- Predict_row(m, X_i, G_i, j.assumed, j.unknow)
  err <- mean((G_i_pred - G_i[,j.unknow]) ^2)
  expect_equal(dim(G_i_pred), c(length(is), length(j.unknow)))

})
