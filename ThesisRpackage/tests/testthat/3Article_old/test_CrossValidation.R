library(testthat)
library(Article3Package)
context("Cross Validation")

################################################################################
# gausian model

test_that("k fold cross validation on normal dataset on indiv", {

  K = 3
  L = 1000
  n = 100
  # sample data
  s <- NormalSampler(n = n,
                     L = L,
                     K = K,
                     prop.outlier = 0.01,
                     sigma = 0.2,
                     c = 0.5,
                     mean.B = 0.0,
                     sd.mu = 1.0,
                     mean.mu = 0.5)
  dat <- sampl(s)

  # lambda = 1
  m <-  RidgeLFMMMethod(K = K,
                    lambda = 1e1)
  m <- fit(m, dat)
  s1 <- loss(m)
  e1 <- prediction_error(m, dat)
  expect_equal(sqrt(s1), e1)

  # lambda = 1e-5
  m <- clean(m)
  m <-  RidgeLFMMMethod(K = K,
                        lambda = 1e-5)
  m <- fit(m, dat)
  s2 <- loss(m)
  e2 <- prediction_error(m, dat)
  expect_equal(sqrt(s2), e2)

  expect_lte(abs(s1 - s2), 1e-3)


  # test cross validation plot
  crossvalid.res <- crossvalidation_kfold_indiv(m, dat,
                                                kfold = 4,
                                                lambdas = c(1e-5,1e0, 1e2,1e3, 1e4, 1e5, 1e6, 1e34))
  plot(crossvalid.res)
  plot(crossvalid.res, "sumUXabscor")


})

test_that("k fold cross validation on normal dataset on locus", {

  K = 4
  L = 1000
  n = 100
  # sample data
  s <- NormalSampler(n = n,
                     L = L,
                     K = K,
                     prop.outlier = 0.01,
                     sigma = 0.2,
                     c = 0.5,
                     mean.B = 0.0,
                     sd.mu = 1.0,
                     mean.mu = 0.5)
  dat <- sampl(s)

  # lambda = 1
  m <-  RidgeLFMMMethod(K = K,
                        lambda = 1e1)

  # test of errAssumingU
  m.res <- fit(m, dat)
  err <- errAssumingU(dat$G, m.res$U)

  # test cross validation plot
  crossvalid.res <- crossvalidation_kfold_locus(m, dat,
                                                kfold = 4,
                                                lambdas = c(1e-10, 1e0, 1e2, 1e40))
  plot(crossvalid.res, "err.assuming.UX")


})

test_that("imputation_error", {

  K = 4
  L = 1000
  n = 100
  # sample data
  s <- NormalSampler(n = n,
                     L = L,
                     K = K,
                     prop.outlier = 0.01,
                     sigma = 0.2,
                     c = 0.5,
                     mean.B = 0.0,
                     sd.mu = 1.0,
                     mean.mu = 0.5)
  dat <- sampl(s)
  missing.prop <- 0.05
  missing <- sample.int(n * L, n * L * missing.prop)
  true.values <- dat$G[missing]
  dat$G[missing] <- NA
  mean(is.na(dat$G))

  # lambda = 1
  m <-  finalLfmmRdigeMethod(K = K,
                             lambda = 1e10)

  # test of errAssumingU
  m.res <- fit(m, dat)
  dat$G[missing] <- true.values
  err <- imputation_error(m.res, dat)
  err
  expect_true(is.numeric(err))

})

test_that("cross validation with missing values on normal dataset ", {


  L = 1000
  n = 100
  cs <- c(0.6, 0.3, 0.0)
  K <- 3
  s <- NormalSampler2(n = 100,
                      L = 1000,
                      K = K,
                      prop.outlier = 0.2,
                      cs = cs)

  dat <- sampl(s)
  m <-  finalLfmmRdigeMethod(K = K,
                             lambda = NULL)

  # test cross validation plot
  crossvalid.res <- crossvalidation_kfold_missingvalue(m, dat,
                                                       rep = 5, missing.prop = 0.1,
                                                       lambdas = c(1e-10, 1e0, 1e2, 1e40))
  plot(crossvalid.res, "imputation_error")


})

test_that("cross validation with missing values on logistic dataset ", {


  K = 3
  L = 1000
  n = 100
  # sample data
  s <- LogisticSampler(n = n,
                     L = L,
                     K = K,
                     prop.outlier = 0.2,
                     c = 0.7)
  dat <- sampl(s)
  m <-  finalLfmmRdigeMethod(K = K,
                             lambda = NULL)

  # test cross validation plot
  crossvalid.res <- crossvalidation_kfold_missingvalue(m, dat,
                                                       rep = 5, missing.prop = 0.2,
                                                       lambdas = c(1e-10, 1e0, 1e2, 1e40))
  plot(crossvalid.res, "imputation_error")


})



################################################################################
# on dataset generated from 1000 genome

test_that("k fold cross validation on dataset generated from 1000genome", {

  G.file <- "~/Projects/Data2016_2017/1000Genomes/Phase3Chrm22/European_Chrm22.rds"
  skip_if_not(file.exists(G.file))


  K = 4
  L = 5000
  n = NULL
  cs <- list()
  cs[[3]] <- 0.6
  # sample data
  s <-  FromTrueSampler(G.file = G.file,
                        n = n,
                        L = L,
                        K = K,
                        prop.outlier = 0.30,
                        rho = NULL,
                        cs = cs,
                        round = FALSE)
  dat <- sampl(s)
  m <-  RidgeLFMMMethod(K = K + 1,
                        lambda = NULL)

  # test cross validation plot
  crossvalid.res <- crossvalidation_kfold_indiv(m, dat, kfold = 5,
                                          lambdas = c(1e-5,1e0, 1e2,1e3, 1e4, 1e5, 1e6, 1e25))
  plot(crossvalid.res)
  plot(crossvalid.res, stat = "loss")
  plot(crossvalid.res, "sumUXabscor")

})


test_that("k fold cross validation on locus on dataset generated from 1000genome", {

  G.file <- "~/Projects/Data2016_2017/1000Genomes/Phase3Chrm22/European_Chrm22.rds"
  skip_if_not(file.exists(G.file))

  K = 4
  L = 1000
  n = NULL
  cs <- list()
  cs[[3]] <- 0.6
  # sample data
  s <-  FromTrueSampler(G.file = G.file,
                        n = n,
                        L = L,
                        K = K,
                        prop.outlier = 0.10,
                        rho = NULL,
                        cs = cs,
                        round = FALSE)
  dat <- sampl(s)
  m <-  RidgeLFMMMethod(K = K,
                        lambda = NULL)

  # test cross validation plot
  crossvalid.res <- crossvalidation_kfold_locus(m, dat,
                                                kfold = 4,
                                                lambdas = c(1e-10, 1e0, 1e2, 1e40))
  plot(crossvalid.res, "err.assuming.UX")

})


test_that("k fold cross validation on locus on dataset generated from 1000genome", {

  skip("too long, just run and see the plot")

  G.file <- "~/Projects/Data2016_2017/1000Genomes/Phase3Chrm22/European_Chrm22.rds"
  skip_if_not(file.exists(G.file))

  K = 4
  L = 1000
  n = NULL
  cs <- list()
  cs[[3]] <- 0.6
  # sample data
  s <-  FromTrueSampler(G.file = G.file,
                        n = n,
                        L = L,
                        K = K,
                        prop.outlier = 0.10,
                        rho = NULL,
                        cs = cs,
                        round = FALSE)
  dat <- sampl(s)
  m <-  finalLfmmRdigeMethod(K = K,
                             lambda = NULL)

  # test cross validation plot
  crossvalid.res <- crossvalidation_kfold_missingvalue(m, dat,
                                                       rep = 5, missing.prop = 0.1,
                                                       lambdas = c(1e-10, 1e0, 1e2, 1e40))
  plot(crossvalid.res, "imputation_error")

})
