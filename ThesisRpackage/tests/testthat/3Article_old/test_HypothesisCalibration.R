library(testthat)
library(Article3Package)
context("Hypothesis calibration")

################################################################################
# gausian model

test_that("linear ridge lfmm + calculated B.sigma2 + gaussian model", {

  skip_if_not("experiment here, run it manualy")

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


  m <-  NormalZscore() %>%
    Zscore(zscorepvalue.functor = .,
           B.sigma2.functor = AnalyticSigma2Functor()) %>%
    RidgeLFMMMethod(K = K,
                    lambda = 10, # to add numerical stability
                    hypothesis.testing.method = .)

  testCalibration(m, s)

})



test_that("linear ridge lfmm + calculated B.sigma2 + gif + gaussian model", {

  skip_if_not("experiment here, run it manualy")

  K = 5
  L = 1000
  n = 100
  # sample data
  s <- NormalSampler(n = n,
                     L = L,
                     K = K,
                     prop.outlier = 0.2,
                     sigma = 0.2,
                     c = 0.5,
                     mean.B = 0.0,
                     sd.mu = 1.0,
                     mean.mu = 0.5)
  dat <- sampl(s)


  m <-  GifCalibratedZscore() %>%
    Zscore(zscorepvalue.functor = .,
           B.sigma2.functor = AnalyticSigma2Functor()) %>%
    RidgeLFMMMethod(K = K ,
                    lambda = 10, # to add numerical stability
                    hypothesis.testing.method = .)

  testCalibration(m, s)

})


test_that("linear ridge lfmm + bootstrap + gif + gaussian model", {

  skip_if_not("experiment here, run it manualy")

  K = 2
  L = 1000
  n = 100
  # sample data
  s <- NormalSampler(n = n,
                     L = L,
                     K = K,
                     prop.outlier = 0.1,
                     sigma = 0.2,
                     c = 0.5,
                     mean.B = 0.0,
                     sd.mu = 1.0,
                     mean.mu = 0.5)
  dat <- sampl(s)


  m <- Bootstrap( PairedBoostrap,
                   50) %>%
    Zscore(zscorepvalue.functor = GifCalibratedZscore(),
           B.sigma2.functor = .) %>%
    RidgeLFMMMethod(K = K ,
                    lambda = 10, # to add numerical stability
                    hypothesis.testing.method = .)

  testCalibration(m, s)

})



test_that("linear ridge lfmm + bootstrap + gaussian model", {

  skip_if_not("experiment here, run it manualy")

  K = 5
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


  m <-  Bootstrap( PairedBoostrap,
                   50) %>%
    Zscore(zscorepvalue.functor = NormalZscore(),
           B.sigma2.functor = .) %>%
    RidgeLFMMMethod(K = K,
                    lambda = 10, # to add numerical stability
                    hypothesis.testing.method = .,
                    reuse.V = FALSE)

  testCalibration(m, s)

})


test_that("linear ridge lfmm + bootstrap + gaussian model with reuse of V", {

  skip_if_not("experiment here, run it manualy")

  K = 3
  L = 1000
  n = 100
  # sample data
  s <- NormalSampler(n = n,
                     L = L,
                     K = K,
                     prop.outlier = 0.1,
                     sigma = 0.2,
                     c = 0.5,
                     mean.B = 0.0,
                     sd.mu = 1.0,
                     mean.mu = 0.5)
  dat <- sampl(s)


  m <-  Bootstrap( PairedBoostrap,
                   100) %>%
  Zscore(zscorepvalue.functor = NormalZscore(),
         B.sigma2.functor = .) %>%
  RidgeLFMMMethod(K = K,
                    lambda = 10, # to add numerical stability
                    hypothesis.testing.method = .,
                    reuse.V = TRUE)

  testCalibration(m, s)

})


test_that("linear ridge lfmm + permutation + gaussian model with reuse of V", {

  skip_if_not("experiment here, run it manualy")

  K = 3
  L = 1000
  n = 100
  # sample data
  s <- NormalSampler(n = n,
                     L = L,
                     K = K,
                     prop.outlier = 0.1,
                     sigma = 0.2,
                     c = 0.5,
                     mean.B = 0.0,
                     sd.mu = 1.0,
                     mean.mu = 0.5)
  dat <- sampl(s)


  m <-  PermutationZscore(100) %>%
    RidgeLFMMMethod(K = K,
                    lambda = 10, # to add numerical stability
                    hypothesis.testing.method = .,
                    reuse.V = TRUE)

  testCalibration(m, s)

})

################################################################################
# Binomial model




test_that("linear ridge lfmm + calculated B.sigma2 + binomial model", {

  skip_if_not("experiment here, run it manualy")

  K = 3
  L = 1000
  n = 100
  # sample data
  s <- LogisticSampler(n = n,
                       L = L,
                       K = K,
                       prop.outlier = 0.01,
                       sigma = 0.2,
                       c = 0.5,
                       mean.B = 0.0,
                       sd.mu = 1.0,
                       mean.mu = 0.5)
  dat <- sampl(s)


  m <-  NormalZscore() %>%
    Zscore(zscorepvalue.functor = .,
           B.sigma2.functor = AnalyticSigma2Functor()) %>%
    RidgeLFMMMethod(K = K,
                    lambda = 10, # to add numerical stability
                    hypothesis.testing.method = .)

  testCalibration(m, s)

})





