library(testthat)
context("Hypothesis Testing glm")

flog.threshold(2)

test_that("phenotypeWayReg_glm_score", {

  ## dat
  s <- NormalSampler2(n = 100,
                      L = 1000,
                      K = 3)
  dat <- sampl(s)

  ## LFmethod
  m <- finalLfmmRdigeMethod(K = 3, lambda = 1e-2)
  m <-  fit(m, dat)

  ## hypothesis testing calibrate=FALSE
  ht.func <- phenotypeWayReg_glm_score(calibrate = FALSE,
                                       family = gaussian)
  glm.res <- ht.func$fun(m, dat)

  ind <- seq_along(glm.res$score)
  qplot(x = ind,
        y = -log10(glm.res$pvalue[1,]),
        color = ind %in% dat$outlier)
  
  ## hypothesis testing calibrate=TRUE
  ht.func <- phenotypeWayReg_glm_score(calibrate = TRUE,
                                       family = gaussian)
  glm.res <- ht.func$fun(m, dat)

  ind <- seq_along(glm.res$score)
  qplot(x = ind,
        y = -log10(glm.res$pvalue[1,]),
        color = ind %in% dat$outlier)

})

test_that("phenotypeWayReg_lm_score", {

  ## dat
  s <- NormalSampler2(n = 100,
                      L = 1000,
                      K = 3)
  dat <- sampl(s)

  ## LFmethod
  m <- finalLfmmRdigeMethod(K = 3, lambda = 1e-2)
  m <-  fit(m, dat)

  ## hypothesis testing calibrate=FALSE
  ht.func <- phenotypeWayReg_lm_score(calibrate = FALSE)
  lm.res <- ht.func$fun(m, dat)

  ind <- seq_along(lm.res$score)
  qplot(x = ind,
        y = -log10(lm.res$pvalue[1,]),
        color = ind %in% dat$outlier)

  ## hypothesis testing calibrate=TRUE
  ht.func <- phenotypeWayReg_lm_score(calibrate = TRUE)
  lm.res <- ht.func$fun(m, dat)

  ind <- seq_along(lm.res$score)
  qplot(x = ind,
        y = -log10(lm.res$pvalue[1,]),
        color = ind %in% dat$outlier)

})

test_that("comparison of phenotypeWayReg_lm_score and phenotypeWayReg_glm_score", {

  ## dat
  s <- NormalSampler2(n = 100,
                      L = 1000,
                      K = 3)
  dat <- sampl(s)

  ## LFmethod
  m <- finalLfmmRdigeMethod(K = 3, lambda = 1e-2)
  m <-  fit(m, dat)

  ## hypothesis testing calibrate=FALSE
  ht.func.glm <- phenotypeWayReg_glm_score(calibrate = FALSE,
                                           family = gaussian)
  glm.res <- ht.func.glm$fun(m, dat)

  ht.func.lm <- phenotypeWayReg_lm_score(calibrate = FALSE)
  lm.res <- ht.func.lm$fun(m, dat)

  ## comparison of regression coefficients
  expect_lt(sqrt(mean((lm.res$B - glm.res$B) ^ 2)), 1e-14)
  
  ## comparison of score
  expect_lt(sqrt(mean((lm.res$score - glm.res$score) ^ 2)), 1e-13)
  
  ## comparison of pvalue
  expect_lt(sqrt(mean((lm.res$pvalue - glm.res$pvalue) ^ 2)), 1e-13)

})

test_that("phenotypeWayReg_lm", {

  ## dat
  s <- NormalSampler2(n = 100,
                      L = 1000,
                      K = 3)
  dat <- sampl(s)

  ## LFmethod
  m <- finalLfmmRdigeMethod(K = 3, lambda = 1e-2)
  m <-  fit(m, dat)

  CoVar <- cbind(matrix(1,nrow(dat$G),1), dat$U)
  lm.res <- phenotypeWayReg_lm(Y = dat$X,X = dat$G, CoVar = CoVar)

  ind <- seq_along(lm.res$B)
  qplot(x = ind,
        y = as.numeric(lm.res$B),
        color = ind %in% dat$outlier)
  qplot(x = ind,
        y = as.numeric(lm.res$B.sigma2),
        color = ind %in% dat$outlier)

})
