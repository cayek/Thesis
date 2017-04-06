library(testthat)
context("Hypothesis Testing glm")

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

