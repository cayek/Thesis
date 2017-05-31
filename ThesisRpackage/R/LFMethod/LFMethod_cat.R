################################################################################
# cate

#' SVA package wrapper
#'
#' @export
cateMethod <- function(K,
                       name = "cateMethod",
                       nickname = NULL) {

  TestRequiredPkg("cate")

  m <- Method(name, hypothesis.testing.method = NULL, nickname = nickname)
  class(m) <- c("cateMethod", class(m))
  m$K = K
  m
}

#' @export
fit.cateMethod <- function(m, dat, reuse = FALSE) {

  L <- ncol(dat$G)
  n <- nrow(dat$G)

  cate.results <- cate::cate.fit(X.primary = dat$X,
                                 X.nuis = dat$X[,-1],
                                 Y = dat$G,
                                 r = m$K,
                                 calibrate = FALSE)

  ## output
  m$B <- matrix(cate.results$beta, 1, L)
  m$U <- matrix(cate.results$Z, n, m$K)
  m$score <- matrix(cate.results$beta.t, 1, L)
  m$pvalue <- matrix(cate.results$beta.p.value, 1, L)
  m
}

#' @export
run.cateMethod <- function(m, dat) {
  m <- fit(m, dat)
}

#' @export
numLatentVarEstimation.cateMethod <- function(m, dat, bcv.plot = FALSE) {

  covar <- as.data.frame(dat$X)
  colnames(covar)[1] <- "V1"

  factor.num <- cate::est.confounder.num(~ V1 | . - V1 + 0,
                                         covar, dat$G,
                                         method = "bcv",
                                         bcv.plot = bcv.plot,
                                         rmax = 30, nRepeat = 20)

  factor.num$r
}
