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
  n <- ncol(dat$G)

  cate.results <- cate::cate.fit(X.primary = dat$X,
                                 X.nuis = dat$X[,-1],
                                 Y = dat$G,
                                 r = m$K,
                                 calibrate = FALSE)

  ## output
  m$B <- matrix(cate.results$beta, 1, L)
  m$score <- matrix(cate.results$beta.t, 1, L)
  m$pvalue <- matrix(cate.results$beta.p.value, 1, L)
  m
}

#' @export
run.cateMethod <- function(m, dat) {
  m <- fit(m, dat)
}

