################################################################################
# FAMT

#' FAMT package wrapper
#'
#' @export
FAMTMethod <- function(K,
                       hypothesis.testing.method = NULL, #useless we retrieve FAMT output
                       name = "FAMTMethod",
                       nickname = NULL) {

  TestRequiredPkg("FAMT")
  TestRequiredPkg("impute")

  m <- Method(name, hypothesis.testing.method, nickname = nickname)
  class(m) <- c("FAMTMethod", class(m))
  m$K = K
  m
}

#' @export
fit.FAMTMethod <- function(m, dat, reuse = FALSE) {

  # because of a bug in FAMT impute must be loaded...
  require("impute")

  if (ncol(dat$X) > 1) {
    stop("only d = 1 allowed ;-)")
  }

  # create FAMT data
  n <- nrow(dat$G)
  L <- ncol(dat$G)
  expresssion <- as.data.frame(t(dat$G))
  covariates <- data.frame(array.id = colnames(expresssion),
                           x = dat$X[,1])
  dat.FAMT = FAMT::as.FAMTdata(expression = expresssion,
                               covariates = covariates)

  # run model
  model <- FAMT::modelFAMT(dat.FAMT, x = 2, nbf = m$K)

  # output
  m$score <- matrix(model$adjtest, nrow = 1, ncol = L)
  m$pvalue <- matrix(model$adjpval, nrow = 1, ncol = L)
  m
}

#' @export
run.FAMTMethod <- function(m, dat) {

  fit(m, dat)
}

