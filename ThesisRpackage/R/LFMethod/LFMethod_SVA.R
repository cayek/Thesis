################################################################################
# SVA

#' SVA package wrapper
#'
#' @export
SVAMethod <- function(K,
                      hypothesis.testing.method = NULL, #useless we retrieve sva output
                      name = "SVAMethod",
                      nickname = NULL) {

  TestRequiredPkg("sva")

  m <- Method(name, hypothesis.testing.method, nickname = nickname)
  class(m) <- c("SVAMethod", class(m))
  m$K = K
  m
}

#' @export
fit.SVAMethod <- function(m, dat, reuse = FALSE) {


  if (ncol(dat$X) > 1) {
    stop("only d = 1 allowed ;-)")
  }

  # create SVA data
  n <- nrow(dat$G)
  L <- ncol(dat$G)
  edata <- t(dat$G)
  one <- matrix(1, n, 1)
  mod <- cbind(one, dat$X)
  mod0 <- one # other co-variable, here only intercept

  # We confounding structure
  # n.sv <- num.sv(edata,mod,method = "leek")
  n.sv <- m$K# K is given
  svobj <- sva::sva(edata,mod,mod0,n.sv = n.sv)

  # We perform association with confounding correction
  modSv <- cbind(mod,svobj$sv)
  mod0Sv <- cbind(mod0,svobj$sv)
  fstatSv <- sva::fstats(edata,modSv,mod0Sv)
  pValuesSv <- sva::f.pvalue(edata,modSv,mod0Sv)


  # output
  m$score <- matrix(fstatSv, nrow = 1, ncol = L)
  m$pvalue <- matrix(pValuesSv, nrow = 1, ncol = L)
  m
}

#' @export
run.SVAMethod <- function(m, dat) {

  m <- fit(m, dat)
}

