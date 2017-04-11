################################################################################
# LEA

#' LEA package wrapper
#'
#' @export
LeaLFMMMethod <- function(K,
                          hypothesis.testing.method = NULL, #useless we retrieve LEA output
                          iterations = 10000,
                          CPU = 1,
                          burnin = 5000,
                          name = "LeaLFMMMethod",
                          nickname = NULL,
                          verbose = TRUE) {

  TestRequiredPkg("LEA")

  m <- Method(name, hypothesis.testing.method, nickname = nickname)
  class(m) <- c("LeaLFMMMethod", class(m))
  m$K = K
  m$verbose = verbose
  m$CPU <- CPU
  m$iterations <- iterations
  m$burnin <- burnin
  m
}


#' @export
fit.LeaLFMMMethod <- function(m, dat, reuse = FALSE) {


  if (ncol(dat$X) > 1) {
    stop("only d = 1 allowed ;-)")
  }

  # moving to a tmp dir
  old.dir <- getwd()
  setwd(tempdir())

  if (anyNA(dat$G)) {
    flog.debug("fit.LeaLFMMMethod: Missing values detected")
    m$missing.index <- which(is.na(dat$G))
    m$imputed.values <- dat$G[m$missing.index]
  }

  # copy file on disk ...
  tmp <- tempfile(tmpdir = ".") # must be write in current dir...bug
  G.file <- paste0(tmp,".lfmm")
  dat$G[is.na(dat$G)] <- 9
  LEA::write.lfmm(dat$G, G.file)
  X.file <- paste0(tmp,".env")
  LEA::write.env(dat$X, X.file)

  # run lfmm
  out <- capture.output(lfmm.res <- LEA::lfmm(input.file = G.file,
                                              K = m$K,
                                              iterations = m$iterations,
                                              burnin = m$burnin,
                                              CPU = m$CPU,
                                              environment.file = X.file,
                                              project = "new"))
  # restrieve results
  m$score <- t(LEA::z.scores(lfmm.res, K = m$K, d = 1))
  m$pvalue <- t(LEA::p.values(lfmm.res, K = m$K, d = 1))

  # restore dir...
  setwd(old.dir)

  DebugMessage("LEA", out)

  m
}

#' @export
run.LeaLFMMMethod <- function(m, dat) {

  m <- fit(m, dat)
}

