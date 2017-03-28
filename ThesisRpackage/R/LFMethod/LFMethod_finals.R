################################################################################
# helpers

fit.final <- function(m, dat, reuse = FALSE) {
  ## impute
  if (anyNA(dat$G)) {
    DebugMessage("Missing values detected")
    m$missing.index <- which(is.na(dat$G))
    dat <- imputeMeanDataSet(dat)
    m$imputed.values <- dat$G[m$missing.index]
  }
  NextMethod()
}

################################################################################

#' @export
finalLfmmRdigeMethod <- function(K, lambda, calibrate  = FALSE, prior.impute = FALSE, nickname = NULL) {
  m <- RidgeLFMMMethod(K = K,
                       hypothesis.testing.method = lm_zscore(calibrate = calibrate,
                                                             sigma.computation = "lm",
                                                             correctionByC = FALSE),
                       lambda = lambda,
                       center = TRUE,
                       name = "RidgeLFMMMethod",
                       nickname = ifelse(!is.null(nickname),nickname,
                                         ifelse(prior.impute,"impute+RidgeLfmm","RidgeLfmm")),
                       reuse.V = FALSE)
  if (prior.impute) {
    class(m) <- c("final", "finalLfmmRdigeMethod", class(m))
  } else {
    class(m) <- c("finalLfmmRdigeMethod", class(m))
  }


  ## for iterative version
  m$it.max = 200
  m$err.max = 1e-6
  m
}

#' @export
fit.finalLfmmRdigeMethod <- function(m, dat, reuse = FALSE) {
  if (anyNA(dat$G)) {
    ## param
    n <- nrow(dat$G)
    L <- ncol(dat$G)
    d <- ncol(dat$X)

    G_ <- dat$G

    ## center
    if (m$center) {
      m$mu <- matrix(dat$G %>% purrr::array_branch(2) %>%
                       purrr::map_dbl(mean, na.rm = TRUE),
                     1, L)
      G_ <- G_ - matrix(1, n, 1) %*% m$mu
    } else {
      G_ <- G_
    }

    ## compute P
    if (!reuse) {
      In <- diag(1, n, n)
      D <- diag(1, d, d)
      P_X_lambda_t = In - dat$X %*% solve(crossprod(dat$X, dat$X) + m$lambda * D, t(dat$X))
      eigen.P_X_D_t = eigen(P_X_lambda_t, symmetric = TRUE)
      m$P = eigen.P_X_D_t$vectors %*% diag(sqrt(eigen.P_X_D_t$values)) %*% t(eigen.P_X_D_t$vectors)
    }

    ## update.func
    update.func <- function(m, G_, dat) {
      RidgeLFMMMethod_main(m, G_, dat, lambda = m$lambda, reuse = TRUE)
    }

    m <- missingValueImputationLoop(m = m,
                                    G_ = G_,
                                    update.func = update.func,
                                    dat = dat,
                                    reuse = reuse)
    # return
    m
  } else {
    NextMethod()
  }
}

################################################################################
finalLfmmLassoMethod <- function(K, sparse.prop, calibrate  = FALSE, nickname = NULL) {
  m <- LassoLFMMMethod(K = K,
                       it.max = 200,
                       err.max = 1e-6,
                       hypothesis.testing.method = lm_zscore(calibrate = calibrate,
                                                             sigma.computation = "lm",
                                                             correctionByC = FALSE),
                       sparse.prop = sparse.prop,
                       lambda = NULL, # if null regularization path
                       lambda.K = 100, # default value used in Friedman et al. 2010
                       lambda.eps = 0.001, # default value used in Friedman et al. 2010
                       center = TRUE,
                       name = "finalLfmmLassoMethod",
                       nickname = "LassoLfmm")

  class(m) <- c("final", "LassoLFMMMethod", class(m))
  m
}


################################################################################

#' @export
finalFamtMethod <- function(K) {
  m <- FAMTMethod(K = K,
             hypothesis.testing.method = NULL, #useless we retrieve FAMT output
             name = "FAMTMethod",
             nickname = "FAMT")
  class(m) <- c("final", "finalFamtMethod", class(m))
  m
}

################################################################################

#' @export
finalSVAMethod <- function(K) {
  m <- SVAMethod(K = K,
                  hypothesis.testing.method = NULL, #useless we retrieve SVA output
                  name = "SVAMethod",
                  nickname = "SVA")
  class(m) <- c("final", "finalSVAMethod", class(m))
  m
}

################################################################################

#' @export
finalRefactorMethod <- function(K, calibrate = FALSE, t = 500, verbose = FALSE) {
  m <- refractorMethod(K = K,
                       t = t,
                       hypothesis.testing.method = lm_zscore(calibrate = calibrate,
                                                             correctionByC = FALSE),
                       name = "refactor",
                       nickname = "Refactor",
                       verbose = verbose)
  class(m) <- c("final", "finalRefactorMethod", class(m))
  m
}

################################################################################

#' @export
finalLEAMethod <- function(K) {
  m <- LeaLFMMMethod(K = K,
                     hypothesis.testing.method = NULL, #useless we retrieve LEA output
                     name = "LeaLFMMMethod",
                     nickname = "LEA",
                     verbose = FALSE)
  class(m) <- c("finalLEAMethod", class(m))
  m
}

################################################################################

#' @export
finalLm <- function(calibrate = FALSE) {
  if (calibrate) {
    hypothesis.testing.method <- Zscore(zscorepvalue.functor = FdrtoolsCalibratedZscore() ,
                                        B.sigma2.functor = AnalyticSigma2Functor())
  } else {
    hypothesis.testing.method <- Zscore(zscorepvalue.functor = NormalZscore() ,
                                        B.sigma2.functor = AnalyticSigma2Functor())
  }

  m <- ClassicLinearMethod(center = TRUE,
                      hypothesis.testing.method = hypothesis.testing.method,
                      nickname = "lm")
  class(m) <- c("finalLm",  class(m))
  m
}

################################################################################

#' @export
finalPcaLm <- function(K, calibrate = FALSE) {
  if (calibrate) {
    hypothesis.testing.method <- Zscore(zscorepvalue.functor = FdrtoolsCalibratedZscore() ,
                                        B.sigma2.functor = AnalyticSigma2Functor())
  } else {
    hypothesis.testing.method <- Zscore(zscorepvalue.functor = NormalZscore() ,
                                        B.sigma2.functor = AnalyticSigma2Functor())
  }
  m <- PCAClassicLinearMethod(K = K,
                         nickname = "PcaLm",
                         hypothesis.testing.method = hypothesis.testing.method)
  class(m) <- c("final", "finalPcaLm", class(m))
  m
}

################################################################################

#' @export
finalOracle <- function(K, calibrate = FALSE) {
  m <- PCAClassicLinearMethod(K = K,
                              nickname = "Oracle",
                              hypothesis.testing.method = lm_zscore(calibrate = calibrate,
                                                                    correctionByC = FALSE),
                              assumingStructure = TRUE)
  class(m) <- c("final", "finalOracle", class(m))
  m
}

################################################################################

#' @export
finalBench <- function(K, lambda, calibrate, sparse.prop,
                       fast.only = FALSE, with.missing = FALSE) {
    bench <- list()
    bench$lfmmRidge <- finalLfmmRdigeMethod(K = K,
                                            lambda = lambda,
                                            calibrate = calibrate)

    bench$famt <- finalFamtMethod(K = K)
    bench$sva <- finalSVAMethod(K = K)
    bench$refactor <- finalRefactorMethod(K = K, calibrate = calibrate)
    bench$lm <- finalLm(calibrate = calibrate)
    bench$pcaLm <- finalPcaLm(K = K, calibrate = calibrate)
    bench$oracle <- finalOracle(K = K, calibrate = calibrate)
    if (!fast.only) {
      bench$lea <- finalLEAMethod(K = K)
      bench$lfmmLasso <- finalLfmmLassoMethod(K = K,
                                              sparse.prop = sparse.prop,
                                              calibrate = calibrate)
    }
    if (with.missing) {
      bench$lfmm.ridge.impute.first <- finalLfmmRdigeMethod(K = K,
                                                            lambda = lambda,
                                                            calibrate = calibrate,
                                                            prior.impute = TRUE)
    }
    bench
}
