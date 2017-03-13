################################################################################
# helpers


################################################################################
# AlternatedSvdLFMMMethod

#' ||G  - U V^t - X B || + lambda ||B||^2
#'
#' @export
AlternatedSvdLFMMMethod <- function(K,
                                    it.max = 100,
                                    err.max = 1e-6,
                                    hypothesis.testing.method = Zscore(NormalZscore(),
                                                                       AnalyticSigma2Functor()),
                                    lambda = 0.0,
                                    center = TRUE,
                                    name = "AlternatedSvdLFMMMethod",
                                    nickname = NULL) {
  m <- Method(name, hypothesis.testing.method, nickname = nickname)
  class(m) <- c("AlternatedSvdLFMMMethod", class(m))
  m$center <- center
  m$K = K
  m$lambda = lambda
  m$it.max = it.max
  m$err.max = err.max
  m
}


#' @export
fit.AlternatedSvdLFMMMethod <- function(m, dat, reuse = FALSE) {

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

  ## update.func
  update.func <- function(m, G_, dat) {
    ## calculate C
    svd.res <- svd(G_ - dat$X %*% m$B, nu = m$K, nv = m$K)
    m$U <- svd.res$u %*% diag(svd.res$d[1:m$K], m$K, m$K)
    m$V <- svd.res$v
    m$C <- tcrossprod(m$U, m$V)
    m$C.nuclear.norm <- sum(svd.res$d[1:m$K])

    ## calculate B
    m$B <- B_ridge(A = G_ - m$C, X = dat$X, lambda = m$lambda)
    m
  }

  m <- missingValueImputationLoop(m = m,
                                  G_ = G_,
                                  update.func = update.func,
                                  dat = dat,
                                  reuse = reuse)

  # compute analytic variance
  m$epsilon.sigma2 <- epsilon.sigma2(m$epsilon,
                                     reduced.df = ifelse(m$center,1,0))


  # B.sigma2 B|V,X so we do not variability of the estimattion of V ... see perso lab notebook (2/12/2016)
  m$B.sigma2 <- B.sigma2(epsilon.sigma2 = m$epsilon.sigma2,
                         X = dat$X,
                         lambda = m$lambda)

  # return
  m
}

#' @export
run.AlternatedSvdLFMMMethod <- function(m, dat) {

  m <- fit(m, dat)
  run.Method(m, dat)
}
