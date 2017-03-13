################################################################################
# helpers

initUVB <- function(G_, X, K, beta) {
  res <- list()
  svd.res <- svd(G_, nu = K, nv = K)
  U = svd.res$u %*% diag(svd.res$d[1:K])
  V = svd.res$v
  C <- tcrossprod(U, V)
  B <- B_ridge(A = G_ - C, X = X, lambda = beta)

  res$U_X <- cbind(U, X)
  res$V_Bt <- cbind(V, t(B))
  res
}

################################################################################
# AlternatedLFMMMethod

#' ||G  - U V^t - X B || + alpha ||U||^2 + beta ||V B^t||^2
#'
#' @export
AlternatedLFMMMethod <- function(K,
                                 hypothesis.testing.method = Zscore(NormalZscore(),
                                                                    AnalyticSigma2Functor()),
                                 it.max = 100,
                                 err.max = 1e-6,
                                 alpha = 10,
                                 beta = 10,
                                 center = TRUE,
                                 name = "AlternatedLFMMMethod") {
  m <- Method(name, hypothesis.testing.method)
  class(m) <- c("AlternatedLFMMMethod", class(m))
  m$center <- center
  m$K <- K
  m$alpha <- alpha
  m$beta <- beta
  m$it.max <- it.max
  m$err.max <- err.max
  m
}


#' @export
fit.AlternatedLFMMMethod <- function(m, dat, reuse = FALSE) {

  # param
  n <- nrow(dat$G)
  L <- ncol(dat$G)
  d <- ncol(dat$X)
  one <- matrix(1, n, 1)

  # compute center
  if (m$center) {
    m$mu <- matrix(dat$G %>% purrr::array_branch(2) %>%
                     purrr::map_dbl(mean, na.rm = TRUE),
                   1, L)
    G_ <- dat$G - one %*% m$mu
  } else {
    G_ <- dat$G
  }

  # impute missing value
  if (anyNA(G_)) {
    DebugMessage("Missing values detected")
    G_ <- m$impute.genotype.method$fun(G_)
  }


  # init algo
  tmp <- list()
  if (!reuse) {
    tmp <- initUVB(G_, dat$X, m$K, m$beta)
  } else {
    tmp$U_X <- cbind(m$U, dat$X)
    tmp$V_Bt <- cbind(m$V, t(m$B))
  }
  it <- 0
  m$epsilon <- G_ - tcrossprod(tmp$U_X, tmp$V_Bt)
  err.new <- mean((m$epsilon) ^ 2)
  stop <- FALSE
  # main loop
  while ((it < m$it.max) && !stop) {

    DebugMessage(paste("it = ",it, "| err = ", err.new, "\n"))
    err.old <- err.new
    it <- it + 1

    ## calculate U
    tmp$U_X[,1:m$K] <- t(B_ridge(A = t(G_),
                                 X = tmp$V_Bt,
                                 lambda = m$alpha)[1:m$K,])

    ## calculate V_B
    tmp$V_Bt <- t(B_ridge(A = G_,
                          X = tmp$U_X,
                          lambda = m$beta))

    ## calculate epsilon
    est <- tcrossprod(tmp$U_X, tmp$V_Bt)
    m$epsilon <- G_ - est

    err.new <- mean(m$epsilon ^ 2)

    if ((abs(err.old - err.new) / err.old) < m$err.max) {
      stop <- TRUE
    }
  }

  m$U <- tmp$U_X[,1:m$K]
  m$V <- tmp$V_Bt[,1:m$K]
  m$B <- t(tmp$V_Bt[,-(1:m$K)])
  m$C <- tcrossprod(m$U, m$V)

  # epsilon
  m$epsilon <- G_ - dat$X %*% m$B - m$C
  m$epsilon.sigma2 <- epsilon.sigma2(m$epsilon,
                                     reduced.df = ifelse(m$center,1,0) )


  # B.sigma2 B|V,X so we do not variability of the estimattion of V ... see perso lab notebook (2/12/2016)
  m$B.sigma2 <- B.sigma2(epsilon.sigma2 = m$epsilon.sigma2,
                         X = dat$X,
                         lambda = m$beta)

  m
}

#' @export
run.AlternatedLFMMMethod <- function(m, dat) {

  m <- fit(m, dat)
  run.Method(m, dat)
}
