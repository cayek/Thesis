################################################################################
# helpers

U_V_hat <- function(G, P, K, V = NULL) {
  res <- list()
  if (K != 0) {
    if (is.null(V)) {
      svd.G = svd(P %*% G, nu = K, nv = K)
      PU = svd.G$u %*% diag(svd.G$d[1:K], nrow = K, ncol = K)
      res$V = svd.G$v
      PC = PU %*% t(res$V)
      res$C = solve(P,PC)
      res$U = solve(P,PU)
    } else {
      # we use the V provided
      res$V <- V
      res$U = G %*% res$V
      res$C = tcrossprod(res$U, res$V)
    }
  }else{
    res$U = NULL
    res$V = NULL
    res$C = matrix(0,nrow(G),ncol(G))
  }
  res
}

#' Compute P, U, V, C and B of the ridge LFMM method
RidgeLFMMMethod_main <- function(m, G_, dat, lambda, reuse) {

  d <- ncol(dat$X)
  n <- nrow(dat$G)

  # Computation of P (see my notebook) :D
  if (!reuse) {
    In <- diag(1, n, n)
    D <- diag(1, d, d)
    P_X_lambda_t = In - dat$X %*% solve(crossprod(dat$X, dat$X) + lambda * D, t(dat$X))
    eigen.P_X_D_t = eigen(P_X_lambda_t, symmetric = TRUE)
    m$P = eigen.P_X_D_t$vectors %*% diag(sqrt(eigen.P_X_D_t$values)) %*% t(eigen.P_X_D_t$vectors)
  }

  # U V
  if (m$reuse.V && !is.null(m$V)) {
    tmp <- U_V_hat(G_, m$P, m$K, V = m$V)
  } else {
    tmp <- U_V_hat(G_, m$P, m$K)
  }
  m[names(tmp)] <- tmp
  rm(tmp)

  # B
  m$B <- B_ridge(A = G_ - m$C, X = dat$X, lambda = lambda)
  m
}

################################################################################
# RidgeLFMMMethod

#' ||G  - U V^t - X B || + lambda ||B||^2
#'
#' @export
RidgeLFMMMethod <- function(K,
                            hypothesis.testing.method = Zscore(NormalZscore(),
                                                               AnalyticSigma2Functor()),
                            lambda = 1.0,# better diff than zero for numerical stability
                            center = TRUE,
                            name = "RidgeLFMMMethod",
                            nickname = NULL,
                            reuse.V = FALSE) {
  m <- Method(name, hypothesis.testing.method,
              nickname = nickname)
  class(m) <- c("RidgeLFMMMethod", "LFMMMethod",class(m))
  m$center <- center
  m$K = K
  m$lambda = lambda
  m$reuse.V = reuse.V
  m
}


#' @export
fit.RidgeLFMMMethod <- function(m, dat, reuse = FALSE, light = TRUE) {

  # param
  n <- nrow(dat$G)
  L <- ncol(dat$G)

  # compute center
  m <- mu(m, dat$G)
  G_ <- center(m, dat$G)

  # impute missing value
  if (anyNA(G_)) {
    flog.debug("Missing values detected")
    G_ <- m$impute.genotype.method$fun(G_)
  }

  # computation of lambda if null, see my notebook le 30/01/2016)
  if (is.null(m$lambda)) {
    aux <- crossprod(dat$X)
    vp <- max(eigen(aux)$values)
    epsilon <- 1e-3
    m$lambda <- vp / (1 / epsilon - 1)
  }

  m <- RidgeLFMMMethod_main(m = m,
                            G_ = G_,
                            dat = dat,
                            lambda = m$lambda,
                            reuse = reuse)


  if (!light) {
    ## epsilon
    m$epsilon <- G_ - dat$X %*% m$B - m$C
    m$epsilon.sigma2 <- epsilon.sigma2(m$epsilon,
                                       reduced.df = ifelse(m$center,1,0))


    ## B.sigma2 B|V,X so we do not variability of the estimattion of V ... see perso lab notebook (2/12/2016)
    m$B.sigma2 <- B.sigma2(epsilon.sigma2 = m$epsilon.sigma2,
                           X = dat$X,
                           lambda = m$lambda)
  } else {
    m$C <- NULL
    m$P <- NULL
  }
  m
}

#' @export
run.RidgeLFMMMethod <- function(m, dat) {

  m <- fit(m, dat)
  run.Method(m, dat)
}

#' prediction function for ridge lfmm.
#'
#' Assume that fit has been run.
#'
#'   @export
predict.RidgeLFMMMethod <- function(m, dat) {

  n <- nrow(dat$G)
  one <- matrix(1, n, 1)

  # compute U
  G_ <- center(m, dat$G)
  res.U <- U_V_hat(G_, m$P, m$K, V = m$V)

  # return
  one %*% m$mu +
    res.U$C +
    dat$X %*% m$B

}
