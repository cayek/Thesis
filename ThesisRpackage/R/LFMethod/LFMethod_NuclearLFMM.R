################################################################################
# helpers

# see Cai et al. 2010 A Singular Value Thresholding Algorithm for Matrix Completion
D_thau <- function(m, X) {
  svd.res <- svd(X, nu = 0, nv = 0) # compute only singular value
  aux <- svd.res$d - m$gamma
  if (m$soft) {
    Sigma <- diag(aux[aux > 0.0])
  } else {
    Sigma <- diag(svd.res$d[aux > 0.0])
  }
  # Sigma <- diag(svd.res$d[svd.res$d > gamma])
  m$K <- ncol(Sigma)
  DebugMessage(paste("---> K = ", m$K, "\n"))
  svd.res <- svd(X, nu = m$K, nv = m$K)
  m$U <- svd.res$u %*% Sigma
  m$V <- svd.res$v
  m$C <- tcrossprod(m$U, m$V)

  m
}

################################################################################
# NuclearNorm LFMM

#' ||G  - C - X B || + gamma ||C||_*
#'
#' @param gamma if -1 try to compute a gamma such that rk(C) = K.
#'
#' @export
NuclearLFMMMethod <- function(K,
                             hypothesis.testing.method = Zscore(NormalZscore(),
                                                               AnalyticSigma2Functor()),
                             it.max = 100,
                             err.max = 1e-6,
                             gamma = NULL,
                             lambda = 0.0, # if null regularization path
                             lambda.K = 100, # default value used in Friedman et al. 2010
                             lambda.eps = 0.001, # default value used in Friedman et al. 2010
                             sparse.prop = NULL, # try to find the lambda such that not null lambda proportion equal this param
                             center = TRUE,
                             name = "NuclearLFMMMethod",
                             lasso = FALSE,
                             soft = TRUE,
                             nickname = NULL) {
  m <- Method(name, hypothesis.testing.method,
              nickname = nickname)
  class(m) <- c("NuclearLFMMMethod", class(m))
  m$center <- center
  m$K <- K
  m$gamma <- gamma
  m$lambda <- lambda
  m$it.max <- it.max
  m$err.max <- err.max
  m$lasso <- lasso
  m$soft <- soft
  m$lambda.K <- lambda.K
  m$lambda.eps <- lambda.eps
  m$sparse.prop <- sparse.prop
  m
}


#' @export
fit.NuclearLFMMMethod <- function(m, dat, reuse = FALSE) {

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


  # compute a gamma
  if (is.null(m$gamma)) {
    svd.res <- svd(G_, nu = 0, nv = 0) # compute only singular value
    m$gamma <- (svd.res$d[m$K] + svd.res$d[m$K + 1]) / 2
    DebugMessage(paste("Gamma = ", m$gamma, "\n"))
  }


  # init algo
  if (!reuse) {
    m$B <- matrix(0, d, L)
    m$C <- matrix(0, n, L)
  }
  it <- 0
  m$epsilon <- G_ - dat$X %*% m$B - m$C
  err.new <- mean((m$epsilon) ^ 2)
  stop <- FALSE
  # main loop
  while ((it < m$it.max) && !stop) {

    DebugMessage(paste("it = ",it, "| err = ", err.new, "\n"))
    err.old <- err.new
    it <- it + 1

    ## calculate C
    m <- D_thau(m, G_ - dat$X %*% m$B)

    ## calculate B
    if (m$lasso) {
      m$B <- B_lasso(A = G_ - m$C, X = dat$X, lambda = m$lambda)
      DebugMessage(paste("---> mean(B == 0) = ", mean(m$B == 0.0), "\n"))
    } else {
      m$B <- B_ridge(A = G_ - m$C, X = dat$X, lambda = m$lambda)
    }

    ## calculate epsilon
    est <- dat$X %*% m$B + m$C
    m$epsilon <- G_ - est

    err.new <- mean(m$epsilon ^ 2)

    if ((abs(err.old - err.new) / err.old) < m$err.max) {
      stop <- TRUE
    }
  }

  # epsilon
  m$epsilon <- G_ - dat$X %*% m$B - m$C
  m$epsilon.sigma2 <- epsilon.sigma2(m$epsilon,
                                     reduced.df = ifelse(m$center,1,0))


  # B.sigma2 B|V,X so we do not variability of the estimattion of V ... see perso lab notebook (2/12/2016)
  m$B.sigma2 <- B.sigma2(epsilon.sigma2 = m$epsilon.sigma2,
                         X = dat$X,
                         lambda = m$lambda)

  m
}

#' @export
run.NuclearLFMMMethod <- function(m, dat) {

  m <- fit.NuclearLFMMMethod(m, dat)
  run.Method(m, dat)
}
