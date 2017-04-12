################################################################################
# helpers

#' core of the classic linear regression
classicLm <- function(G_, X, center, res = list()) {

  res$B <- B_ridge(G_, X, 0.0)
  res$epsilon <- G_ - X %*% res$B
  res$epsilon.sigma2 <- epsilon.sigma2(res$epsilon,
                                       reduced.df = ifelse(center,1,0) + ncol(X))
  res$B.sigma2 <- B.sigma2(epsilon.sigma2 = res$epsilon.sigma2,
                           X = X,
                           lambda = 0.0,
                           A = G_)
  res
}


# solve ||A - x B||^2 + lambda ||B||^2
B_ridge <- function(A, X, lambda) {
  if (anyNA(A)) {
    ## Whith missing value
    L <- ncol(A)
    d <- ncol(X)
    B <- matrix(NA, d, L)
    D <- diag(1, d, d)
    for (j in 1:L) {
      missing.row <- is.na(A[,j])
      A_j <- A[!missing.row,j]
      X_j <- X[!missing.row,]
      B[,j] <- solve((crossprod(X_j,X_j) + lambda * D), crossprod(X_j, A_j))
    }
    B
  } else {
    ## Whithout missing value
    D <- diag(1, ncol(X), ncol(X))
    solve((crossprod(X,X) + lambda * D), crossprod(X, A))
  }
}

# solve ||A - x B||^2 + lambda |B|
# we assume that covariate are orthogonal !!
B_lasso <- function(A, X, lambda) {
  B_hat <- solve((crossprod(X,X) ), crossprod(X, A))
  sign(B_hat) * ((abs(B_hat) - lambda) %>% purrr::map_dbl(~ max(.x, 0)))
}

#' formula find in Cule et al. 2011
B.sigma2 <- function(epsilon.sigma2, X, lambda, A = NULL) {
  # const
  d <- ncol(X)
  D <- diag(1, d, d)

  if (!is.null(A) && anyNA(A)) {
    ## Whith missing value
    L <- ncol(A)
    var.B <- matrix(NA, d, L)
    for (j in 1:L) {
      missing.row <- is.na(A[,j])
      X_j <- X[!missing.row,]
      aux <- solve(crossprod(X_j) + lambda * D)
      var.B[,j] <- matrix(diag(aux %*% crossprod(X_j) %*% aux), d, 1) * epsilon.sigma2[,j]
    }
    var.B
  } else {
    ## Whithout missing value
    # compute sigma2.B
    aux <- solve(crossprod(X) + lambda * D)
    var.B <- matrix(diag(aux %*% crossprod(X) %*% aux), d, 1)
    return( var.B %*% epsilon.sigma2)
  }
}

#' formula find in Cule et al. 2011
#'
#' @param reduced.df use to compute effective.degree.freedom
epsilon.sigma2 <- function(epsilon, reduced.df) {
  L <- ncol(epsilon)

  if (anyNA(epsilon)) {
    ## Whithout missing value
    ## compute s2
    sigma2 <- epsilon %>% purrr::array_branch(2) %>%
      purrr::map_dbl(function(x) sum(x ^ 2, na.rm = TRUE) / (sum(!is.na(x)) - reduced.df))
    sigma2 <- matrix(sigma2, 1, L)
    sigma2
  } else {
    ## Whithout missing value
    effective.degree.freedom <- nrow(epsilon) - reduced.df
    ## compute s2
    sigma2 <- epsilon %>% purrr::array_branch(2) %>%
      purrr::map_dbl(function(x) sum(x ^ 2) / effective.degree.freedom)
    sigma2 <- matrix(sigma2, 1, L)
    sigma2
  }
}

################################################################################
# classic Linear method

#' G = X B + E
#'
#' @export
ClassicLinearMethod <- function(center = TRUE,
                                hypothesis.testing.method = Zscore(),
                                nickname = NULL) {
  m <- Method(name = "ClassicLinearMethod",
              hypothesis.testing.method = hypothesis.testing.method,
              nickname = nickname)
  class(m) <- c("ClassicLinearMethod", class(m))
  m$center <- center
  m
}

#' @export
fit.ClassicLinearMethod <- function(m, dat, reuse = FALSE) {

  # constants
  n <- nrow(dat$G)
  L <- ncol(dat$G)
  one <- matrix(1, n, 1)

  # compute center
  if (m$center) {
    m$mu <- matrix(dat$G %>% purrr::array_branch(2) %>%
                     purrr::map_dbl(mean, na.rm = TRUE),
                   1, L)
    G_ <- dat$G - one %*% m$mu
  } else {
    m$mu <- matrix(0, 1, L)
    G_ <- dat$G
  }

  ## We do not impute missing values first !!!
  m <- classicLm(G_, dat$X, center = m$center, res = m)

  if (anyNA(G_)) {
    flog.debug("fit.ClassicLinearMethod : Missing values detected")
    #   G_ <- m$impute.genotype.method$fun(G_)
    m$missing.index <- which(is.na(G_))
    m$imputed.values <- (one %*% m$mu +  dat$X %*% m$B)[m$missing.index]
  }


  return(m)
}

#' @export
run.ClassicLinearMethod <- function(m, dat) {

  m <- fit(m, dat)
  run.Method(m, dat)
}



#' loglr of classic lineara method
#'
#' for linear regresion the log likelyhood ratio is
#'  - 1 / (2 * sigma^2) (RSS_0 - RSS) whit murphy's book notation
#'
#' @export
loglr.ClassicLinearMethod <- function(m, dat) {
  L <- ncol(dat$G)
  n <- nrow(dat$G)
  d <- ncol(dat$X)
  one <- matrix(1, n, 1)

  log.lr <- matrix(NA, d, L)

  for (h in 1:d) {
    B0 <- m$B
    B0[h,] <- 0 # h-th co variate is set to zero
    RSS0 <- (dat$G - one %*% m$mu - dat$X %*% B0) %>%
      purrr::array_branch(2) %>%
      purrr::map_dbl(function(x) sum((x) ^ 2) )
    RSS <- m$epsilon %>%
      purrr::array_branch(2) %>%
      purrr::map_dbl(function(x) sum((x) ^ 2))
    log.lr[h,] <- -1 / (2 * m$epsilon.sigma2) * (RSS0 - RSS)
  }
  log.lr
}

################################################################################
# PCA + classic Linear method



#' PCA + linear method
#'
#' svd(G) -> U ...
#' G = (U|X) B + E
#'
#' @export
PCAClassicLinearMethod <- function(K,
                                   center = TRUE,
                                   hypothesis.testing.method = Zscore(),
                                   nickname = NULL,
                                   assumingStructure = FALSE) {
  m <- Method(name = "PCAClassicLinearMethod",
              hypothesis.testing.method = hypothesis.testing.method,
              nickname = nickname)
  class(m) <- c("PCAClassicLinearMethod", class(m))
  m$center <- center
  m$K <- K
  m$assumingStructure <- assumingStructure
  m
}


#' @export
fit.PCAClassicLinearMethod <- function(m, dat, reuse = FALSE) {

  # constants
  n <- nrow(dat$G)
  L <- ncol(dat$G)
  one <- matrix(1, n, 1)
  d <- ncol(dat$X)

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
    flog.debug("fit.PCAClassicLinearMethod: Missing values detected")
    G_ <- m$impute.genotype.method$fun(G_)
  }

  if (m$assumingStructure && is.null(dat$U)) {
    flog.warning("dat$U is null so oracle can not use this information !")
  }

  ## Run K svd
  if (m$assumingStructure && !is.null(dat$U)) {
    m$U <- dat$U
    m$K <- ncol(dat$U)
  } else {
    svd.res <- svd(G_, nu = m$K, nv = m$K)
    m$U <- svd.res$u
    # Run K pca
    #pca.res <- prcomp(G_, center = FALSE, scale = FALSE)
    #m$U <- pca.res$x[,1:m$K]; cat("pca.res")
  }

  # run lm
  X_ <- cbind(dat$X, m$U)
  B_ <- B_ridge(G_, X_, 0.0)
  m$B <- B_[1:d,, drop = FALSE]
  m$V <- t(B_[(d + 1):(d + m$K),, drop = FALSE])
  m$C <- tcrossprod(m$U, m$V)
  m$epsilon <- G_ - X_ %*% B_
  m$epsilon.sigma2 <- epsilon.sigma2(m$epsilon,
                                     reduced.df = ifelse(m$center,1,0))
  m$B.sigma2 <- B.sigma2(epsilon.sigma2 = m$epsilon.sigma2,
                         X = X_,
                         lambda = 0.0)[1:d,, drop = FALSE]
  return(m)
}

#' @export
run.PCAClassicLinearMethod <- function(m, dat) {

  m <- fit(m, dat)
  run.Method(m, dat)
}
