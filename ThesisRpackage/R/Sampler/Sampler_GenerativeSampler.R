################################################################################
# Normal sampler object

#' Title
#'
#'
#' @export
NormalSampler <- function(n, L, K, prop.outlier = 0.02, sigma = 0.2,
                          c = 0.6, cs = NULL,
                          mean.B = 0.0,
                          sd.B = 1,
                          sd.mu = 1.0, mean.mu = 0.5,
                          sd.U = 1.0, sd.V = 1.0) {
  structure(list(n = n,
                 L = L,
                 K = K,
                 prop.outlier = prop.outlier,
                 sigma = sigma,
                 c = c,
                 cs = cs,
                 mean.B = mean.B,
                 sd.B = sd.B,
                 sd.mu = sd.mu,
                 mean.mu = mean.mu,
                 sd.U = sd.U,
                 sd.V = sd.V), class = c("NormalSampler","Sampler"))
}

#' Sample G = mu + UV^T + XB^T + e
#'
#'
#' @export
sampl.NormalSampler <- function(x) {

  outlier = ((1 - x$prop.outlier) * x$L + 1):x$L
  nb.outlier = length(outlier)

  if (x$K != 0) {
    U = MASS::mvrnorm(x$n, mu = rep(0.0,x$K), Sigma = x$sd.U * diag(x$K))
    V = MASS::mvrnorm(x$L, mu = rep(0.0,x$K), Sigma = x$sd.V * diag(x$K))
  } else {
    U = NULL
    V = NULL
  }

  mu = matrix(rep(rnorm(x$L, x$mean.mu, x$sd.mu),x$n), nrow = x$n,ncol = x$L, byrow = TRUE)
  if (!is.null(U)) {
    if (!is.null(x$c)) {
      X = matrix(sample_correlated_X(U[, 1], x$c), nrow = x$n,ncol = 1)
    } else {
      X = matrix(sample_X_sum_correlated_U(U, x$cs), nrow = x$n, ncol = 1)
    }
  } else {
    X = MASS::mvrnorm(n = x$n,
                      Sigma = 1 * diag(1, 1, 1),
                      mu = rep(0, 1))
  }
  B = matrix(0, 1, x$L)
  B[1,outlier] = rnorm(nb.outlier, x$mean.B, x$sd.B)
  epsilon = MASS::mvrnorm(x$n, mu = rep(0.0,x$L), Sigma = x$sigma * diag(x$L))

  if (!is.null(U)) {
    G = mu + U %*% t(V) + X %*% B + epsilon
  } else {
    G = mu + X %*% B + epsilon
  }
  GenerativeDataSet(G = G,
                    X = X,
                    U = U,
                    V = V,
                    B = B,
                    epsilon = epsilon,
                    outlier = outlier,
                    mu = mu[1,, drop = FALSE])
}

#' @export
name.NormalSampler <- function(obj) {
  paste0("NormalSampler|K=",obj$K)
}

################################################################################
# Logistic sampler object


#' Title
#'
#'
#' @export
#'
LogisticSampler <- function(n, L, K, ploidy = 1,
                            prop.outlier = 0.02,
                            sigma = 0.0, # error for the normal sample, the binomial already add noise. I keep it for consistency with past code...
                            c = 0.6, mean.B = 0.0,
                            sd.B = 1,
                            sd.mu= 1.0, mean.mu = 0.5,
                            sd.U = 1.0, sd.V = 1.0) {
  structure(list(ploidy = ploidy,
                 n = n,
                 L = L,
                 K = K,
                 prop.outlier = prop.outlier,
                 sigma = sigma,
                 c = c,
                 mean.B = mean.B,
                 sd.B = sd.B,
                 sd.mu = sd.mu,
                 mean.mu = mean.mu,
                 sd.U = sd.U,
                 sd.V = sd.V), class = c("LogisticSampler", "Sampler"))
}

#' Sample P(G) = logistic(mu + UV^T + XB^T + e)
#'
#' @export
sampl.LogisticSampler <- function(x) {

  logistic = function(x){
    return(1 / (1 + exp(-x)))
  }

  normal.model.dat = sampl.NormalSampler(x)
  P_G = logistic(normal.model.dat$G)
  G = apply(P_G, 1:2, function(p) rbinom(1,x$ploidy,p))

  normal.model.dat$G = G
  return(normal.model.dat)
}

################################################################################
# Normal sampler 2

#' see my labnotebook (2/02/2017)
#'
#'
#' @export
NormalSampler2 <- function(n, L, K, prop.outlier = 0.02, sigma = 0.2,
                          cs = c(0.6, rep(0, K - 1)),
                          mean.B = 0.0,
                          sd.B = 1,
                          sd.mu = 1.0, mean.mu = 0.5,
                          sd.U = 1.0, sd.V = 1.0) {
  structure(list(n = n,
                 L = L,
                 K = K,
                 prop.outlier = prop.outlier,
                 sigma = sigma,
                 cs = cs,
                 mean.B = mean.B,
                 sd.B = sd.B,
                 sd.mu = sd.mu,
                 mean.mu = mean.mu,
                 sd.U = sd.U,
                 sd.V = sd.V), class = c("NormalSampler2","Sampler"))
}

#' Sample G = mu + UV^T + XB^T + e
#'
#'
#' @export
sampl.NormalSampler2 <- function(s) {

  outlier = ((1 - s$prop.outlier) * s$L + 1):s$L
  nb.outlier = length(outlier)

  # U and X
  Sigma <- diag(x = s$sd.U, nrow = s$K, ncol = s$K)
  Sigma <- rbind(Sigma, matrix(s$cs, nrow = 1))
  Sigma <- cbind(Sigma, matrix(c(s$cs, 1.0), ncol = 1))
  UX <- MASS::mvrnorm(s$n, mu = rep(0.0,s$K + 1), Sigma = Sigma)
  U <- UX[,1:s$K, drop = FALSE]
  X <- UX[,s$K + 1, drop = FALSE]

  # V
  V <- MASS::mvrnorm(s$L, mu = rep(0.0, s$K), Sigma = s$sd.V * diag(s$K))


  mu <- matrix(rep(rnorm(s$L, s$mean.mu, s$sd.mu), s$n), nrow = s$n, ncol = s$L, byrow = TRUE)

  B <- matrix(0, 1, s$L)
  B[1,outlier] <- rnorm(nb.outlier, s$mean.B, s$sd.B)
  epsilon = MASS::mvrnorm(s$n, mu = rep(0.0, s$L), Sigma = s$sigma * diag(s$L))

  G = mu + U %*% t(V) + X %*% B + epsilon

  GenerativeDataSet(G = G,
                    X = X,
                    U = U,
                    V = V,
                    B = B,
                    epsilon = epsilon,
                    outlier = outlier,
                    mu = mu[1,, drop = FALSE])
}

#' @export
name.NormalSampler2 <- function(obj) {
  paste0("NormalSampler2|K=",obj$K)
}


