################################################################################
## FromTrueSampler2

#' see my notebook (13/06/2017), sample a generative model from
#' true dataset.
#'
#'
#' 
#' @export
#' @param cs if NULL sample. Must be of size K.
FromTrueSampler2 <- function(G.file,
                             K,
                             prop.outlier,
                             cs = NULL,
                             pca.file = NULL,
                             rho.B = 1.0,
                             n = NULL,
                             L = NULL
                             ) {

  structure(list(G.file = G.file,
                 K = K,
                 n = n,
                 L = L,
                 pca.file = pca.file,
                 prop.outlier = prop.outlier,
                 cs = cs,
                 rho.B = rho.B,
                 loaded = FALSE),
            class = c("FromTrueSampler2","Sampler"))
}

#' @export
Sampler_load.FromTrueSampler2 <- function(s) {
  Sampler_load.FromTrueSampler(s)
}


#' Sample data from true dataset version 2
#'
#'
#' @export
sampl.FromTrueSampler2 <- function(s) {

  s <- Sampler_load(s)

  L <- ncol(s$G)
  n <- nrow(s$G)
  
  ## U X
  ## cov matrix
  U.sd <- sqrt(diag(cov(s$U)))
  Sigma <- diag(x = 1, nrow = s$K, ncol = s$K)
  Sigma <- rbind(Sigma, matrix(s$cs, nrow = 1))
  Sigma <- cbind(Sigma, matrix(c(s$cs, 1.0), ncol = 1)) ## correlation matrix
  Sigma <- diag(c(U.sd, 1)) %*% Sigma %*% diag(c(U.sd, 1)) ## covariance matrix
  UX <- MASS::mvrnorm(n, mu = rep(0.0,s$K + 1), Sigma = Sigma)
  U <- UX[,1:s$K, drop = FALSE]
  X <- UX[,s$K + 1, drop = FALSE]


  ## B
  aux <- sqrt(solve(crossprod(UX))[s$K + 1, s$K + 1])
  E.sd <- apply(s$E, 2, sd)
  outlier <- sample.int(L, L * s$prop.outlier)
  B <- matrix(0, 1, L)
  B[1,outlier] <- sapply(E.sd[outlier], function(e){rnorm(1, 0, s$rho.B * E.sd * aux)})

  ## G synthese
  G <- s$one %*% s$mu +
    tcrossprod(U, s$V) +
    X %*% B + s$E

  ## we remove variable with no variance
  G <- Preprocessing_filter_sd(G)

  ## filter n and L 
  n <- nrow(G)
  L <- ncol(G)
  if (!is.null(s$n) && s$n <= n) {
    sample.ind <- sample(n, s$n)
  } else {
    ## all
    sample.ind <- 1:n
  }
  if (!is.null(s$L) && s$L <= L) {
    sample.loc <- sample(L, s$L)
  } else {
    ## all
    sample.loc <- 1:L
  }

  # return
  GenerativeDataSet(G = G[sample.ind, sample.loc, drop = FALSE],
                    X = X[sample.ind,,drop = FALSE],
                    U = U[sample.ind,,drop = FALSE],
                    V = s$V[sample.loc,,drop = FALSE],
                    B = B[,sample.loc,drop = FALSE],
                    epsilon = s$E[sample.ind, sample.loc, drop = FALSE],
                    outlier = which(sample.loc %in% outlier),
                    mu = s$mu[1,sample.loc, drop = FALSE])
}
