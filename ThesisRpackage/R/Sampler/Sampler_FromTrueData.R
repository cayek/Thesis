################################################################################
# FromTrueSampler

#' see my notebook (18/01/2017), sample a generative model from
#' true dataset.
#'
#' @param rho must be between 0 and 1. the proportion of variance explained by X.
#' If NULL just a sum between UV^T and XB
#'
#' @export
FromTrueSampler <- function(G.file,
                            n, L, K,
                            prop.outlier,
                            pca.file = NULL,
                            rho = NULL,
                            cs = NULL,
                            round = FALSE,
                            sd.V.rho = 1, ## var of association effect (B) * var of V,
                            rho.E = 1.0, ## coef before E
                            B.outlier.sampler = function(n, mean, sd) rnorm(n, mean, sd)) {
  structure(list(G.file = G.file,
                 pca.file = pca.file,
                 n = n,
                 L = L,
                 K = K,
                 prop.outlier = prop.outlier,
                 rho = rho,
                 cs = cs,
                 sd.V.rho = sd.V.rho,
                 rho.E = rho.E,
                 round = round,
                 B.outlier.sampler = B.outlier.sampler),
            class = c("FromTrueSampler","Sampler"))
}

#' Sample data from true dataset
#'
#'
#' @export
sampl.FromTrueSampler <- function(s) {

  ## read file
  G <- read_G(s$G.file)
  n <- nrow(G)
  L <- ncol(G)

  ## sample row an col
  if (!is.null(s$n) && s$n <= n) {
    G <- G[sample(n, s$n),]
  }
  if (!is.null(s$L) && s$L <= L) {
    G <- G[,sample(L, s$L)]
  }

  ## remove NA
  G <- Preprocessing_filter_na(G, 0)

  ## recompute L and n
  n <- nrow(G)
  L <- ncol(G)

  ## center
  one <- matrix(1, n, 1)
  mu <- matrix(G %>% purrr::array_branch(2) %>%
                   purrr::map_dbl(mean, na.rm = TRUE),
                 1, L)
  G_ <- G - one %*% mu

  ## compute svd: G = C + E (C = U Sigma V^T)
  pca <- compute_PCA(s, G_)
  U <- pca$x[,1:s$K]
  V <- pca$rotation[,1:s$K]
  E <- G_ - tcrossprod(U,
                       V)

  ## compute X
  if (is.null(s$cs)) {
    s$cs <- runif(s$K)
  } else if (is.function(s$cs)) {
    s$cs <- s$cs() # ^^
  }
  X <- sample_X_sum_correlated_U(U = U, cs = s$cs)

  ## outlier
  nb.outlier <- s$prop.outlier * L
  outlier <- sample(L, nb.outlier)
  sd.V <- sd(as.vector(V))
  ## compute a B
  B = matrix(0, 1, L)
  B[1, outlier] = s$B.outlier.sampler(nb.outlier, 0, sd.V * s$sd.V.rho)

  ## variance balancing between structure and co-variable X
  if (!is.null(s$rho)) {
    a <- (1 - s$rho)
    b <- s$rho
  } else {
    a <- 1
    b <- 1
  }
  V[outlier,] <- a * V[outlier,,drop = FALSE]
  B[,outlier] <- b * B[,outlier, drop = FALSE]

  # synthese
  G <- one %*% mu +
    tcrossprod(U, V) +
    X %*% B +
    s$rho.E * E

  if (s$round) {
    G[,outlier] <- round(G[,outlier, drop = FALSE])
  }

  ## we remove variable with no variance
  G <- Preprocessing_filter_sd(G)

  # return
  GenerativeDataSet(G = G,
                    X = X,
                    U = U,
                    V = V,
                    B = B,
                    epsilon = E,
                    outlier = outlier,
                    mu = mu[1,, drop = FALSE])
}
