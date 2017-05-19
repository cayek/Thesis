FromTrueSampler.builder <- setRefClass("FromTrueSampler", contains = "Sampler",
                                       fields = c("G.file",
                                                  "pca.file",
                                                  "n",
                                                  "L",
                                                  "K",
                                                  "prop.outlier",
                                                  "rho",
                                                  "cs",
                                                  "round",
                                                  "sd.V.rho",
                                                  "rho.E",
                                                  "B.outlier.sampler",
                                                  "G",
                                                  "U",
                                                  "V",
                                                  "one",
                                                  "mu",
                                                  "E"
                                                  ))






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
                            B.outlier.sampler = function(n, mean, sd) rnorm(n, mean, sd),
                            reference = FALSE) {

  if (reference) {
    FromTrueSampler.builder$new(G.file = G.file,
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
                                B.outlier.sampler = B.outlier.sampler,
                                loaded = FALSE)
  } else {
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
                   B.outlier.sampler = B.outlier.sampler,
                   loaded = FALSE),
              class = c("FromTrueSampler","Sampler"))
  }
}

#' @export
Sampler_load.FromTrueSampler <- function(s) {
  if (!s$loaded) {
    s$G <- read_G(s$G.file)
    ## remove NA
    s$G <- Preprocessing_filter_na(s$G, 0)
    pca <- compute_PCA(s, s$G)
    s$U <- pca$x[,1:s$K]
    s$V <- pca$rotation[,1:s$K]
    s$mu <- matrix(pca$center, 1, ncol(s$G))
    s$one <- matrix(1, nrow(s$G), 1)
    s$E <- s$G - s$one %*% s$mu - tcrossprod(s$U, s$V)
    s$loaded <- TRUE
  }
  s
}


#' Sample data from true dataset
#'
#'
#' @export
sampl.FromTrueSampler <- function(s) {

  ## load the sampler
  s <- Sampler_load(s)

  ## sample row an col
  n <- nrow(s$G)
  L <- ncol(s$G)
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

  
  ## G
  G <- s$G[sample.ind, sample.loc]
  n <- nrow(G)
  L <- ncol(G)

  ## U V mu
  U <- s$U[sample.ind,, drop = FALSE]
  V <- s$V[sample.loc,, drop = FALSE]
  mu <- s$mu[,sample.loc, drop = FALSE]
  one <- s$one[sample.ind,, drop = FALSE]
  E <- s$E[sample.ind, sample.loc, drop = FALSE]

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
