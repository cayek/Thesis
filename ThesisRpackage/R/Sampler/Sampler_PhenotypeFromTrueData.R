################################################################################
# FromTrueSampler

#' @export
PhenotypeFromTrueSampler <- function(G.file,
                                     climate.file,
                                     pca.file,
                                     n, L, K,
                                     J,
                                     beta,
                                     delta) {
  s <- Sampler()
  s$G.file <- G.file
  s$climate.file <- climate.file
  s$pca.file <- pca.file
  s$n <- n
  s$L <- L
  s$K <- K
  s$J <- J
  s$beta <- beta
  s$delta <- delta
  class(s) <- c("PhenotypeFromTrueSampler", class(s))
  s
}

sampl.PhenotypeFromTrueSampler <- function() {
  # read file
  G <- read_G(s$G.file)
  n <- nrow(G)
  L <- ncol(G)

  # sample row an col
  if (!is.null(s$n) && s$n <= n) {
    G <- G[sample(n, s$n),]
    n <- s$n
  }
  if (!is.null(s$L) && s$L <= L) {
    G <- G[sample(n, s$n),]
    n <- s$n
  }

}
