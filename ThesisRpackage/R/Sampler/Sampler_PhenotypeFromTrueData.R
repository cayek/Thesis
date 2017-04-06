################################################################################
# FromTrueSampler

#' @export
PhenotypeFromTrueSampler <- function(G.file,
                                     coord.file,
                                     env.file,
                                     pca.file,
                                     n, L, K,
                                     J,
                                     beta,
                                     delta) {
  s <- Sampler()
  s$G.file <- G.file
  s$coord.file<- coord.file
  s$env.file <- env.file
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

#' @export
sampl.PhenotypeFromTrueSampler <- function(s) {

  TestRequiredPkg("raster")

  ## read file
  G <- read_G(s$G.file)
  coord <- read_all(s$coord.file)
  n <- nrow(G)
  L <- ncol(G)

  ## sample row an col
  if (!is.null(s$n) && s$n <= n) {
    sample.n <- sample.int(n, s$n)
    G <- G[sample.n,]
    coord <- coord[sample.n,]
    n <- s$n
  }
  if (!is.null(s$L) && s$L <= L) {
    G <- G[,sample.int(L, s$L)]
    L <- s$L
  }

  ## environment variable
  if (!is.null(s$env.file) && file.exists(s$env.file)) {
    flog.trace("Reading env from", s$env.file)
    env <- read_all(s$env.file)
  } else {
    flog.trace("Computing env")
    climate <- raster::getData('worldclim', var='bio', res = 2.5)
    bio <- raster::extract(climate, y = coord)
    pc.bio <- prcomp(bio,scale = T)
    env <- pc.bio$x[,1]
    if (!is.null(s$env.file)) {
      flog.trace("Writting env to", s$env.file)
      saveRDS(env, s$env.file)
    }
  }

  ## PCA
  if (!is.null(s$pca.file) && file.exists(s$pca.file)) {
      flog.trace("Reading pca from", s$pca.file)
    pca <- read_all(s$pca.file)
  } else {
    pca <- prcomp(G)
    if (!is.null(s$pca.file)) {
      flog.trace("Writting file", s$pca.file)
      saveRDS(pca, s$pca.file)
    }
  }

  ## X simulation
  flog.trace("Computing phenotype")
  sigma = sqrt(sum(pca$sdev ^ 2) - sum(pca$sdev[1:s$K] ^ 2))
  base.effect = sqrt(sum(pca$sdev ^ 2))
  J = s$J
  loc.J = sort(sample.int(ncol(G), s$J))
  beta = s$beta * base.effect
  delta = s$delta * base.effect ##G X E
  X = beta * rowSums(G[,loc.J]) +
    delta * rowSums(G[,loc.J]) * env +
    rowSums(pca$x[,1:s$K]) + rnorm(n, sd = sigma)
  
  ## return
  X = matrix(X, n, 1)
  dat <- TrueDataSet(G = G,
              X = X,
              outlier = loc.J)
  dat$env <- env
  dat$coord <- coord
  dat
}
