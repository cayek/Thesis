#' @export
long_gwas_riz_PCA <- function(n = NULL,
                          L = NULL,
                          K = c(1, 2, 3),
                          lambdas = c(1e-10, 1e-1, 1e10),
                          cluster.nb = NULL,
                          save = TRUE, bypass = FALSE) {

  long_init(cluster.nb = cluster.nb,
            bypass = bypass)

  ## sampler
  s <- TrueSampler(G.file = "../Data/gwas_riz/climate/genotype.rds",
                   X.file = "../Data/gwas_riz/climate/tmaxPC2.rds",
                   outlier.file = NULL,
                   n = NULL,
                   L = NULL)

  ## PCA

  ## cross validation

  ## lfmm
  lfmm <- finalLfmmRdigeMethod(K = NULL,
                               lambda = NULL,
                               calibrate = FALSE)

  exp <- Experiment()

  # save exp
  if (save) {
    dumpExperiment(exp)
  }
  exp


}

#' @export
long_gwas_riz_CV <- function(n = NULL,
                              L = NULL,
                              K = c(1, 2, 3),
                              lambdas = c(1e-10, 1e-1, 1e10),
                              cluster.nb = NULL,
                              save = TRUE, bypass = FALSE) {

  long_init(cluster.nb = cluster.nb,
            bypass = bypass)

  ## sampler
  s <- TrueSampler(G.file = "../Data/gwas_riz/climate/genotype.rds",
                   X.file = "../Data/gwas_riz/climate/tmaxPC2.rds",
                   outlier.file = NULL,
                   n = NULL,
                   L = NULL)

  ## CV



  # save exp
  if (save) {
    dumpExperiment(exp)
  }
  exp


}

#' @export
long_gwas_riz_run_LFMM <- function(n = NULL,
                             L = NULL,
                             K = c(1, 2, 3),
                             lambdas = c(1e-10, 1e-1, 1e10),
                             cluster.nb = NULL,
                             save = TRUE, bypass = FALSE) {

}

