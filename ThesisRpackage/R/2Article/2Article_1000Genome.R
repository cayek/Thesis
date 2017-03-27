#' @export
Article2_1000Genome <- function(s, ## oncly for test
                                K,
                                cluster.nb = NULL,
                                save = TRUE,
                                bypass = FALSE) {

  cl <- long_init(cluster.nb = cluster.nb,
            bypass = bypass)

  require(tess3r)
  require(ThesisRpackage)
  if (is.null(s)) {
    dat <- readRDS("~/Projects/Thesis/Data/1000Genomes/Phase3Chrm22/Eu_Af_Afam.maf.05.rds")
  } else {
    dat <- sampl(s)
  }
  exp <- Experiment(name = "Article2_1000Genome",
                    description = make_description("run of tess3r", dat.file = dat$G.file,
                                                   K = K))

  ## tess3
  exp$tess3R.method <- tess3RImplementationMethod(K = K)
  exp$tess3R.method <- fit(exp$tess3R.method, dat)
  if (!is.null(cl)) {
    parallel::stopCluster(cl)
  }

  ## snmf
  exp$snmf.method <- sNMFMethod(K = K,
                                openMP.core.num = ifelse(!is.null(cluster.nb), cluster.nb, 1))
  exp$snmf.method <- fit(m = exp$snmf.method, dat)

  ## save exp
  if (save) {
    dumpExperiment(exp)
  }
  exp
}
