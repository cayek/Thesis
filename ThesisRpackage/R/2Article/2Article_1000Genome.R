#' @export
Article2_1000Genome <- function(dat.file = "~/Projects/Thesis/Data/1000Genomes/Phase3Chrm22/Eu_Af_Afam.maf.05.rds",
                                K = 3,
                                openMP.core.num = 1,
                                save = TRUE) {

  require(tess3r)
  require(ThesisRpackage)
  dat <- readRDS(dat.file)

  exp <- Experiment(name = "Article2_1000Genome",
                    description = make_description("run of tess3r", dat.file = dat.file,
                                                   K = K))

  ## compute XBin
  ploidy <- computePloidy(dat$G)
  XBin <- computeXBin(dat$G, ploidy)


  ## snmf
  DebugMessage("Run of snmf")
  exp$snmf.method <- sNMFMethod(K = K,
                                openMP.core.num = ifelse(!is.null(openMP.core.num), openMP.core.num, 1))
  exp$snmf.method <- fit(m = exp$snmf.method, dat)


  ## tess3
  DebugMessage("Run of tess3")
  exp$tess3r <- tess3r::tess3Main(X = NULL,
                                  XProba = XBin,
                                  coord = dat$coord,
                                  K = K,
                                  ploidy = ploidy,
                                  lambda = 1.0,
                                  W = dat$W,
                                  method = "projected.ls",
                                  max.iteration = 200,
                                  tolerance = 1e-5,
                                  openMP.core.num = openMP.core.num,
                                  Q.init = NULL,
                                  mask = 0.0,
                                  copy = FALSE,
                                  algo.copy = FALSE,
                                  verbose = TRUE,
                                  only.ancestry = TRUE)


  ## save exp
  if (save) {
    dumpExperiment(exp)
  }
  exp
}

#' @export
Article2_1000Genome_res <- function(exp, indiv) {

  assertthat::assert_that(exp$name == "Article2_1000Genome")
  df.res <- tibble()


  pops <- list()
  pops[["EU"]] <- c("TSI", "GBR")
  pops[["AFAM"]] <- c("ASW")
  pops[["AF"]] <- c("YRI", "LWK")

  ## tess3
  cluster.mean <- list()
  Q <- exp$tess3r$Q
  for (n in names(pops)) {
    cluster.mean[[n]] <- apply(Q[indiv$pop %in% pops[[n]],], 2, mean)
  }
  df.res <- as_tibble(cluster.mean) %>%
    mutate(method = "tess3") %>%
    rbind(df.res)

  ## snmf
  cluster.mean <- list()
  Q <- exp$snmf.method$Q
  for (n in names(pops)) {
    cluster.mean[[n]] <- apply(Q[indiv$pop %in% pops[[n]],], 2, mean)
  }
  df.res <- as_tibble(cluster.mean) %>%
    mutate(method = "snmf") %>%
    rbind(df.res)

  df.res
}
