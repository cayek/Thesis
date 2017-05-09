#' @export
Article3_ValidationNumerique_Sample <- function(L, only.EUR,
                                                dat.file = "~/Projects/Thesis/Data/1000Genomes/Phase3/Eu_Af_Afam.maf.05.rds") {
  dat <- readRDS(dat.file)

  if (only.EUR) {
    ## keep only Eu
    Eu.ind <- dat$indiv$super_pop == "EUR"
    G <- dat$G[Eu.ind,]
  } else {
    G <- dat$G
  }
  
  ## rm for memory
  rm(dat)
  gc()

  ## filter maf
  G <- Preprocessing_filter_maf(G, 0.05, FALSE)

  ## save this dataset
  ## saveRDS(G, "~/Projects/Thesis/Data/1000Genomes/Phase3/Eu.maf.05.G.rds")
  ## G <- readRDS("~/Projects/Thesis/Data/1000Genomes/Phase3/Eu.maf.05.G.rds")

  ## filter sd == 0
  G <- Preprocessing_filter_sd(G, 0.0, FALSE)

  ## sample loci
  sample.loci <- sample.int(ncol(G), L)
  G <- G[, sample.loci]
  dim(G)

  ## standartization
  G <- scale(G)
  anyNA(G)

  ## save
  file <- paste0("~/Projects/Thesis/Data/ThesisDataset/3Article/1000GenomesPhase3/",
                 "ValidationNumerique_",
                 ifelse(only.EUR, "EU", "ALL"),
                 "_L",
                 L,
                 '.G.rds')

  saveRDS(G, file)
  file
}
