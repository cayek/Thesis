#' run a PCA on HGDB dataset
#'
#' @export
HGDP_PCA <- function(s, save = TRUE) {

  exp <- PCAExperiment(s = s, description = paste0("PCAexperiment on ", basename(s$G.file), " and ", basename(s$X.file)))
  exp <- runExperiment(exp)

  # save exp
  if (save) {
    dumpExperiment(exp)
  }

  exp
}


#' crossvalidation experiment
#'
#' @export
HGDB_crossvalidation <- function(s, K, kfold, lambdas, save = TRUE) {

  # lfmm ridge
  lfmm.ridge <- RidgeLFMMMethod(K = K,
                                lambda = NULL,
                                nickname = "lfmm ridge")
  # run exp
  exp <- CrossValidationExperiment(m = lfmm.ridge,
                                   s = s,
                                   kfold = kfold,
                                   lambdas = lambdas)
  exp <- runExperiment(exp)

  # save exp
  if (save) {
    dumpExperiment(exp)
  }
  exp
}





#' main experiment
#'
#' @export
HGDB_runs <- function(s, Ks, lambdas, save = TRUE) {
  ComparisonExperiment_GridRun(method.constructor = finalLfmmRdigeMethod,
                               s = s,
                               ms = NULL,
                               Ks = Ks,
                               lambdas = lambdas,
                               gif = c(TRUE),
                               save = save)
}

#' @export
HGDB_get_frichotLoci <- function() {
  table <- tabulizer::extract_tables("~/Projects/Biblio/org-ref-pdfs/frichot13_testin_assoc_between_loci_envir.pdf",
                                     pages = 8, method = "data.frame")[[1]]
  loci.names <- table$Data.[grepl("rs", table$Data.)]
  loci.names
}

#' plot the result
#'
#' @export
HGDB_runs_plot <- function(loci.found, s, exp, K, lambda) {

  ## sample data
  set.seed(exp$seed)
  dat <- sampl(s)

  color <- colnames(dat$G) %in% loci.found

  ## retrieve
  pattern <- paste0("lambda=",lambda,"\\|K=",K)
  toplot <- exp$df.res %>%
    dplyr::filter(grepl(pattern, method.name)) %>%
    dplyr::filter(variable.name == "pvalue1")
  ggplot2::ggplot(toplot, aes(x = index, y = -log10(estimate), color = color, size = color)) +
    geom_point()
}

################################################################################
# Long running

#' Run of PCA
#'
#'
#' @export
long_HGDB_PCA <- function() {
  library(Article3Package)

  ## X_prec

  G.file <- "~/Projects/Data2016_2017/Hgdp_Li/Hgdp_Li.rds"
  X.file <- "~/Projects/Data2016_2017/Hgdp_Li/X_prec.rds"

  s <- TrueSampler(G.file = G.file,
                   X.file = X.file,
                   outlier.file = NULL,
                   n = NULL,
                   L = NULL)

  exp <- HGDP_PCA(s, save = TRUE)


  ## X_tmp

  G.file <- "~/Projects/Data2016_2017/Hgdp_Li/Hgdp_Li.rds"
  X.file <- "~/Projects/Data2016_2017/Hgdp_Li/X_tmp.rds"

  s <- TrueSampler(G.file = G.file,
                   X.file = X.file,
                   outlier.file = NULL,
                   n = NULL,
                   L = NULL)

  exp <- HGDP_PCA(s, save = TRUE)

}

#' Run of cross validation
#'
#' @export
long_HGDB_CrossVal <- function() {
  cl <- parallel::makeCluster(7)
  doParallel::registerDoParallel(cl)


  G.file <- "~/Projects/Data2016_2017/Hgdp_Li/Hgdp_Li.rds"
  X.file <- "~/Projects/Data2016_2017/Hgdp_Li/X_tmp.rds"

  s <- TrueSampler(G.file = G.file,
                   X.file = X.file,
                   outlier.file = NULL,
                   n = NULL,
                   L = NULL)

  lambdas <- c(1e-10, 1e0,1e2, 1e3, 1e4, 1e5, 1e30)
  kfold <- 5
  ## K = 5
  exp <- HGDB_crossvalidation(s, K = 5, kfold = kfold, lambdas = lambdas, save = TRUE)

  ## K = 20
  exp <- HGDB_crossvalidation(s, K = 20, kfold = kfold, lambdas = lambdas, save = TRUE)
}

#' Run of lfmm
#'
#' @export
long_HGDB_lfmm <- function(bypass = FALSE,
                           lambdas = c(1e-2),
                           Ks = c(20)) {

  KrakTest(bypass)

  G.file <- "~/Projects/Data2016_2017/Hgdp_Li/Hgdp_Li.rds"
  X.file <- "~/Projects/Data2016_2017/Hgdp_Li/X_tmp.rds"

  s <- TrueSampler(G.file = G.file,
                   X.file = X.file,
                   outlier.file = NULL,
                   n = NULL,
                   L = NULL)



  HGDB_runs(s, Ks = Ks, lambdas = lambdas, save = TRUE)
}
