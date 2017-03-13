#' main experiment
#'
#' @export
GSE42861_experiment <- function(s, save = TRUE) {

  # glm
  glm <- Method(name = "glm",
                hypothesis.testing.method = phenotypeWayReg_glm_score(family = binomial,
                                                                      factorized.X1 = TRUE),
                impute.genotype.method = imputeByMean(),
                nickname = "glm")

  # glm + 2 PCs
  glm_2PC <- PCAClassicLinearMethod(K = 2,
                                    center = TRUE,
                                    hypothesis.testing.method = phenotypeWayReg_glm_score(family = binomial,
                                                                                          factorized.X1 = TRUE),
                                    nickname = "glm+2PCs",
                                    assumingStructure = FALSE)


  # glm + 6 pcs
  glm_6PC <- PCAClassicLinearMethod(K = 6,
                                    center = TRUE,
                                    hypothesis.testing.method = phenotypeWayReg_glm_score(family = binomial,
                                                                                          factorized.X1 = TRUE),
                                    nickname = "glm+6PCs",
                                    assumingStructure = FALSE)
  # glm + 6 refactor
  glm_6refractor <- refractorMethod(K = 6,
                                    verbose = FALSE,
                                    t = 500,
                                    nickname = "glm+6refractor")
  # glm + 6 lfmm ridge
  glm_6lfmm.ridge <- RidgeLFMMMethod(K = 6,
                                     hypothesis.testing.method = phenotypeWayReg_glm_score(),
                                     lambda = 1e6,
                                     nickname = "glm+6lfmm")
  # lfmm ridge
  lfmm.ridge <- RidgeLFMMMethod(K = 6,
                                hypothesis.testing.method = lm_zscore(gif = FALSE),
                                lambda = 1e6,
                                nickname = "lfmm ridge")
  # run exp
  exp <- ComparisonExperiment(s,
                              glm,
                              glm_2PC,
                              glm_6PC,
                              glm_6refractor,
                              glm_6lfmm.ridge,
                              lfmm.ridge)
  exp <- runExperiment(exp)
  # save exp
  if (save) {
    dumpExperiment(exp)
  }
  exp
}

#' @export
GSE42861_plot <- function(exp) {
  # Rmk: i am only interested in pvalue1 and score1 other pavalue was not computed
  # with glm of lm

  # qqplot
  ggplot(exp$df.res %>% dplyr::filter(variable.name == "pvalue1")) +
    stat_qq(aes(sample = -log10(estimate)),
            distribution = stats::qexp, dparams = list(rate = log(10))) +
    geom_abline(slope = 1, intercept = 0) +
    facet_grid(method.name~.) +
    ggtitle("-log10(pvalue) qqplot")

}

#' @export
GSE42861_get_RahmaniLoci <- function() {
  table <- tabulizer::extract_tables("~/Projects/Biblio/org-ref-pdfs/SF_Rahmani_2016.pdf",
                                     pages = 19, method = "data.frame")[[1]]
  table
}

################################################################################
# Long running

#' Run of PCA
#'
#'
#' @export
long_GSE42861_PCA <- function() {
  library(Article3Package)
  G.file <- "~/Projects/Data2016_2017/GSE42861/betanormalized_metylationlvl.filtered.rds"
  X.file <- "~/Projects/Data2016_2017/GSE42861/X.rds"

  s <- TrueSampler(G.file = G.file,
                   X.file = X.file,
                   outlier.file = NULL,
                   n = NULL,
                   L = NULL)

  exp <- HGDP_PCA(s, save = TRUE)
}

#' Run of LFMM
#'
#'
#' @export
long_GSE42861_LFMM <- function() {
  cl <- parallel::makeCluster(2)
  doParallel::registerDoParallel(cl)

  library(Article3Package)
  G.file <- "~/Projects/Data2016_2017/GSE42861/betanormalized_metylationlvl.filtered.rds"
  X.file <- "~/Projects/Data2016_2017/GSE42861/X.rds"

  s <- TrueSampler(G.file = G.file,
                   X.file = X.file,
                   outlier.file = NULL,
                   n = NULL,
                   L = NULL)


  lambdas <- c(1e-10, 1e0, 1e2, 1e10)
  Ks <- c(1,6,8,20)
  HGDB_runs(s, Ks = Ks, lambdas = lambdas, save = TRUE)
}

#' Run of GSE42861_experiment
#'
#'
#' @export
long_GSE42861_exp <- function() {
  library(Article3Package)

  G.file <- "~/Projects/Data2016_2017/GSE42861/betanormalized_metylationlvl.rds"
  X.file <- "~/Projects/Data2016_2017/GSE42861/X.rds"

  s <- TrueSampler(G.file = G.file,
                   X.file = X.file,
                   outlier.file = NULL,
                   n = NULL,
                   L = NULL)


  cl <- parallel::makeCluster(6)
  doParallel::registerDoParallel(cl)
  exp <- GSE42861_experiment(s, save = TRUE)
  exp
}

#' cross validation
#'
#'
#' @export
long_GSE42861_CrossVal <- function(cluster.nb = NULL,
                                   K = 6,
                                   G.file = "~/Projects/Data2016_2017/GSE42861/betanormalized_metylationlvl.filtered.rds",
                                   X.file = "~/Projects/Data2016_2017/GSE42861/X.rds",
                                   lambdas = c(1e-10, 1e0, 1e2, 1e10),
                                   rep = 5,
                                   missing.prop = 0.5,
                                   save = TRUE,
                                   bypass = FALSE) {

  KrakTest(bypass)

  if (!is.null(cluster.nb)) {
    cl <- parallel::makeCluster(cluster.nb)
    doParallel::registerDoParallel(cl)
  }



  s <- TrueSampler(G.file = G.file,
                   X.file = X.file,
                   outlier.file = NULL,
                   n = NULL,
                   L = NULL)
  dat <- sampl(s)

  m <-  finalLfmmRdigeMethod(K = K,
                             lambda = NULL)

  description <- paste0("long_GSE42861_CrossVal with K=", K,
                        "and lambdas = ",paste(lambdas,collapse = '|'))


  exp <- Experiment(name = "long_GSE42861_CrossVal", description = description)
  exp$crossvalidation.res <-  crossvalidation_kfold_missingvalue(m = m,
                                                                 dat = dat,
                                                                 rep = rep,
                                                                 missing.prop = missing.prop,
                                                                 lambdas = lambdas)


  # save exp
  if (save) {
    dumpExperiment(exp)
  }
  exp
}

#' cross validation
#'
#'
#' @export
long_GSE42861_lfmm_glm <- function(K.lfmm = 6,
                                   K.refactor = 6,
                                   G.file = "~/Projects/Data2016_2017/GSE42861/betanormalized_metylationlvl.filtered.rds",
                                   X.file = "~/Projects/Data2016_2017/GSE42861/X.rds",
                                   lambda = 1e-10,
                                   save = TRUE,
                                   bypass = FALSE,
                                   refactor = FALSE) {

  KrakTest(bypass)

  s <- TrueSampler(G.file = G.file,
                   X.file = X.file,
                   outlier.file = NULL,
                   n = NULL,
                   L = NULL)
  dat <- sampl(s)

  G <- dat$G
  X <- dat$X

  ## other co.var correction
  dat$G <- G
  dat$X <- X[,-1]
  m.lm <- finalLm()
  m.lm <- fit(m.lm, dat)

  m.lfmm <-  finalLfmmRdigeMethod(K = K.lfmm,
                                  lambda = lambda)
  m.refactor <- finalRefactorMethod(K = K.refactor)
  description <- paste0("long_GSE42861_lfmm_glm with K=", K.lfmm,
                        "and lambdas = ", lambda)
  exp <- Experiment(name = "long_GSE42861_lfmm_glm", description = description)

  # run of the method
  dat$G <- m.lm$epsilon
  dat$X <- X[,1, drop = FALSE]
  exp$m.lfmm <- fit(m.lfmm, dat)
  exp$m.refactor <- fit(m.refactor, dat)

  # hypothesis testing
  glm.aux <- function(m, name) {
    glm.func <- phenotypeWayReg_glm_score(family = binomial,
                                          factorized.X1 = TRUE)
    glm.res <- glm.func$fun(m, dat)
    df <- tibble(index = 1:length(glm.res$score), method.name = name,
                 estimate = glm.res$score[1,], variable.name = "score")
    df <- tibble(index = 1:length(glm.res$pvalue), method.name = name,
                 estimate = glm.res$pvalue[1,], variable.name = "pvalue") %>%
      rbind(df)
    df
  }
  exp$df.res <- rbind(glm.aux(exp$m.refactor, "refactor"),
                      glm.aux(exp$m.lfmm, "lfmm"))

  # save exp
  if (save) {
    dumpExperiment(exp)
  }
  exp

}

