library(testthat)
context("Article3_ValidationNumerique")


test_that("Article3_ValidationNumerique_Sample", {

  skip("Only to sample for test")

  file.res <- Article3_ValidationNumerique_Sample(L = 1000,
                                                  only.EUR = TRUE,
                                                  dat.file = "~/Projects/Thesis/Data/1000Genomes/Phase3/Eu_Af_Afam.maf.05.sample.rds")

  ## test and PCA
  G <- readRDS(file.res)
  dim(G)
  anyNA(G)


  ## PCA
  svd.res <- svd(G, nu = 0, nv = 0)
  vars <- svd.res$d / sum(svd.res$d)
  ## plot
  pl <- qplot(x = seq_along(vars), y = vars, geom='line') +
    geom_point() +
    coord_cartesian(xlim = c(1,100))
  pl

})

test_that("run of main exp", {

  skip("run it")
  G.file <- "~/Projects/Thesis/Data/ThesisDataset/3Article/1000GenomesPhase3/ValidationNumerique_EU_L1000.G.rds"
  skip_if_not(file.exists(G.file))

  exp <- Article3_MethodComparison(G.file = G.file,
                                   outlier.props = c(0.005),
                                   n = NULL,
                                   L = 1000,
                                   K = 2,
                                   K.method = 2,
                                   correctionByC = FALSE,
                                   cs = c(0.1),
                                   cs.sum = TRUE,
                                   sd.V.rho = 1, ## BUG of > 1 because lfmm lasso find K = 0
                                   fast.only = TRUE,
                                   cluster.nb = NULL,
                                   save = FALSE,
                                   bypass = TRUE
                                   )

  Article3_MethodComparison_plot_relative_diff_AUC(exp)
})

test_that("Play with experiment", {

  skip("run and play on krak")

  library(ThesisRpackage)
  G.file <- "~/Projects/Thesis/Data/ThesisDataset/3Article/1000GenomesPhase3/ValidationNumerique_EU_L5e+05.G.rds"

  ## methods
  methods <- finalBench(2, 1e-5, TRUE, 0.01, TRUE)
  methods$famt <- NULL
  methods$lfmmRidge <- NULL
  methods$sva <- NULL

  exp <- Article3_MethodComparison(G.file,
                                   outlier.props = 0.001,
                                   n = 100, L = 10000,
                                   K = 2,
                                   K.method = 2,
                                   cs = c(0.8),
                                   cs.sum = FALSE,
                                   sd.V.rho = 1, 
                                   nb.rep = 4,
                                   fast.only = TRUE,
                                   cluster.nb = 4,
                                   save = FALSE, bypass = TRUE,
                                   methods = methods)

  Article3_MethodComparison_plot_relative_diff_AUC(exp)
  Article3_MethodComparison_plot_precisionRecall(exp)
  Article3_MethodComparison_plot_AUC(exp)
  Article3_MethodComparison_plot_GIF(exp)

 })
