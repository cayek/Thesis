library(testthat)
context("Article3_MethodComparison")

futile.logger::flog.threshold(futile.logger::ERROR, name = "ThesisRpackage")
futile.logger::flog.threshold(futile.logger::ERROR, name = "console")

test_that("Article3_MethodComparison", {
  G.file <- "~/Projects/Thesis/Data/1000Genomes/Phase3/European_Chrm22.maf.05.sample.10000.rds"
  skip_if_not(file.exists(G.file))

  exp <- Article3_MethodComparison(G.file = G.file,
                                   outlier.props = c(0.1),
                                   n = NULL,
                                   L = 500,
                                   K = 2,
                                   cs = c(0.6),
                                   nb.rep = 2,
                                   fast.only = TRUE,
                                   cluster.nb = NULL,
                                   save = FALSE, bypass = TRUE)

  expect_equal(dim(exp$df), c(8000, 13))

  Article3_MethodComparison_plot_pvalueGrid(exp, c = 0.6)
  Article3_MethodComparison_plot_precisionRecall(exp)

  ## AUC
  Article3_MethodComparison_plot_AUC(exp)
  Article3_MethodComparison_plot_GIF(exp)
})
