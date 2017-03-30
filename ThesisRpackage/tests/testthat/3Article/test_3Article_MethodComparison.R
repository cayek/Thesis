library(testthat)
context("Article3_MethodComparison")

test_that("Article3_MethodComparison", {
  G.file <- "~/Projects/Thesis/Data/1000Genomes/Phase3Chrm22/European_Chrm22.maf.05.sample.10000.rds"
  skip_if_not(file.exists(G.file))

  exp <- Article3_MethodComparison(G.file = G.file,
                                   outlier.props = c(0.1),
                                   n = NULL,
                                   L = 1000,
                                   K = 4,
                                   cs = c(0.4),
                                   nb.rep = 1,
                                   fast.only = TRUE,
                                   cluster.nb = NULL,
                                   save = FALSE, bypass = TRUE)

  expect_equal(dim(exp$df), c(8000, 13))

  Article3_MethodComparison_plot_pvalueGrid(exp, c = 0.4)
  Article3_MethodComparison_plot_precisionRecall(exp)

})
