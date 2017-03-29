library(testthat)
context("Article3_MethodComparison")

test_that("Article3_MethodComparison", {
  G.file <- "~/Projects/Thesis/Data/1000Genomes/Phase3Chrm22/European_Chrm22.maf.05.sample.10000.rds"
  skip_if_not(file.exists(G.file))

  exp <- Article3_MethodComparison(G.file = G.file,
                                   outlier.props = c(0.05, 0.1),
                                   n = NULL,
                                   L = 1000,
                                   K = 4,
                                   cs = function() runif(1, 0.3, 0.6), # choose at random
                                   nb.rep = 1,
                                   fast.only = TRUE,
                                   cluster.nb = NULL,
                                   save = TRUE, bypass = TRUE)

  Article3_MethodComparison_plot(exp, plot.type = "pvalue.grid")
  Article3_MethodComparison_plot(exp, plot.type = "precision.recall")

})
