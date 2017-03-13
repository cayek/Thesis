library(testthat)
context("gwas riz test")

test_that("long_gwas_riz", {

  G.file <- "../Data/gwas_riz/climate/genotype.rds"
  X.file <- "../Data/gwas_riz/climate/tmaxPC2.rds"
  skip_if_not(!file.exists(G.file))

  exp <- long_gwas_riz(n = 100,
                       L = 1000,
                       K = c(3),
                       lambdas = c(1e-10),
                       cluster.nb = NULL,
                       save = FALSE, bypass = TRUE)

})
