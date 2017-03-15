library(testthat)
library(Article3Package)
context("experiment on HGDP")

test_that("HGDP PCA test on sample", {
  G.file <- "~/Projects/Data2016_2017/Hgdp_Li/Hgdp_Li.sample.rds"
  skip_if_not(file.exists(G.file))

  X.file <- "~/Projects/Data2016_2017/Hgdp_Li/X_prec.rds"

  s <- TrueSampler(G.file = G.file,
                   X.file = X.file,
                   outlier.file = NULL,
                   n = 100,
                   L = 1000)

  exp <- HGDP_PCA(s, save = FALSE)
  expect_equal(exp$description, "PCAexperiment on Hgdp_Li.sample.rds and X_prec.rds")


  plot(exp) +
    scale_x_continuous(limits = c(1,10))

})

test_that("HGDP crossvalidation", {

  G.file <- "~/Projects/Data2016_2017/Hgdp_Li/Hgdp_Li.sample.rds"
  skip_if_not(file.exists(G.file))

  X.file <- "~/Projects/Data2016_2017/Hgdp_Li/X_prec.rds"

  s <- TrueSampler(G.file = G.file,
                   X.file = X.file,
                   outlier.file = NULL,
                   n = 100,
                   L = 1000)

  lambdas <- c(1e-10, 1e-5, 1e0,1e2, 1e3, 1e4, 1e5, 1e30)
  kfold <- 2
  exp <- HGDB_crossvalidation(s, K = 5, kfold = kfold, lambdas = lambdas, save = FALSE)
  expect_equal(nrow(exp$crossvalidation.res$errs), length(lambdas) * kfold)

  plot(exp, 'sumUXabscor')

})



test_that("HGDP runs", {

  G.file <- "~/Projects/Data2016_2017/Hgdp_Li/Hgdp_Li.sample.rds"
  skip_if_not(file.exists(G.file))

  X.file <- "~/Projects/Data2016_2017/Hgdp_Li/X_prec.rds"

  s <- TrueSampler(G.file = G.file,
                   X.file = X.file,
                   outlier.file = NULL,
                   n = NULL,
                   L = 1000)

  lambdas <- c(1e-10)
  Ks <- c(3)
  exp <- HGDB_runs(s, Ks = Ks, lambdas = lambdas, save = FALSE)
  loci.found <- HGDB_get_frichotLoci()

  HGDB_runs_plot(loci.found = loci.found, s = s, exp = exp, K = 3, lambda = 1e-10)

})
