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

