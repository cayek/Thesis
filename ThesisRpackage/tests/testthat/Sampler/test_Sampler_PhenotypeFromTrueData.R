library(testthat)
context("Sampler_PhenotypeFromTrueData")

test_that("PhenotypeFromTrueSampler", {
  G.file <- "~/Projects/Thesis/Data/1001Genomes/1001_SNP_MATRIX/G_OF_filtered.sample.rds"
  skip_if_not(file.exists(G.file))
  climate.file <- "~/Projects/Thesis/Data/1001Genomes/1001_SNP_MATRIX/G_OF_filtered.sample..climate.rds"
  pca.file <- "~/Projects/Thesis/Data/1001Genomes/1001_SNP_MATRIX/G_OF_filtered.sample.pca.rds"
  s <- PhenotypeFromTrueSampler(G.file,
                                climate.file,
                                pca.file,
                                n = NULL,
                                L = NULL,
                                K = 40,
                                J = 10,
                                beta = 6,
                                delta = 0.3)
  

}
