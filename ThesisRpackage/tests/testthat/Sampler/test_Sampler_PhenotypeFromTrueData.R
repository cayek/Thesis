library(testthat)
context("Sampler_PhenotypeFromTrueData")

test_that("PhenotypeFromTrueSampler", {
  skip("Play with files...")
  G.file <- "~/Projects/Thesis/Data/1001Genomes/1001_SNP_MATRIX/G_OF_filtered.sample.rds"
  skip_if_not(file.exists(G.file))
  env.file <- "~/Projects/Thesis/Data/1001Genomes/1001_SNP_MATRIX/G_OF_filtered.env.rds"
  pca.file <- "~/Projects/Thesis/Data/1001Genomes/1001_SNP_MATRIX/G_OF_filtered.sample.pca.rds"
  coord.file <- "~/Projects/Thesis/Data/1001Genomes/1001_SNP_MATRIX/G_OF_filtered.coord.rds"
  s <- PhenotypeFromTrueSampler(G.file,
                                coord.file,
                                env.file,
                                pca.file,
                                n = NULL,
                                L = NULL,
                                K = 40,
                                J = 10,
                                beta = 6,
                                delta = 0.3)
  flog.threshold(9)
  dat <- sampl(s)

  ## test output
  expect_equal(length(dat$outlier), 10)
  expect_equal(dim(dat$X), c(nrow(dat$G), 1))

  ## associations
  cor(dat$X, dat$G[,dat$outlier])
  r = as.numeric( cor(dat$X, dat$G))
  sum(dat$outlier %in% which(r > 0.3) ) ## number of detected loci (puissance)
  mean(!(dat$outlier %in% which(r > 0.3))) ## fdr

})
