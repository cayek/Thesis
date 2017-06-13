library(testthat)
context("From true data 2")

test_that("FromTrueSampler2", {

  G.file <- "~/Projects/Thesis/Data/1000Genomes/Phase3/European_Chrm22.maf.05.sample.10000.rds"
  pca.file <- "~/Projects/Thesis/Data/1000Genomes/Phase3/European_Chrm22.maf.05.sample.10000_PCA.rds"
  skip_if_not(file.exists(G.file))


  cs = c(0.4, 0.2, -0.1, 0.0)
  s <- FromTrueSampler2(G.file = G.file,
                        K = 4,
                        prop.outlier = 0.1,
                        cs = cs,
                        pca.file = pca.file,
                        rho.B = 1.0)
  dat <- sampl(s)

  expect_lte(abs(cor(dat$X, dat$U[,1]) - cs[1]), 1e-1)
  expect_lte(abs(cor(dat$X, dat$U[,2]) - cs[2]), 1e-1)
  expect_lte(abs(cor(dat$X, dat$U[,3]) - cs[3]), 1e-1)
  expect_lte(abs(cor(dat$X, dat$U[,4]) - cs[4]), 1e-1)

  ## same covariance in simulated data and true data?
  skip("run it manually, plot the matrix")
  s <- Sampler_load(s)
  cov(s$U)
  cov(dat$U) ## quite same covariance matrix
  
})

test_that("FromTrueSampler2 n and L param", {

  G.file <- "~/Projects/Thesis/Data/1000Genomes/Phase3/European_Chrm22.maf.05.sample.10000.rds"
  pca.file <- "~/Projects/Thesis/Data/1000Genomes/Phase3/European_Chrm22.maf.05.sample.10000_PCA.rds"
  skip_if_not(file.exists(G.file))


  cs = c(0.4, 0.2, -0.1, 0.0)
  s <- FromTrueSampler2(G.file = G.file,
                        K = 4,
                        prop.outlier = 0.1,
                        cs = cs,
                        pca.file = pca.file,
                        rho.B = 1.0,
                        n = NULL,
                        L = 1000)
  dat <- sampl(s)

  expect_equal(dim(dat$G), c(503, 1000))
  expect_lte(abs(cor(dat$X, dat$U[,1]) - cs[1]), 1e-1)
  expect_lte(abs(cor(dat$X, dat$U[,2]) - cs[2]), 1e-1)
  expect_lte(abs(cor(dat$X, dat$U[,3]) - cs[3]), 1e-1)
  expect_lte(abs(cor(dat$X, dat$U[,4]) - cs[4]), 1e-1)

  ## same covariance in simulated data and true data?
  skip("run it manually, plot the matrix")
  s <- Sampler_load(s)
  cov(s$U)
  cov(dat$U) ## quite same covariance matrix
  
})

