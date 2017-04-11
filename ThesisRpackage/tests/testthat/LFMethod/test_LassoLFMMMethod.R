library(testthat)
context("lasso lfmm method")


test_that("lasso lfmm on binomial dataset with outlier", {

  # sample data
  K <- 5
  s <- LogisticSampler(n = 100,
                       L = 1000,
                       K = K,
                       prop.outlier = 0.2,
                       sigma = 0.2,
                       c = 0.6,
                       mean.B = 0.0,
                       sd.mu = 1.0,
                       mean.mu = 0.5)
  dat <- sampl(s)

  m <- LassoLFMMMethod(K = K + 2,
                       lambda = 1e-2,
                       it.max = 100,
                       err.max = 1e-6)
  m <- run(m, dat)

  gplot_stat(m$B[1,], dat$B[1,], outlier = dat$outlier) +
    geom_point(aes(x = index, y = stat, color = outlier))

})

################################################################################
# regularization path

test_that("lasso Regularization path lfmm on binomial dataset with outlier", {

  # sample data
  K <- 5
  s <- LogisticSampler(n = 100,
                       L = 1000,
                       K = K,
                       prop.outlier = 0.2,
                       sigma = 0.2,
                       c = 0.6,
                       mean.B = 0.0,
                       sd.mu = 1.0,
                       mean.mu = 0.5)
  dat <- sampl(s)

  m <- LassoLFMMMethod(K = K + 2, # ;D
                       lambda = NULL,
                       lambda.K = 2,
                       lambda.eps = 0.001,
                       it.max = 100,
                       err.max = 1e-6)
  # m <- run(m, dat)
  # gplot_stat(m$score[1,], dat$B[1,], outlier = dat$outlier) +   geom_point(aes(x = index, y = stat, color = outlier))

  # FdrControl exp
  exp <- FDRControlExperiment(nb.rep = 1, s = s,
                              m)
  exp <- runExperiment(exp)
  p <- plot(exp, plot.type = "precision.recall", summary_bin = TRUE, geom = "point")
  print(p)
})

################################################################################
# sparse proportion

test_that("Test of sparse proportion feature", {

  set.seed(3547434)
  # sample data
  K <- 5
  prop <- 0.1
  s <- LogisticSampler(n = 100,
                       L = 1000,
                       K = K,
                       prop.outlier = prop,
                       c = 0.6,
                       mean.B = 0.0,
                       sd.mu = 1.0,
                       mean.mu = 0.5)
  dat <- sampl(s)

  m <- LassoLFMMMethod(K = K,
                       sparse.prop = prop,
                       lambda = NULL,
                       lambda.K = 100,
                       lambda.eps = 0.001,
                       it.max = 100,
                       err.max = 1e-6)

  m <- run(m, dat)

  expect_lte(abs(mean(m$B != 0.0) - prop), 0.025)
  gplot_stat(m$B[1,], dat$B[1,], outlier = dat$outlier) +
    geom_point(aes(x = index, y = stat, color = outlier))

})

################################################################################
# soft

test_that("Test of soft feature", {

  set.seed(55785)
  # sample data
  K <- 5
  prop <- 0.1
  s <- LogisticSampler(n = 100,
                       L = 1000,
                       K = K,
                       prop.outlier = prop,
                       c = 0.6,
                       mean.B = 0.0,
                       sd.mu = 1.0,
                       mean.mu = 0.5)
  dat <- sampl(s)

  m <- LassoLFMMMethod(K = K,
                       sparse.prop = 0.10,
                       lambda = NULL,
                       lambda.K = 50,
                       lambda.eps = 0.002,
                       it.max = 100,
                       err.max = 1e-6,
                       soft = TRUE)
  m <- run(m, dat)

  expect_lte(abs(mean(m$B != 0.0) - prop), 0.042)
  gplot_stat(m$B[1,], dat$B[1,], outlier = dat$outlier) +
    geom_point(aes(x = index, y = stat, color = outlier))

})

################################################################################
# with missing values


test_that("Test of lasso with missing values", {

  set.seed(34557)
  # sample data
  K <- 5
  prop <- 0.1
  s <- LogisticSampler(n = 100,
                       L = 1000,
                       K = K,
                       prop.outlier = prop,
                       c = 0.6,
                       mean.B = 0.0,
                       sd.mu = 1.0,
                       mean.mu = 0.5) %>%
    SparseMissingValueSampler(missing.prop = 0.1, missing.prop.by.loci = 0.5)
  dat <- sampl(s)

  m <- LassoLFMMMethod(K = K,
                       sparse.prop = 0.10,
                       lambda = 1e-1,
                       lambda.K = 50,
                       lambda.eps = 0.002,
                       it.max = 100,
                       err.max = 1e-6,
                       soft = TRUE)
  m <- run(m, dat)

  expect_lte(abs(mean(m$B != 0.0) - prop), 0.01)
  gplot_stat(m$B[1,], dat$B[1,], outlier = dat$outlier) +
    geom_point(aes(x = index, y = stat, color = outlier))

})

################################################################################
# Lasso_LambdaGammaRange

test_that("Lasso_LambdaRange", {

  K <- 5
  prop <- 0.1
  s <- LogisticSampler(n = 100,
                       L = 1000,
                       K = K,
                       prop.outlier = prop,
                       c = 0.6,
                       mean.B = 0.0,
                       sd.mu = 1.0,
                       mean.mu = 0.5)
  dat <- sampl(s)

  res <- Lasso_LambdaRange(dat, K = K)
  expect_equal(length(res), 100)

})


################################################################################
# Lasso_HeuristicGammaLambda

test_that("Lasso_HeuristicGammaLambda", {

  K <- 5
  prop <- 0.1
  s <- LogisticSampler(n = 100,
                       L = 1000,
                       K = K,
                       prop.outlier = prop,
                       c = 0.6,
                       mean.B = 0.0,
                       sd.mu = 1.0,
                       mean.mu = 0.5)
  dat <- sampl(s)

  gamma.lambda.heuristic <- Lasso_HeuristicGammaLambda(dat, K, prop, lambda.lfmm = 1e-10)
  gamma.lambda.heuristic$lambda
  gamma.lambda.heuristic$gamma

  m <- LassoLFMMMethod(K = 5,
                       gamma = NULL,
                       lambda = NULL,
                       lambda.K = NULL,
                       lambda.eps = NULL,
                       sparse.prop = 0.1,
                       it.max = 200,
                       err.max = 1e-6,
                       soft = TRUE)
  m <- fit(m, dat) ## heuristic do not work well... but let's try that

})

################################################################################
# SSMPG dataset

test_that("lasso lfmm on case3", {

  skip_if_not("run it manualy")

  K <- 3
  s <- TrueSampler(G.file = "~/Projects/Thesis/Data/SSMPG2015/Case3/Case3.lfmm",
                   X.file = "~/Projects/Thesis/Data/SSMPG2015/Case3/Case3.env",
                   outlier.file = "~/Projects/Thesis/Data/SSMPG2015/Case3/Case3.outlier")
  dat <- sampl(s)

  # lambda = 0.0
  m <- LassoLFMMMethod(K = K,
                       lambda = 1e-2,
                       it.max = 10,
                       err.max = 1e-6)
  m <- run(m, dat)

  gplot_stat(m$B[1,], outlier = dat$outlier) +
    geom_point(aes(x = index, y = stat, color = outlier))

})


test_that("lasso lfmm on case2", {

  skip_if_not("run it manualy")

  K <- 6
  s <- TrueSampler(G.file = "../../Data2016_2017/SSMPG2015/Case2/Case2.lfmm",
                   X.file = "../../Data2016_2017/SSMPG2015/Case2/Case2.env",
                   outlier.file = "../../Data2016_2017/SSMPG2015/Case2/Case2.outlier")
  dat <- sampl(s)

  # lambda = 0.0
  m <- LassoLFMMMethod(K = K,
                       lambda = 1e-2,
                       it.max = 10,
                       err.max = 1e-6)
  m <- run(m, dat)

  gplot_stat(m$B[1,], outlier = dat$outlier) +
    geom_point(aes(x = index, y = stat, color = outlier))

})



################################################################################
## Bug with FDRControlExperiment


test_that("lasso lfmm on case2", {
  G.file <- "~/Projects/Thesis/Data/1000Genomes/Phase3/European_Chrm22.maf.05.sample.10000.rds"
  skip_if_not(file.exists(G.file))
  K <- 2
  prop.outlier <- 0.05
  s <- FromTrueSampler(G.file = G.file,
                       n = NULL,
                       L = 200, ## IF L is too small alternated algo raise an error !!!
                       K = K,
                       prop.outlier = prop.outlier,
                       rho = NULL,
                       cs = 0.6,
                       round = FALSE)

  m <- finalLfmmLassoMethod(K = K,
                            sparse.prop = prop.outlier,
                            calibrate = FALSE)

  ## FDRControlExperiment
  exp <- FDRControlExperiment(nb.rep = 2, sampler = s, m)
  exp <- runExperiment(exp)
})
