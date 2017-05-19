library(testthat)
context("FromTrueSampler")

test_that("From true data set sample", {

  G.file <- "~/Projects/Thesis/Data/1000Genomes/Phase3/European_Chrm22.maf.05.sample.10000.rds"
  skip_if_not(file.exists(G.file))

  s <- FromTrueSampler(G.file = G.file,
                       n = 100,
                       L = 1000,
                       K = 4,
                       prop.outlier = 0.1,
                       rho = 0.9,
                       cs = c(0.5, 0),
                       round = FALSE)


  ## test load
  s <- Sampler_load(s)
  n <- nrow(s$G)
  L <- ncol(s$G)
  expect_equal(dim(s$one), c(n,1))
  expect_equal(dim(s$mu), c(1,L))
  expect_equal(dim(s$U), c(n,s$K))
  expect_equal(dim(s$V), c(L,s$K))
  expect_equal(dim(s$E), c(n, L))
  expect_equal(s$loaded, TRUE)

  ## simulate data
  dat <- sampl(s)
  expect_equal(dim(dat$G), c(s$n, s$L))
  expect_equal(dim(dat$U), c(s$n, s$K))
  expect_equal(dim(dat$V), c(s$L, s$K))
  expect_equal(dim(dat$X), c(s$n, 1))

  ## plot
  ## plot(dat) ## DEPRECATED
  hist(dat$X)
  hist(dat$G)

  ## plot the variance
  variance <- dat$G %>%
    purrr::array_branch(2) %>%
    purrr::map_dbl(var)
  gplot_stat(variance,
             outlier = dat$outlier) +
    geom_point(aes(x = index, color = outlier, y = stat))

  ## plot the data with lm
  m <- ClassicLinearMethod()
  m <- run(m, dat)
  gplot_stat(m$B[1,],
             m$score[1,],
             outlier = dat$outlier) +
    geom_point(aes(x = index, color = outlier, y = stat)) # RMK : you can have na in the score because of column without variation !
  ## gplot_confunding(dat, m, i = 1, j = 2, d = 1) ## DEPRECATED

})

test_that("From true data set sample, analyse with lfmm ridge", {

  G.file <- "~/Projects/Thesis/Data/1000Genomes/Phase3/European_Chrm22.maf.05.sample.10000.rds"
  skip_if_not(file.exists(G.file))

  s <- FromTrueSampler(G.file = G.file,
                       n = 100,
                       L = 1000,
                       K = 3,
                       prop.outlier = 0.1,
                       rho = NULL,
                       cs = c(0.5,0,0),
                       sd.V.rho = 10.0,
                       round = FALSE)
  dat <- sampl(s)

  # Ridge lfmm
  m <- RidgeLFMMMethod(K = 3,
                       lambda = 1e-5)
  m <- run(m, dat)
  gplot_stat(m$B[1,],
             m$score[1,],
             outlier = dat$outlier) +
    geom_point(aes(x = index, color = outlier, y = stat))
  ## gplot_confunding(dat, m, i = 1, j = 2, d = 1) ## DEPRECATED

})


test_that("test of pca.file parama", {

  skip("Play with file")

  G.file <- "~/Projects/Thesis/Data/1000Genomes/Phase3/European_Chrm22.maf.05.sample.10000.rds"
  pca.file <- "~/Projects/Data2016_2017/AthalianaGegMapLines/call_method_75/test_pca.rds"
  file.remove(pca.file)
  skip_if_not(file.exists(G.file))
  
  s <- FromTrueSampler(G.file = G.file,
                       pca.file = pca.file,
                       n = 100,
                       L = 1000,
                       K = 2,
                       prop.outlier = 0.1,
                       rho = 0.9,
                       cs = c(0.5, 0),
                       round = FALSE)
  expect_output(dat <- sampl(s),
                ".*Compute PCA.*")
  expect_output(dat <- sampl(s),
                ".*Reading pca.*")
 
  ## plot
  ## plot(dat) ## DEPRECATED
  hist(dat$X)
  hist(dat$G)
  ## plot the variance
  variance <- dat$G %>%
    purrr::array_branch(2) %>%
    purrr::map_dbl(var)
  gplot_stat(variance,
             outlier = dat$outlier) +
    geom_point(aes(x = index, color = outlier, y = stat))

  ## plot the data with lm
  m <- ClassicLinearMethod()
  m <- run(m, dat)
  gplot_stat(m$B[1,],
             m$score[1,],
             outlier = dat$outlier) +
    geom_point(aes(x = index, color = outlier, y = stat)) # RMK : you can have na in the score because of column without variation !
  ## gplot_confunding(dat, m, i = 1, j = 2, d = 1) ## DEPRECATED

})

test_that("From true data set sample with reference", {

  G.file <- "~/Projects/Thesis/Data/1000Genomes/Phase3/European_Chrm22.maf.05.sample.10000.rds"
  skip_if_not(file.exists(G.file))

  s <- FromTrueSampler(G.file = G.file,
                       n = 100,
                       L = 1000,
                       K = 4,
                       prop.outlier = 0.1,
                       rho = 0.9,
                       cs = c(0.5, 0),
                       round = FALSE,
                       reference = TRUE)


  ## test load
  s <- Sampler_load(s)
  n <- nrow(s$G)
  L <- ncol(s$G)
  expect_equal(dim(s$one), c(n,1))
  expect_equal(dim(s$mu), c(1,L))
  expect_equal(dim(s$U), c(n,s$K))
  expect_equal(dim(s$V), c(L,s$K))
  expect_equal(dim(s$E), c(n, L))
  expect_equal(s$loaded, TRUE)

  ## simulate data
  dat <- sampl(s)
  expect_equal(dim(dat$G), c(s$n, s$L))
  expect_equal(dim(dat$U), c(s$n, s$K))
  expect_equal(dim(dat$V), c(s$L, s$K))
  expect_equal(dim(dat$X), c(s$n, 1))

  ## plot
  ## plot(dat) ## DEPRECATED
  hist(dat$X)
  hist(dat$G)

  ## plot the variance
  variance <- dat$G %>%
    purrr::array_branch(2) %>%
    purrr::map_dbl(var)
  gplot_stat(variance,
             outlier = dat$outlier) +
    geom_point(aes(x = index, color = outlier, y = stat))

  ## plot the data with lm
  m <- ClassicLinearMethod()
  m <- run(m, dat)
  gplot_stat(m$B[1,],
             m$score[1,],
             outlier = dat$outlier) +
    geom_point(aes(x = index, color = outlier, y = stat)) # RMK : you can have na in the score because of column without variation !
  ## gplot_confunding(dat, m, i = 1, j = 2, d = 1) ## DEPRECATED

})
