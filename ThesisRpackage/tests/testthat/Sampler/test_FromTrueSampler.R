library(testthat)
context("FromTrueSampler")

test_that("From true data set sample, Arabidopsis Thaliana", {

  G.file <- "~/Projects/Data2016_2017/AthalianaGegMapLines/call_method_75/call_method_75_TAIR9.G.rds"
  skip_if_not(file.exists(G.file))

  s <- FromTrueSampler(G.file = G.file,
                       n = 100,
                       L = 1000,
                       K = 2,
                       prop.outlier = 0.1,
                       rho = 0.9,
                       cs = c(0.5, 0),
                       round = FALSE)
  dat <- sampl(s)
  ## plot
  ## plot(dat) ## DEPRECATED
  hist(dat$X)
  hist(dat$G)
  # plot the variance
  variance <- dat$G %>%
    purrr::array_branch(2) %>%
    purrr::map_dbl(var)
  gplot_stat(variance,
             outlier = dat$outlier) +
    geom_point(aes(x = index, color = outlier, y = stat))

  # plot the data with lm
  m <- ClassicLinearMethod()
  m <- run(m, dat)
  gplot_stat(m$B[1,],
             m$score[1,],
             outlier = dat$outlier) +
    geom_point(aes(x = index, color = outlier, y = stat)) # RMK : you can have na in the score because of column without variation !
  ## gplot_confunding(dat, m, i = 1, j = 2, d = 1) ## DEPRECATED

})

test_that("From true data set sample, Arabidopsis Thaliana and analyse with lfmm ridge", {

  G.file = "~/Projects/Data2016_2017/AthalianaGegMapLines/call_method_75/call_method_75_TAIR9.G.rds"
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

test_that("From true data set sample, Arabidopsis Thaliana experiment", {

  skip("Run and see plot ;D")

  G.file = "~/Projects/Data2016_2017/AthalianaGegMapLines/call_method_75/call_method_75_TAIR9.G.rds"
  K = 3
  s <- FromTrueSampler(G.file = G.file,
                       n = 200,
                       L = 1000,
                       K = K,
                       prop.outlier = 0.2,
                       rho = NULL,
                       cs = c(0.6,0,0),
                       round = FALSE)
  dat <- sampl(s)

  lfmm.ridge <- RidgeLFMMMethod(K = K ,
                                lambda = 1,
                                nickname = "ridge")
  lfmm.lasso <- LassoLFMMMethod(K = K ,
                                it.max = 100,
                                err.max = 1e-8,
                                nickname = "lasso")
  lfmm.lea <- LeaLFMMMethod(K = K, verbose = TRUE,
                            nickname = "lea")
  lm <- ClassicLinearMethod(nickname = "lm")
  lm.pca <- PCAClassicLinearMethod(K = K,nickname = "pca+lm")
  exp <- FDRControlExperiment(1, s,
                              lfmm.ridge,
                              #lfmm.lea,
                              lfmm.lasso,
                              lm.pca,
                              lm)
  exp <- runExperiment(exp)
  p <- plot(exp, plot.type = "pvalue.grid", summary_bin = TRUE)
  print(p)
  p <- plot(exp, plot.type = "precision.recall", summary_bin = TRUE, geom = "line")
  print(p)

})


test_that("From true data set sample, Arabidopsis Thaliana, test with rho = NULL", {

  G.file <- "~/Projects/Data2016_2017/AthalianaGegMapLines/call_method_75/call_method_75_TAIR9.G.rds"
  skip_if_not(file.exists(G.file))

  s <- FromTrueSampler(G.file = G.file,
                       n = 100,
                       L = 1000,
                       K = 2,
                       prop.outlier = 0.1,
                       rho = NULL,
                       cs = c(0.5, 0),
                       round = FALSE)
  dat <- sampl(s)

  ## plot
  ## plot(dat) ## DEPRECATED
  hist(dat$X)
  hist(dat$G)
  # plot the variance
  variance <- dat$G %>%
    purrr::array_branch(2) %>%
    purrr::map_dbl(var)
  gplot_stat(variance,
             outlier = dat$outlier) +
    geom_point(aes(x = index, color = outlier, y = stat)) # variance of outlier more important.

  # plot the data with lm
  m <- ClassicLinearMethod()
  m <- run(m, dat)
  gplot_stat(m$B[1,],
             m$score[1,],
             outlier = dat$outlier) +
    geom_point(aes(x = index, color = outlier, y = stat)) # RMK : you can have na in the score because of column without variation !
  ## gplot_confunding(dat, m, i = 1, j = 2, d = 1) ## DEPRECATED

})

test_that("test of pca.file parama", {

  skip("Play with file")

  G.file <- "~/Projects/Data2016_2017/AthalianaGegMapLines/call_method_75/call_method_75_TAIR9.G.rds"
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
  # plot the variance
  variance <- dat$G %>%
    purrr::array_branch(2) %>%
    purrr::map_dbl(var)
  gplot_stat(variance,
             outlier = dat$outlier) +
    geom_point(aes(x = index, color = outlier, y = stat))

  # plot the data with lm
  m <- ClassicLinearMethod()
  m <- run(m, dat)
  gplot_stat(m$B[1,],
             m$score[1,],
             outlier = dat$outlier) +
    geom_point(aes(x = index, color = outlier, y = stat)) # RMK : you can have na in the score because of column without variation !
  ## gplot_confunding(dat, m, i = 1, j = 2, d = 1) ## DEPRECATED

})
