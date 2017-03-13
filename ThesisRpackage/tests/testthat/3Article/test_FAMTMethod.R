library(testthat)
library(Article3Package)
context("FAMT method")

################################################################################
# FAMT

test_that("FAMT method", {

  skip_if_not_installed("FAMT")

  K = 2
  # sample data
  s <- NormalSampler(n = 100,
                     L = 1000,
                     K = K,
                     prop.outlier = 0.02,
                     sigma = 0.2,
                     c = 0.6,
                     mean.B = 0.0,
                     sd.mu = 1.0,
                     mean.mu = 0.5)
  dat <- sampl(s)

  # method
  m <- FAMTMethod(K = K)

  expect_equal(name(m), "FAMTMethod|NULL")

  # test fit
  m <- fit(m, dat)
  # test run
  m <- run(m, dat)

  # plot
  gplot_stat(m$score[1,],
             outlier = dat$outlier) +
    geom_histogram(aes(stat, fill = outlier, y = ..density..))
  # RMK : the score is a F score

  gplot_stat(m$pvalue[1,],
             outlier = dat$outlier) +
    geom_histogram(aes(stat, fill = outlier, y = ..density..))

  gplot_stat(m$pvalue[1,],
             outlier = dat$outlier) +
    geom_point(aes(x = index, y = -log10(stat), color = outlier))

})

################################################################################
# on 1000 genome

test_that("FAMT method on 1000 genome", {

  G.file <- "~/Projects/Data2016_2017/1000Genomes/Phase3Chrm22/European_Chrm22.rds"
  skip_if_not(file.exists(G.file))
  cs <- c(0.6, 0.3, 0.0)
  K <- 3
  s <- FromTrueSampler(G.file = G.file,
                       n = NULL,
                       L = 1000,
                       K = K,
                       prop.outlier = 0.05,
                       rho = NULL,
                       cs = cs,
                       round = FALSE)
  dat <- sampl(s)
  m <- FAMTMethod(K = K)
  m <- run(m, dat)


})
