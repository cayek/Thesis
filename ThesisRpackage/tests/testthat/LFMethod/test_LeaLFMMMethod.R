library(testthat)
context("Lea lfmm method")

################################################################################
# LEA

test_that("lea lfmm method", {

  skip_if_not_installed("LEA")

  K = 2
  # sample data
  s <- RandomXSampler(NormalSampler(n = 10,
                                    L = 100,
                                    K = K,
                                    prop.outlier = 0.02,
                                    sigma = 0.2,
                                    c = 0.6,
                                    mean.B = 0.0,
                                    sd.mu = 1.0,
                                    mean.mu = 0.5), d = 1)
  dat <- sampl(s)

  # method
  m <- LeaLFMMMethod(K = K, verbose = TRUE)

  expect_equal(m$name, "LeaLFMMMethod")

  # test fit
  m <- fit(m, dat)
  # test run
  m <- run(m, dat)

  ## plot
  gplot_stat(m$score[1,],
             outlier = dat$outlier) +
    geom_histogram(aes(stat, fill = outlier, y = ..density..)) +
    stat_function(fun = dnorm)


  gplot_stat(m$pvalue[1,],
             outlier = dat$outlier) +
    geom_histogram(aes(stat, fill = outlier, y = ..density..))

})

test_that("lea lfmm method simu from true dataset", {

  skip("too long")
  skip_if_not_installed("LEA")
  G.file <- "~/Projects/Thesis/Data/1000Genomes/Phase3/European_Chrm22.maf.05.sample.10000.rds"
  skip_if_not(file.exists(G.file))
  s <- FromTrueSampler(G.file = G.file,
                       n = NULL,
                       L = 1000,
                       K = 4,
                       prop.outlier = 0.1,
                       rho = NULL,
                       cs = NULL,
                       round = FALSE)
    dat <- sampl(s)

    ## run lEA lfmm
    m <- finalLEAMethod(K = 4)
    m <- run(m, dat)

    gplot_stat(m$score[1,],
               outlier = dat$outlier) +
      geom_histogram(aes(stat, fill = outlier, y = ..density..)) +
      stat_function(fun = dnorm)
    gplot_stat(m$pvalue[1,],
               outlier = dat$outlier) +
      geom_histogram(aes(stat, fill = outlier, y = ..density..))
    gplot_stat(m$pvalue[1,],
               outlier = dat$outlier) +
      geom_point(aes(x = index, color = outlier, y = -log10(stat)))


})
