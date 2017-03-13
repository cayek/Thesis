library(testthat)
library(Article3Package)
context("phenotype way lm_score")

test_that("test on refractor demo dataset", {
  G.file <- "~/Projects/Data2016_2017/refractorDemo/betanormalized_metylationlvl.rds"
  X.file <- "~/Projects/Data2016_2017/refractorDemo/phenotype.rds"

  skip_if_not(file.exists(G.file))

  K <- 5
  s <- TrueSampler(G.file = G.file,
                   X.file = X.file,
                   outlier.file = NULL,
                   n = 100,
                   L = 1000)

  dat <- sampl(s)

  m.ridge <- RidgeLFMMMethod(K = K,
                             hypothesis.testing.method = phenotypeWayReg_glm_score())
  m.ridge <- run(m.ridge, dat)

  gplot_stat(m.ridge$B[1,],
             outlier = dat$outlier) +
    geom_point(aes(x = index, y = stat, color = outlier))
})



test_that("test on refractor GSE...", {
  G.file <- "~/Projects/Data2016_2017/GSE42861/betanormalized_metylationlvl.sample.rds"
  X.file <- "~/Projects/Data2016_2017/GSE42861/X.sample.rds"

  skip_if_not(file.exists(G.file))

  K <- 2
  s <- TrueSampler(G.file = G.file,
                   X.file = X.file,
                   outlier.file = NULL,
                   n = 100,
                   L = 1000)

  dat <- sampl(s)

  m.ridge <- RidgeLFMMMethod(K = K,
                             hypothesis.testing.method =
                               phenotypeWayReg_glm_score(family = binomial,
                                                         factorized.X1 = TRUE))
  m.ridge <- run(m.ridge, dat)

  gplot_stat(m.ridge$B[1,],
             outlier = dat$outlier) +
    geom_point(aes(x = index, y = stat, color = outlier))
})
