library(testthat)
context("2Article_1000Genome")

test_that("2Article_1000Genome", {

  s <- Tess3Sampler(n = 50,
                    nsites.neutral = 100)
  s$G.file <- ""## just for the exp$description

  set.seed(545587)
  exp <- Article2_1000Genome(s = s,
                             K = 2,
                             cluster.nb = NULL,
                             save = FALSE,
                             bypass = TRUE)
  expect_equal(exp$description, "run of tess3r with dat.file= K=2 ")
  expect_equal(exp$name, "Article2_1000Genome")
  expect_lt(tess3r::ComputeRmseWithBestPermutation(exp$tess3R.method$Q,
                                                      exp$snmf.method$Q), 0.2)


})
