library(testthat)
context("2Article_1000Genome")

test_that("2Article_1000Genome", {

  dat.file <- "~/Projects/Thesis/Data/1000Genomes/Phase3/Eu_Af_Afam.maf.05.sample.rds"
  skip_if_not(file.exists(dat.file))

  skip("too long")

  exp <- Article2_1000Genome(dat.file,
                             K = 3,
                             openMP.core.num = 4,
                             save = FALSE)


  expect_equal(exp$description, "run of tess3r with dat.file=~/Projects/Thesis/Data/1000Genomes/Phase3Chrm22/Eu_Af_Afam.maf.05.sample.rds K=3 ")
  expect_equal(exp$name, "Article2_1000Genome")
  expect_lt(tess3r::ComputeRmseWithBestPermutation(exp$tess3r$Q,
                                                      exp$snmf.method$Q), 0.07)

  barplot(exp$tess3r$Q, border = NA)
  barplot(exp$snmf.method$Q, border = NA)
})
