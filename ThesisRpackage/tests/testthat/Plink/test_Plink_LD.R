library(testthat)
context("plink LD")


test_that("Plink_LD_clumping", {

  skip('to be continued')
  prefix.in <- "~/Projects/Thesis/Data/Celiac/dubois_2010/FinnuncorrNLITUK1UK3hap300_sample"
  skip_if_not(file.exists(paste0(prefix, ".bed")))

  Plink_LD_clumping(prefix.in = prefix,
                    )

}

