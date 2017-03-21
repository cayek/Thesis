library(testthat)
context("dataReaders")

test_that("read_vcf", {

  f <- "~/Projects/Thesis/Data/1000Genomes/Phase3Chrm22/ALL.chr1.phase3_shapeit2_mvncall_integrated_v5a.20130502.genotypes.vcf"
  skip_if_not(file.exists(f))

  dat <- read_vcf(f,
                   maf.threshold = 0.05,
                   n_max = 60,
                   block.size = 20)

  expect_equal(dim(dat$G), c(2504, 12))
  expect_equal(mean(unique(as.vector(dat$G)) %in% c(0,1,2)), 1)
  expect_equal(length(colnames(dat$G)), 12)
  expect_equal(length(rownames(dat$G)), 2504)
  expect_equal(dim(dat$snps.info), c(12, 9))
  expect_equal(dat$snps.info$ID, colnames(dat$G))

}

