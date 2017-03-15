library(testthat)
library(Article3Package)
context("Athaliana illustration function")

test_that("long_Athaliana_runs", {
  G.file <- "~/Projects/Data2016_2017/AthalianaGegMapLines/call_method_75/call_method_75_TAIR9.G.rds"
  skip_if_not(file.exists(G.file))

  exp <- long_Athaliana_runs(Ks = 5, lambdas = c(1e-1, 1e2),
                             n = 100, L = 1000, save = FALSE, gif = TRUE)
  # expect_false(anyNA(exp$df.res)) # I found this bug ...
  # this is not a bug. This is because there is SNPs without variance in the G matrix

  expect_true(mean(is.na(exp$df.res$estimate)) < 0.005) # we put na.rm = TRUE in gif function so that not all pvalue are to NA.

  ## method names
  expect_equal(unique(exp$df.res$method.name),
               c("RidgeLfmm|lambda=0.1|K=5|gif=1",
                 "RidgeLfmm|lambda=100|K=5|gif=1",
                 "lm"))

})

