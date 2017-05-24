library(testthat)
context("Article3_Celiac")

flog.threshold(futile.logger::ERROR, name = "ThesisRpackage")
flog.threshold(futile.logger::ERROR, name = "console")

test_that("Article3_Celiac_lm", {

  skip("DEPRECATED")

  m <- finalLfmmRdigeMethod(K = 6,
                            1e-1)

  dat <- NormalSampler(50, 500, 5) %>%
    sampl()
  dat$G <- scale(dat$G)


  ## run
  m1 <- run(m, dat)

  ## fit
  m <- fit(m, dat)

  ## lm
  m2 <- Article3_Celiac_lm(m, dat)

  ## plot
  gplot_stat(m1$pvalue[1,],
             m2$pvalue[1,],
             outlier = dat$outlier) +
    geom_point(aes(x = index, y = -log10(stat), color = outlier))
  ## plot must be same !!
})

