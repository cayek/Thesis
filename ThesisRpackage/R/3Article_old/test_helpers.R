

#' Plot and compare with lm,  ridge lfmm, pca+lm
#'
#' @export
plotAndCompare <- function(m, dat, K) {
  m.ridge <- RidgeLFMMMethod(K = K)
  m.ridge <- run(m.ridge, dat)
  m.lm <- ClassicLinearMethod()
  m.lm <- run(m.lm, dat)
  m.pca <- PCAClassicLinearMethod(K = K)
  m.pca <- run(m.pca, dat)
  gplot_stat(m$B[1,],
             m.ridge$B[1,],
             m.pca$B[1,],
             m.lm$B[1,],
             outlier = dat$outlier) +
    geom_point(aes(x = index, y = stat, color = outlier))
}

#' Test if the test is well calibrated
#'
#' @export
testCalibration <- function(m, s) {

  # compute dat and run method
  dat <- sampl(s)
  # m <- fit(m, dat)
  m <- run(m, dat)

  # plot hist of pvalue
  p <- gplot_stat(m$pvalue[1,],
                  outlier = dat$outlier) +
    geom_histogram(aes(stat, fill = outlier, y = ..density..), position = "dodge") +
    stat_function(fun = dunif) +
    ggtitle("Hist of pvalue")
  print(p)

  # plot of pvalue only on neutral
  p <- gplot_stat(m$score[1,],
                  outlier = dat$outlier) +
    geom_histogram(aes(stat, fill = outlier, y = ..density..), position = "dodge") +
    stat_function(fun = dnorm) +
    ggtitle("Hist of score")
  print(p)


  # experiment on fdr control
  m <- clean(m) # to forget former estimates
  exp <- FDRControlExperiment(nb.rep = 10, s = s,
                              m)
  exp <- runExperiment(exp)
  p <- plot(exp, plot.type = "pvalue.grid", summary_bin = TRUE)
  print(p)
  p <- plot(exp, plot.type = "qqplot")
  print(p)
}


#' Run lm, lfmmRidge and lm + pca on the sampler
#'
#'   @export
samplerTest <- function(s, K, lfmm.lambda) {
  m.ridge <- RidgeLFMMMethod(K = K, lambda = lfmm.lambda, nickname = "lfmmRidge")
  m.lm <- ClassicLinearMethod(nickname = "lm")
  m.pca <- PCAClassicLinearMethod(K = K, nickname = "PCA+lm")

  #### Estimated comparison
  cat("Estimation comparison \n")
  exp <- ComparisonExperiment(s = s, m.ridge, m.lm, m.pca)
  exp <- runExperiment(exp)
  p <- plot(exp, variable.name.regexp = "score|B") +
    facet_grid(variable.name~method.name, scales = "free")
  print(p)

  #### Hypothesis testing comparison
  cat("Hypothesis testing comparison \n")
  exp <- FDRControlExperiment(nb.rep = 1, s = s, m.ridge, m.lm, m.pca)
  exp <- runExperiment(exp)
  p <- plot(exp, plot.type = "pvalue.grid", summary_bin = TRUE)
  print(p)
  p <- plot(exp, plot.type = "precision.recall", summary_bin = TRUE, geom = "point")
  print(p)
  p <- plot(exp, plot.type = "qqplot")
  print(p)
}
