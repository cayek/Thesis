
#' @export
lfmmRidgeBoostrapAssumingV <- function(dat, s, K, boot.rep, exp.rep) {
  m1 <- Bootstrap(PairedBoostrap, boot.rep) %>%
    Zscore(B.sigma2.functor = .) %>%
    RidgeLFMMMethod(K = K,
                    lambda = 10,
                    hypothesis.testing.method = .,
                    nickname = "lfmmRidge boostrap",
                    reuse.V = TRUE)

  m2 <- AnalyticSigma2Functor() %>%
    Zscore(B.sigma2.functor = .) %>%
    RidgeLFMMMethod(K = K,
                    lambda = 10,
                    hypothesis.testing.method = .,
                    nickname = "lfmmRidge analytic sigma",
                    reuse.V = TRUE)

  #### run of method
  m2 <- run(m = m2, dat = dat)
  m1 <- run(m = m1, dat = dat)
  cat("Plot of score hist \n")
  gplot_stat(m1$score[1,], m2$score[1,],
             outlier = dat$outlier) +
    geom_histogram(aes(stat, fill = outlier, y = ..density..)) +
    stat_function(fun = dnorm)
  cat("Plot of B hist \n")
  gplot_stat(m1$B[1,], m2$B[1,],
             m1$B.sigma2[1,], m2$B.sigma2[1,],
             outlier = dat$outlier) +
    geom_point(aes(x = index, y = stat, color = outlier))


  #### Estimated comparison
  cat("Estimation comparison \n")
  exp <- ComparisonExperiment(s = s, m1, m2)
  exp <- runExperiment(exp)
  p <- plot(exp, variable.name.regexp = "score|B") +
    facet_grid(variable.name~method.name, scales = "free")
  print(p)

  #### Hypothesis testing comparison
  cat("Hypothesis testing comparison \n")
  exp <- FDRControlExperiment(nb.rep = exp.rep, s = s, m1, m2)
  exp <- runExperiment(exp)
  p <- plot(exp, plot.type = "pvalue.grid", summary_bin = TRUE)
  print(p)
  p <- plot(exp, plot.type = "precision.recall", summary_bin = TRUE, geom = "point")
  print(p)
  p <- plot(exp, plot.type = "qqplot")
  print(p)
}




#' @export
CompGenerativeModel <- function(s, method.K, boot.rep, exp.rep) {

  dat <- sampl(s)
  plot(dat)

  ## methods
  lm <- AnalyticSigma2Functor() %>%
    Zscore(B.sigma2.functor = .) %>%
    ClassicLinearMethod(hypothesis.testing.method = .,
                        nickname = "lm")
  lm <- run(m = lm, dat = dat)

  lmPca <- AnalyticSigma2Functor() %>%
    Zscore(B.sigma2.functor = .) %>%
    PCAClassicLinearMethod(K = method.K,
                           hypothesis.testing.method = .,
                           nickname = "PCA+lm")
  lmPca <- run(m = lmPca, dat = dat)

  lmPcaStructure <- AnalyticSigma2Functor() %>%
    Zscore(B.sigma2.functor = .) %>%
    PCAClassicLinearMethod(K = method.K,
                           hypothesis.testing.method = .,
                           nickname = "structure+lm",
                           assumingStructure = TRUE)
  lmPcaStructure <- run(m = lmPcaStructure, dat = dat)

  LfmmRidgeBootstrap <- Bootstrap(PairedBoostrap, boot.rep) %>%
    Zscore(B.sigma2.functor = .) %>%
    RidgeLFMMMethod(K = method.K,
                    lambda = 10,
                    hypothesis.testing.method = .,
                    nickname = "lfmmRidge boostrap",
                    reuse.V = TRUE)
  LfmmRidgeBootstrap <- run(m = LfmmRidgeBootstrap, dat = dat)

  LfmmRidgeAnalyticSigma <- AnalyticSigma2Functor() %>%
    Zscore(B.sigma2.functor = .) %>%
    RidgeLFMMMethod(K = method.K,
                    lambda = 10,
                    hypothesis.testing.method = .,
                    nickname = "lfmmRidge analytic sigma")
  LfmmRidgeAnalyticSigma <- run(m = LfmmRidgeAnalyticSigma, dat = dat)

  LeaLfmm <- LeaLFMMMethod(K = method.K,
                           nickname = "LeaLfmm")
  LeaLfmm <- run(m = LeaLfmm, dat = dat)

  famt <- FAMTMethod(K = method.K,
                           nickname = "famt")
  famt <- run(m = famt, dat = dat)
  sva <- SVAMethod(K = method.K,
                     nickname = "sva")
  sva <- run(m = sva, dat = dat)


  ## plot of scrore
  cat("Plot of score hist \n")
  p <- gplot_stat(LfmmRidgeBootstrap$score[1,],
             LfmmRidgeAnalyticSigma$score[1,],
             LeaLfmm$score[1,],
             lm$score[1,],
             lmPca$score[1,],
             lmPcaStructure$score[1,],
             outlier = dat$outlier) +
    geom_histogram(aes(stat, fill = outlier, y = ..density..)) +
    stat_function(fun = dnorm)
  print(p)

  cat("Plot of pvalue hist \n")
  p <- gplot_stat(LfmmRidgeBootstrap$pvalue[1,],
             LfmmRidgeAnalyticSigma$pvalue[1,],
             LeaLfmm$pvalue[1,],
             lm$pvalue[1,],
             lmPca$pvalue[1,],
             lmPcaStructure$pvalue[1,],
             famt$pvalue[1,],
             sva$pvalue[1,],
             outlier = dat$outlier) +
    geom_histogram(aes(stat, fill = outlier, y = ..density..)) +
    stat_function(fun = dunif)
  print(p)

  cat("Plot of pvalue qqplot \n")
  p <- gplot_stat(LfmmRidgeBootstrap$pvalue[1,],
             LfmmRidgeAnalyticSigma$pvalue[1,],
             LeaLfmm$pvalue[1,],
             lm$pvalue[1,],
             lmPca$pvalue[1,],
             famt$pvalue[1,],
             sva$pvalue[1,],
             outlier = dat$outlier) +
    stat_qq(aes(sample = stat),
            distribution = stats::qunif) +
    geom_abline(slope = 1, intercept = 0) +
    ggtitle("pvalue qqplot")
  print(p)

  cat("manhattan \n")
  p <- gplot_stat(LfmmRidgeBootstrap$pvalue[1,],
                  LfmmRidgeAnalyticSigma$pvalue[1,],
                  LeaLfmm$pvalue[1,],
                  lm$pvalue[1,],
                  lmPca$pvalue[1,],
                  lmPcaStructure$pvalue[1,],
                  famt$pvalue[1,],
                  sva$pvalue[1,],
                  outlier = dat$outlier) +
    geom_point(aes(y = -log10(stat), x = index, color = outlier))
  print(p)

  ## Estimation comp
  cat("Estimation comparison \n")
  exp <- ComparisonExperiment(s = s,
                              LfmmRidgeBootstrap,
                              LfmmRidgeAnalyticSigma,
                              LeaLfmm,
                              lm,
                              lmPca,
                              lmPcaStructure,
                              famt,
                              sva)
  exp <- runExperiment(exp)
  p <- plot(exp, variable.name.regexp = "score|B") +
    facet_grid(variable.name~method.name, scales = "free")
  print(p)

  #### Hypothesis testing comparison
  cat("Hypothesis testing comparison \n")
  exp <- FDRControlExperiment(nb.rep = exp.rep, s = s,
                              LfmmRidgeBootstrap,
                              LfmmRidgeAnalyticSigma,
                              LeaLfmm,
                              lm,
                              lmPca,
                              lmPcaStructure,
                              famt,
                              sva)
  exp <- runExperiment(exp)
  p <- plot(exp, plot.type = "pvalue.grid", summary_bin = TRUE)
  print(p)
  p <- plot(exp, plot.type = "precision.recall", summary_bin = TRUE, geom = "point")
  print(p)
  p <- plot(exp, plot.type = "qqplot")
  print(p)

}



#' @export
RidgeVsLasso <- function(s, method.K, exp.rep) {

  dat <- sampl(s)
  # plot(dat)

  ## methods
  lm <- AnalyticSigma2Functor() %>%
    Zscore(B.sigma2.functor = .) %>%
    ClassicLinearMethod(hypothesis.testing.method = .,
                        nickname = "lm")
  # lm <- run(m = lm, dat = dat)

  lmPca <- AnalyticSigma2Functor() %>%
    Zscore(B.sigma2.functor = .) %>%
    PCAClassicLinearMethod(K = method.K,
                           hypothesis.testing.method = .,
                           nickname = "PCA+lm")
  # lmPca <- run(m = lmPca, dat = dat)

  lmPcaStructure <- AnalyticSigma2Functor() %>%
    Zscore(B.sigma2.functor = .) %>%
    PCAClassicLinearMethod(K = method.K,
                           hypothesis.testing.method = .,
                           nickname = "structure+lm",
                           assumingStructure = TRUE)
  # lmPcaStructure <- run(m = lmPcaStructure, dat = dat)

  LfmmRidge <- GifCalibratedZscore() %>%
    Zscore(B.sigma2.functor = AnalyticSigma2Functor(),
           zscorepvalue.functor = .) %>%
    RidgeLFMMMethod(K = method.K,
                    lambda = 10,
                    hypothesis.testing.method = .,
                    nickname = "lfmmRidge + gif")
  # LfmmRidge <- run(m = LfmmRidge, dat = dat)

  LeaLfmm <- LeaLFMMMethod(K = method.K,
                           nickname = "LeaLfmm",
                           verbose = FALSE)
  # LeaLfmm <- run(m = LeaLfmm, dat = dat)

  famt <- FAMTMethod(K = method.K,
                     nickname = "famt")
  # famt <- run(m = famt, dat = dat)

  LfmmLasso <- LassoLFMMMethod(K = method.K,
                               lambda = NULL,
                               lambda.K = 50,
                               lambda.eps = 0.001,
                               it.max = 100,
                               err.max = 1e-6,
                               nickname = "LfmmLasso")
  # LfmmLasso <- run(m = LfmmLasso, dat = dat)

  LfmmNuclear <- NuclearLFMMMethod(K = method.K,
                                   nickname = "LfmmNuclear")

  #### Hypothesis testing comparison
  cat("Hypothesis testing comparison \n")
  exp <- FDRControlExperiment(nb.rep = exp.rep, s = s,
                              LfmmRidge,
                              LeaLfmm,
                              lm,
                              lmPca,
                              lmPcaStructure,
                              famt,
                              LfmmNuclear,
                              LfmmLasso)
  exp <- runExperiment(exp)
  p <- plot(exp, plot.type = "pvalue.grid", summary_bin = TRUE)
  print(p)
  p <- plot(exp, plot.type = "precision.recall", summary_bin = TRUE, geom = "point")
  print(p)

}


#' @export
WithMissingValue <- function(s, method.K, exp.rep) {

  # dat <- sampl(s)

  ## methods
  lm <- AnalyticSigma2Functor() %>%
    Zscore(B.sigma2.functor = .) %>%
    ClassicLinearMethod(hypothesis.testing.method = .,
                        nickname = "lm")
  # lm <- run(m = lm, dat = dat)

  lmPca <- AnalyticSigma2Functor() %>%
    Zscore(B.sigma2.functor = .) %>%
    PCAClassicLinearMethod(K = method.K,
                           hypothesis.testing.method = .,
                           nickname = "PCA+lm")
  # lmPca <- run(m = lmPca, dat = dat)

  lmPcaStructure <- AnalyticSigma2Functor() %>%
    Zscore(B.sigma2.functor = .) %>%
    PCAClassicLinearMethod(K = method.K,
                           hypothesis.testing.method = .,
                           nickname = "structure+lm",
                           assumingStructure = TRUE)
  # lmPcaStructure <- run(m = lmPcaStructure, dat = dat)

  LfmmRidge <- GifCalibratedZscore() %>%
    Zscore(B.sigma2.functor = AnalyticSigma2Functor(),
           zscorepvalue.functor = .) %>%
    RidgeLFMMMethod(K = method.K,
                    lambda = 10,
                    hypothesis.testing.method = .,
                    nickname = "lfmmRidge + gif")
  # LfmmRidge <- run(m = LfmmRidge, dat = dat)

  LfmmRidgeAlternated <- GifCalibratedZscore() %>%
    Zscore(B.sigma2.functor = AnalyticSigma2Functor(),
           zscorepvalue.functor = .) %>%
    AlternatedSvdLFMMMethod(K = method.K,
                            lambda = 10,
                            it.max = 100,
                            err.max = 1e-6,
                            hypothesis.testing.method = .,
                            nickname = "lfmmRidgeAlternated + gif")

  LeaLfmm <- LeaLFMMMethod(K = method.K,
                           nickname = "LeaLfmm",
                           verbose = FALSE)
  # LeaLfmm <- run(m = LeaLfmm, dat = dat)


  #### Hypothesis testing comparison
  cat("Hypothesis testing comparison \n")
  exp <- FDRControlExperiment(nb.rep = exp.rep, s = s,
                              LfmmRidge,
                              LeaLfmm,
                              lm,
                              lmPca,
                              lmPcaStructure,
                              LfmmRidgeAlternated)
  exp <- runExperiment(exp)
  p <- plot(exp, plot.type = "pvalue.grid", summary_bin = TRUE)
  print(p)
  p <- plot(exp, plot.type = "precision.recall", summary_bin = TRUE, geom = "line")
  print(p)


}

#' @export
AlternatedVsAnalyticRidge <- function(s, method.K, exp.rep, lambda) {

  # dat <- sampl(s)
  lm <- AnalyticSigma2Functor() %>%
    Zscore(B.sigma2.functor = .) %>%
    ClassicLinearMethod(hypothesis.testing.method = .,
                        nickname = "lm")

  lmPca <- AnalyticSigma2Functor() %>%
    Zscore(B.sigma2.functor = .) %>%
    PCAClassicLinearMethod(K = method.K,
                           hypothesis.testing.method = .,
                           nickname = "PCA+lm")

  LfmmRidge <- GifCalibratedZscore() %>%
    Zscore(B.sigma2.functor = AnalyticSigma2Functor(),
           zscorepvalue.functor = .) %>%
    RidgeLFMMMethod(K = method.K,
                    lambda = lambda,
                    hypothesis.testing.method = .,
                    nickname = "lfmmRidge + gif")
  # LfmmRidge <- run(LfmmRidge, dat)
  LfmmRidgeAlternated <- GifCalibratedZscore() %>%
    Zscore(B.sigma2.functor = AnalyticSigma2Functor(),
           zscorepvalue.functor = .) %>%
    AlternatedSvdLFMMMethod(K = method.K,
                            lambda = lambda,
                            it.max = 150,
                            err.max = 1e-8,
                            hypothesis.testing.method = .,
                            nickname = "lfmmRidgeAlternated + gif")
  # LfmmRidgeAlternated <- run(LfmmRidgeAlternated, dat)

  #### Hypothesis testing comparison
  cat("Hypothesis testing comparison \n")
  exp <- FDRControlExperiment(nb.rep = exp.rep, s = s,
                              LfmmRidge,
                              lm,
                              lmPca,
                              LfmmRidgeAlternated)
  exp <- runExperiment(exp)
  p <- plot(exp, plot.type = "pvalue.grid", summary_bin = TRUE)
  print(p)
  p <- plot(exp, plot.type = "precision.recall", summary_bin = TRUE, geom = "line")
  print(p)

  #### Estimated comparison
  cat("Estimation comparison \n")
  exp <- ComparisonExperiment(s = s,
                              LfmmRidge,
                              lm,
                              lmPca,
                              LfmmRidgeAlternated)
  exp <- runExperiment(exp)
  p <- plot(exp, variable.name.regexp = "score|B") +
    facet_grid(variable.name~method.name, scales = "free")
  print(p)

}


#' @export
LambdaChoice <- function(s, method.K, exp.rep, lambda.lasso, lambda.ridge) {

  dat <- sampl(s)

  lm <- AnalyticSigma2Functor() %>%
    Zscore(B.sigma2.functor = .) %>%
    ClassicLinearMethod(hypothesis.testing.method = .,
                        nickname = "lm")
  lm <- run(lm, dat)
  cat(paste0("= lm: var_neutre / var_outlier = ",
             var(lm$B[,-dat$outlier]) / var(lm$B[,dat$outlier]), "\n"))

  lmPca <- AnalyticSigma2Functor() %>%
    Zscore(B.sigma2.functor = .) %>%
    PCAClassicLinearMethod(K = method.K,
                           hypothesis.testing.method = .,
                           nickname = "PCA+lm")
  lmPca <- run(lmPca, dat)
  cat(paste0("= lmPca: var_neutre / var_outlier = ",
             var(lmPca$B[,-dat$outlier]) / var(lmPca$B[,dat$outlier]), "\n"))

  # lambda.ridge = 1e-10
  LfmmRidge <- NormalZscore() %>%
    Zscore(B.sigma2.functor = AnalyticSigma2Functor(),
           zscorepvalue.functor = .) %>%
    RidgeLFMMMethod(K = method.K,
                    lambda = lambda.ridge,
                    hypothesis.testing.method = .,
                    nickname = "lfmmRidge")
  LfmmRidge <- run(LfmmRidge, dat)
  cat(paste0("= LfmmRidge: var_neutre / var_outlier = ",
             var(LfmmRidge$B[,-dat$outlier]) / var(LfmmRidge$B[,dat$outlier]), "\n"))

  LfmmLasso <- LassoLFMMMethod(K = method.K,
                       lambda = lambda.lasso,
                       it.max = 100,
                       err.max = 1e-6,
                       nickname = "lfmmLasso")
  LfmmLasso <- run(LfmmLasso, dat)


  #### Estimated comparison
  cat("Estimation comparison \n")
  exp <- ComparisonExperiment(s = s,
                              LfmmRidge,
                              lm,
                              lmPca,
                              LfmmLasso)
  exp <- runExperiment(exp)
  p <- plot(exp, variable.name.regexp = "B") +
    facet_grid(variable.name~method.name, scales = "free")
  print(p)

}

#' @export
Validation_1000Genome <- function(
  # dataset
  n,
  L,
  c,
  K,
  prop.outlier,
  # method
  method.K,
  ridge.lambda,
  sparse.prop,
  gif,
  # method
  exp.rep) {

  # data sample
  G.file <- "~/Projects/Data2016_2017/1000Genomes/Phase3Chrm22/European_Chrm22.rds"
  s <- FromTrueSampler(G.file = G.file,
                       n = n,
                       L = L,
                       K = K,
                       prop.outlier = prop.outlier,
                       rho = NULL,
                       cs = c(c, rep(0,K - 1)),
                       round = FALSE)

  # method
  ## lm
  lm <- GifCalibratedZscore() %>%
    Zscore(B.sigma2.functor = AnalyticSigma2Functor(),
           zscorepvalue.functor = .) %>%
    ClassicLinearMethod(hypothesis.testing.method = .,
                        nickname = "lm")
  ## lm + pca
  lmPca <- GifCalibratedZscore() %>%
    Zscore(B.sigma2.functor = AnalyticSigma2Functor(),
           zscorepvalue.functor = .) %>%
    PCAClassicLinearMethod(K = method.K,
                           hypothesis.testing.method = .,
                           nickname = "PCA+lm")

  ## lfmm ridge
  LfmmRidge <- RidgeLFMMMethod(K = method.K,
                               hypothesis.testing.method = lm_zscore(gif = gif),
                               lambda = ridge.lambda,
                               nickname = "lfmm ridge")

  ## lfmm lasso
  LfmmLasso <- LassoLFMMMethod(K = K,
                               it.max = 200,
                               err.max = 1e-8,
                               lambda = NULL,
                               lambda.K = 30,
                               lambda.eps = 0.001,
                               sparse.prop = sparse.prop,
                               hypothesis.testing.method = lm_zscore(gif = gif),
                               nickname = "lfmm lasso")

  ## LEA lfmm
  LeaLfmm <- LeaLFMMMethod(K = method.K,
                           nickname = "LeaLfmm",
                           verbose = FALSE)


  # experiment
  #### Hypothesis testing comparison
  cat("Hypothesis testing comparison \n")
  exp <- FDRControlExperiment(nb.rep = exp.rep,
                              s = s,
                              LfmmRidge,
                              lm,
                              lmPca,
                              LfmmLasso,
                              LeaLfmm)
  exp <- runExperiment(exp)
  p <- plot(exp, plot.type = "pvalue.grid", summary_bin = TRUE)
  print(p)
  p <- plot(exp, plot.type = "precision.recall", summary_bin = TRUE, geom = "line")
  print(p)
}


#' @export
LassoRidgeEtLambda <- function(s, method.K, exp.rep, ridge.lambda.low,
                               ridge.lambda.high,
                               ridge.lambda.medium,
                               sparse.prop, gif) {


  lm <- GifCalibratedZscore() %>%
    Zscore(B.sigma2.functor = AnalyticSigma2Functor(),
           zscorepvalue.functor = .) %>%
    ClassicLinearMethod(hypothesis.testing.method = .,
                        nickname = "lm")
  ## lm + pca
  lmPca <- GifCalibratedZscore() %>%
    Zscore(B.sigma2.functor = AnalyticSigma2Functor(),
           zscorepvalue.functor = .) %>%
    PCAClassicLinearMethod(K = method.K,
                           hypothesis.testing.method = .,
                           nickname = "PCA+lm")
  ## lfmm ridge
  LfmmRidge_low <- RidgeLFMMMethod(K = method.K,
                               hypothesis.testing.method = lm_zscore(gif = gif),
                               lambda = ridge.lambda.low,
                               nickname = "lfmm ridge low")

  LfmmRidge_high <- RidgeLFMMMethod(K = method.K,
                                   hypothesis.testing.method = lm_zscore(gif = gif),
                                   lambda = ridge.lambda.high,
                                   nickname = "lfmm ridge high")

  LfmmRidge_med <- RidgeLFMMMethod(K = method.K,
                                    hypothesis.testing.method = lm_zscore(gif = gif),
                                    lambda = ridge.lambda.medium,
                                    nickname = "lfmm ridge med")
  # lfmm lasso
  LfmmLasso <- LassoLFMMMethod(K = method.K,
                               it.max = 200,
                               err.max = 1e-8,
                               lambda = NULL,
                               lambda.K = 4,
                               lambda.eps = 0.001,
                               sparse.prop = sparse.prop,
                               hypothesis.testing.method = lm_zscore(gif = gif),
                               nickname = "lfmm lasso")

  #### Hypothesis testing comparison
  cat("Hypothesis testing comparison \n")
  exp <- FDRControlExperiment(nb.rep = exp.rep, s = s,
                              LfmmLasso,
                              LfmmRidge_low,
                              LfmmRidge_high,
                              LfmmRidge_med,
                              lmPca,
                              lm
                              )
  exp <- runExperiment(exp)
  p <- plot(exp, plot.type = "pvalue.grid", summary_bin = TRUE)
  print(p)
  p <- plot(exp, plot.type = "precision.recall", summary_bin = TRUE, geom = "point")
  print(p)

  #### Estimated comparison
  # cat("Estimation comparison \n")
  # exp <- ComparisonExperiment(s = s,
  #                             LfmmRidge_low,
  #                             LfmmRidge_high,
  #                             LfmmRidge_med)
  # exp <- runExperiment(exp)
  # p <- plot(exp, variable.name.regexp = "B") +
  #   facet_grid(variable.name~method.name, scales = "free")
  # print(p)


}


#' @export
NuclearLfmm <- function(s, method.K, lambda.lfmmridge, lambda.lfmmnuclear, gamma, exp.rep, lasso, soft) {

  gif <- TRUE

  lm <- ClassicLinearMethod(nickname = "lm")

  ## lm + pca
  lmPca <- PCAClassicLinearMethod(K = method.K,
                           nickname = "PCA+lm",
                           hypothesis.testing.method = lm_zscore(gif = gif))
  ## lfmm ridge
  LfmmRidge <- RidgeLFMMMethod(K = method.K,
                                   lambda = lambda.lfmmridge,
                                   nickname = "lfmm ridge",
                               hypothesis.testing.method = lm_zscore(gif = gif))

  m.nuclear <- NuclearLFMMMethod(K = method.K,
                                 lambda = lambda.lfmmnuclear,
                                 gamma = gamma,
                                 nickname = "lfmm nuclear",
                                 lasso = lasso,
                                 soft = soft,
                                 hypothesis.testing.method = lm_zscore(gif = gif, correctionByC = TRUE))

  #### Hypothesis testing comparison
  cat("Hypothesis testing comparison \n")
  exp <- FDRControlExperiment(nb.rep = exp.rep, s = s,
                              m.nuclear,
                              LfmmRidge,
                              lmPca,
                              lm
  )
  exp <- runExperiment(exp)
  p <- plot(exp, plot.type = "precision.recall", summary_bin = TRUE, geom = "point")
  print(p)

  #### Estimated comparison
  # cat("Estimation comparison \n")
  # exp <- ComparisonExperiment(s = s,
  #                             LfmmRidge_low,
  #                             LfmmRidge_high,
  #                             LfmmRidge_med)
  # exp <- runExperiment(exp)
  # p <- plot(exp, variable.name.regexp = "B") +
  #   facet_grid(variable.name~method.name, scales = "free")
  # print(p)


}

#' @export
calibration <- function(s, method.K, lambda.lfmmridge, gif, exp.rep, comp.exp = FALSE) {

  if (gif) {
    hypothesis.testing.method <- Zscore(zscorepvalue.functor = GifCalibratedZscore() ,
                                        B.sigma2.functor = AnalyticSigma2Functor())
  } else {
    hypothesis.testing.method <- Zscore(zscorepvalue.functor = NormalZscore() ,
                                        B.sigma2.functor = AnalyticSigma2Functor())
  }

  lm <- ClassicLinearMethod(nickname = "lm",
                            hypothesis.testing.method = hypothesis.testing.method
                            )

  ## lm + pca
  lmPca <- PCAClassicLinearMethod(K = method.K,
                                  nickname = "PCA+lm",
                                  hypothesis.testing.method = hypothesis.testing.method)
  ## lfmm ridge
  LfmmRidge <- RidgeLFMMMethod(K = method.K,
                               lambda = lambda.lfmmridge,
                               nickname = "lfmm ridge",
                               hypothesis.testing.method = lm_zscore(gif = gif))

  ## lfmm ridge
  # LfmmLasso <- LassoLFMMMethod(K = method.K,
  #                              it.max = 200,
  #                              err.max = 1e-8,
  #                              lambda = 0.0005,
  #                              sparse.prop = 0.01,
  #                              nickname = "lfmm lasso",
  #                              hypothesis.testing.method = lm_zscore(gif = gif))

  #### Estimated comparison
  if (comp.exp) {
    cat("Estimation comparison \n")
    exp <- ComparisonExperiment(s = s,
                                LfmmRidge,
                                lmPca,
                                lm)
    exp <- runExperiment(exp)
    toplot <- exp$df.res %>%
      dplyr::filter(variable.name %in% c("B1"))
    ggplot(toplot, aes(x = estimate, fill = outlier)) +
      geom_histogram() +
      facet_grid(method.name ~ variable.name, scales = "free")
  }


  #### Hypothesis testing comparison
  cat("Hypothesis testing comparison \n")
  exp <- FDRControlExperiment(nb.rep = exp.rep, s = s,
                              LfmmRidge,
                              lmPca,
                              # LfmmLasso,
                              lm)
  exp <- runExperiment(exp)
  p <- plot(exp, plot.type = "pvalue.grid", summary_bin = TRUE)
  print(p)
  p <- plot(exp, plot.type = "precision.recall", summary_bin = TRUE, geom = "line")
  print(p)
  p <- plot(exp, plot.type = "qqplot")
  print(p)


}


#' @export
bootstrapCalibration <- function(s, method.K, lambda.lfmmridge, exp.rep, comp.exp = FALSE) {

  # hyp testing func
  hypothesis.testing.method <- Zscore(zscorepvalue.functor = NormalZscore() ,
                                      B.sigma2.functor = AnalyticSigma2Functor())

  lm <- ClassicLinearMethod(nickname = "lm",
                            hypothesis.testing.method = hypothesis.testing.method
  )

  ## lm + pca
  lmPca <- PCAClassicLinearMethod(K = method.K,
                                  nickname = "PCA+lm",
                                  hypothesis.testing.method = hypothesis.testing.method)
  ## lfmm ridge
  LfmmRidgeGif <- RidgeLFMMMethod(K = method.K,
                                  lambda = lambda.lfmmridge,
                                  nickname = "lfmmridge+gif",
                                  hypothesis.testing.method = lm_zscore(gif = TRUE))
  LfmmRidgeLm <- RidgeLFMMMethod(K = method.K,
                               lambda = lambda.lfmmridge,
                               nickname = "lfmmridge+lm",
                               hypothesis.testing.method = lm_zscore(gif = FALSE,
                                                                    sigma.computation = "lm"))
  LfmmRidgeLmDf <- RidgeLFMMMethod(K = method.K,
                                 lambda = lambda.lfmmridge,
                                 nickname = "lfmmridge+lm+df",
                                 hypothesis.testing.method = lm_zscore(gif = FALSE,
                                                                       sigma.computation = "lm+df"))

  LfmmRidge <- Bootstrap( PairedBoostrap,
                          50) %>%
    Zscore(zscorepvalue.functor = NormalZscore(),
           B.sigma2.functor = .) %>%
    RidgeLFMMMethod(K = method.K ,
                    lambda = lambda.lfmmridge, # to add numerical stability
                    nickname = "lfmmridge+\n paredBoot",
                    hypothesis.testing.method = .)



  ## lfmm boot
  LfmmRidgeBoot <- RidgeLFMMMethod(K = method.K,
                                  lambda = lambda.lfmmridge,
                                  nickname = "lfmmridge+lm+\n pairedBoot",
                                  hypothesis.testing.method = lm_zscore(gif = FALSE,
                                                                        sigma.computation = c("bootstrap"),
                                                                        bootstrap.func = PairedBoostrap,
                                                                        bootstrap.rep = 50))

  #### Estimated comparison
  if (comp.exp) {
    cat("Estimation comparison \n")
    exp <- ComparisonExperiment(s = s,
                                LfmmRidgeGif,
                                LfmmRidgeBoot,
                                LfmmRidgeLm,
                                LfmmRidge,
                                LfmmRidgeLmDf,
                                lmPca,
                                lm)
    exp <- runExperiment(exp)
    toplot <- exp$df.res %>%
      dplyr::filter(variable.name %in% c("B1"))
    ggplot(toplot, aes(x = estimate, fill = outlier)) +
      geom_histogram() +
      facet_grid(method.name ~ variable.name, scales = "free")
  }


  #### Hypothesis testing comparison
  cat("Hypothesis testing comparison \n")
  exp <- FDRControlExperiment(nb.rep = exp.rep, s = s,
                              LfmmRidgeGif,
                              LfmmRidgeBoot,
                              LfmmRidgeLm,
                              LfmmRidgeLmDf,
                              LfmmRidge,
                              lmPca,
                              lm)
  exp <- runExperiment(exp)
  p <- plot(exp, plot.type = "pvalue.grid", summary_bin = TRUE)
  print(p)
  p <- plot(exp, plot.type = "precision.recall", summary_bin = TRUE, geom = "line")
  print(p)
  p <- plot(exp, plot.type = "qqplot")
  print(p)


}

#' @export
finalSimuValiation <- function(s, method.K, lambda.lfmmridge, exp.rep, calibrate, comp.exp = FALSE, with.missing = FALSE, fast.only = FALSE) {

  # Method
  bench <- finalBench(K = method.K, lambda = lambda.lfmmridge, calibrate = calibrate, with.missing = with.missing, fast.only = fast.only)

  if (with.missing) {
    bench <- bench[c("lfmmRidge",
                     "oracle",
                   "lfmm.ridge.impute.first")]
  }

  if (!with.missing) {
    # plot data
    expPCA <- PCAExperiment(s)
    expPCA <- runExperiment(expPCA)
    p <- plot(expPCA) +
      scale_x_continuous(limits = c(1,60))
    print(p)
  }
  # Estimated comparison
  if (comp.exp) {
    cat("Estimation comparison \n")
    exp <- do.call(ComparisonExperiment,c(list(s = s), bench))
    exp <- runExperiment(exp)
    toplot <- exp$df.res %>%
      dplyr::filter(variable.name %in% c("B1"))
    p <-  ggplot(toplot, aes(x = estimate, fill = outlier)) +
      geom_histogram() +
      facet_grid(method.name ~ variable.name, scales = "free")
    print(p)

  }


  # Hypothesis testing comparison
  cat("Hypothesis testing comparison \n")
  exp <- do.call(FDRControlExperiment,c(list(nb.rep = exp.rep, s = s), bench))

  exp <- runExperiment(exp)
  p <- plot(exp, plot.type = "pvalue.grid", summary_bin = TRUE)
  print(p)
  p <- plot(exp, plot.type = "precision.recall", summary_bin = TRUE, geom = "line")
  print(p)
  p <- plot(exp, plot.type = "qqplot")
  print(p)

}

#' @export
crossValidationCriteria <- function(s, method.K, lambda.lfmmridge, lambdas,
                                    seed = sample.int(.Machine$integer.max, 1), cross.rep = 5) {

  # Method
  bench <- finalBench(K = method.K, lambda = lambda.lfmmridge, gif = FALSE, with.missing = FALSE, fast.only = TRUE)

  # plot data
  expPCA <- PCAExperiment(s, seed = seed)
  expPCA <- runExperiment(expPCA)
  p <- plot(expPCA) +
    scale_x_continuous(limits = c(1,60))
  print(p)

  # cross validation
  if (!is.null(lambdas)) {
    set.seed(seed)
    dat <- sampl(s)
    m <-  finalLfmmRdigeMethod(K = method.K,
                               lambda = NULL)
    crossvalid.res <- crossvalidation_kfold_missingvalue(m, dat,
                                                         rep = cross.rep, missing.prop = 0.5,
                                                         lambdas)
    p <- plot(crossvalid.res, "imputation_error")
    print(p)
  }

  # Hypothesis testing comparison
  cat("Hypothesis testing comparison \n")
  exp <- do.call(FDRControlExperiment,c(list(nb.rep = 1, s = s, seed = seed), bench))

  exp <- runExperiment(exp)
  p <- plot(exp, plot.type = "precision.recall", summary_bin = TRUE, geom = "line")
  print(p)

}

