#' @export
Article3_withMissingValueResults <- function(s,
                                             s.name,
                                             nb.rep,
                                             lambda,
                                             K,
                                             cluster.nb = NULL,
                                             save = TRUE, bypass = FALSE) {

  ## init
  long_init(cluster.nb = cluster.nb,
            bypass = bypass)

  ## methods
  oracle <- finalOracle(K = K,
                        calibrate = FALSE)
  lfmm.alternated <- finalLfmmRdigeMethod(K = K,
                                          lambda = lambda,
                                          calibrate = FALSE,
                                          prior.impute = FALSE,
                                          nickname = "lfmm.alternated")
  lfmm.prior.impute <- finalLfmmRdigeMethod(K = K,
                                            lambda = lambda,
                                            calibrate = FALSE,
                                            prior.impute = TRUE,
                                            "lfmm.prior.impute")
  ## exp
  exp <- FDRControlExperiment(nb.rep, sampler = s,
                              lfmm.alternated,
                              lfmm.prior.impute,
                              oracle)
  exp$name <- "Article3_withMissingValueResults"
  exp$description <-  make_description("Article3_withMissingValueResults",
                                       s.name = s.name,
                                       lambda = lambda,
                                       K = K)

  ## main
  exp <- runExperiment(exp)

  class(exp) <- c("Article3_withMissingValueResults", class(exp))
  ## save exp
  if (save) {
    dumpExperiment(exp)
  }
  exp
}


#' @export
plot.Article3_withMissingValueResults <- function(exp) {
  p1 <- plot.FDRControlExperiment(exp, plot.type = "pvalue.grid")
  print(p1)
  p2 <- plot.FDRControlExperiment(exp, plot.type = "precision.recall")
  print(p2)
}
