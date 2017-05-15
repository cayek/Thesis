#' @export
Article3_withMissingValueResults <- function(s,
                                             s.name,
                                             nb.rep,
                                             lambda,
                                             K,
                                             fast.only = TRUE,
                                             cluster.nb = NULL,
                                             save = TRUE, bypass = FALSE) {

  ## init
  cl <- long_init(cluster.nb = cluster.nb,
                  bypass = bypass)

  ## methods
  methods <- list()
  methods$oracle <- finalOracle(calibrate = FALSE)
  methods$lfmm.alternated <- finalLfmmRdigeMethod(K = K,
                                          lambda = lambda,
                                          calibrate = FALSE,
                                          prior.impute = FALSE,
                                          nickname = "lfmm.alternated")
  methods$lfmm.prior.impute <- finalLfmmRdigeMethod(K = K,
                                            lambda = lambda,
                                            calibrate = FALSE,
                                            prior.impute = TRUE,
                                            "lfmm.prior.impute")
  if (!fast.only) {
    methods$lfmm.lasso <- finalLfmmLassoMethod(K = K,
                                               sparse.prop = s$sampler$prop.outlier,
                                               calibrate = FALSE,
                                               nickname = "lfmm.lasso")
  }

  ## exp
  exp <- do.call(FDRControlExperiment, c(list(nb.rep = nb.rep, sampler = s), methods))
  exp$name <- "Article3_withMissingValueResults"
  exp$description <-  make_description("Article3_withMissingValueResults",
                                       s.name = s.name,
                                       lambda = lambda,
                                       K = K)

  ## main
  exp <- runExperiment(exp)

  class(exp) <- c("Article3_withMissingValueResults", class(exp))

  ## return
  long_return(cl = cl, save = save, exp = exp)
}


#' @export
plot.Article3_withMissingValueResults <- function(exp) {
  p1 <- plot.FDRControlExperiment(exp, plot.type = "pvalue.grid")
  print(p1)
  p2 <- plot.FDRControlExperiment(exp, plot.type = "precision.recall", summary_bin = TRUE)
  # p2 <- ggplot(exp$result$df.pvalue, aes(x = true.power, y = 1 - true.fdr,
  #                         color = method)) +
  #   geom_smooth() +
  #   ylab("1 - observed false positives rate") +
  #   xlab("observed power")
  print(p2)
}
