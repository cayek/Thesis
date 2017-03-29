#' @export
Article3_MethodComparisonWithMissingValue <- function(G.file,
                                                      outlier.props = c(0.05, 0.1),
                                                      n = NULL, L = 10000,
                                                      K = 4,
                                                      cs = c(0.6, 0.3, 0.0, 0.0),
                                                      nb.rep = 5,
                                                      fast.only = FALSE,
                                                      cluster.nb = NULL,
                                                      save = TRUE, bypass = FALSE) {

  cl <- long_init(cluster.nb,
                  bypass)

  exp <- Experiment()
  exp$name <- "Article3_MethodComparisonWithMissingValue"
  exp$description = make_description("Article3_MethodComparisonWithMissingValue",
                                     G.file = G.file,
                                     K = K,
                                     n = n, L = L,
                                     cs = cs,
                                     outlier.props = outlier.props,
                                     nb.rep = nb.rep)
  exp$fast.only <- fast.only
  exp$nb.rep <- nb.rep
  exp$outlier.props  <- outlier.props
  exp$s <- FromTrueSampler(G.file = G.file,
                           n = n,
                           L = L,
                           K = K,
                           prop.outlier = NULL,
                           rho = NULL,
                           cs = cs,
                           round = FALSE) %>%
    SparseMissingValueSampler(missing.prop = 0.1, missing.prop.by.loci = 0.5)

  exp <- Article3_MethodComparison_main(exp)

  ## return
  long_return(cl, save, exp)
}
