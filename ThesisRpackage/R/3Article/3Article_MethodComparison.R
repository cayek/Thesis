Article3_MethodComparison_main <- function(exp) {
  exp$df <- tibble()
  for (p in exp$outlier.props) {
    for (c in exp$cs) {
      flog.console(paste0("outlier prop=",p, " and c=",c))
      s <- exp$s
      s$prop.outlier <- p
      s$cs <- c
      bench <- finalBench(K = s$K,
                          lambda = 1e-5,
                          sparse.prop = p,
                          calibrate = FALSE,
                          fast.only = exp$fast.only, with.missing = FALSE)
      exp.aux <- do.call(FDRControlExperiment,c(list(nb.rep = exp$nb.rep, s = s), bench))
      exp.aux <- runExperiment(exp.aux)


      exp$df <- exp.aux$result$df.pvalue %>%
        dplyr::mutate(outlier.prop = p) %>%
        dplyr::mutate(`cor(U1,X)` = c) %>%
        rbind(exp$df)
    }
  }
  exp
}


#' Article3_MethodComparison
#'
#' @param outlier.props proportion of outlier simulated
#' @param cs correlation between PC1 and X

#'
#' @export
Article3_MethodComparison <- function(G.file,
                                      outlier.props = c(0.01, 0.05, 0.1),
                                      n = NULL, L = 10000,
                                      K = 4,
                                      cs = c(0.2, 0.4, 0.6, 0.8),
                                      nb.rep = 5,
                                      fast.only = FALSE,
                                      cluster.nb = NULL,
                                      save = TRUE, bypass = FALSE) {


  cl <- long_init(cluster.nb,
                  bypass, log.file = "Article3_MethodComparison.log")

  exp <- Experiment()
  exp$name <- "Article3_MethodComparison"
  exp$description <- make_description("Article3_MethodComparison",
                                     G.file = G.file,
                                     K = K,
                                     n = n, L = L,
                                     cs = cs,
                                     outlier.props = outlier.props,
                                     nb.rep = nb.rep)
  exp$fast.only <- fast.only
  exp$nb.rep <- nb.rep
  exp$outlier.props  <- outlier.props
  exp$cs  <- cs
  exp$s <- FromTrueSampler(G.file = G.file,
                           n = n,
                           L = L,
                           K = K,
                           prop.outlier = NULL,
                           rho = NULL,
                           cs = NULL,
                           round = FALSE)
  exp <- Article3_MethodComparison_main(exp)

  ## return
  long_return(cl, save, exp)
}

#' @export
Article3_MethodComparison_plot_pvalueGrid <- function(exp, c) {
  assertthat::assert_that(exp$name == "Article3_MethodComparison")

  toplot <- exp$df %>%
    dplyr::filter(`cor(U1,X)` == c)
  p <- ggplot(toplot ,
              aes(x = expected.fd, y = true.fd  - expected.fd )) +
    facet_grid(method ~ outlier.prop, scales = "free") +
    layer(geom = "point", mapping = aes(color = method, group = as.factor(rep)),
          params = list(size = 0.1),
          stat = "identity", position = "identity") +
    stat_summary_bin(fun.y = median, geom = "point") +
    stat_summary_bin(fun.y = function(x) quantile(x,0.9), geom = "point", color = "blue4") +
    stat_summary_bin(fun.y = function(x) quantile(x,0.1), geom = "point", color = "blue4") +
    ylab("observed false positives – expected false positives") +
    xlab("expected false positives") +
    guides(colour = "none") +
    ggtitle(paste0("cor(U1,X)=",c))
  p
}

#' @export
Article3_MethodComparison_plot_precisionRecall <- function(exp) {
  assertthat::assert_that(exp$name == "Article3_MethodComparison")


  p <- ggplot(exp$df, aes(x = true.power, y = 1 - true.fdr,
                          color = method)) +
    geom_smooth() +
    ylab("1 - observed false positives rate") +
    xlab("observed power") +
    facet_grid(`cor(U1,X)` ~ outlier.prop, scales = "free")
  p
}
