Article3_MethodComparison_main <- function(exp) {
  exp$df <- tibble()
  for (p in exp$outlier.props) {
    for (c in exp$cs) {
      flog.info(paste0("outlier prop=",p, " and c=",paste0(c, collapse = "|")), name = "console")
      s <- exp$s
      s$prop.outlier <- p
      if(exp$cs.sum) {
        aux <- runif(s$K)
        aux <- aux / sum(aux) * c
        s$cs <- aux
      } else {
        s$cs <- c
      }
      if (is.null(exp$methods)) {
        bench <- finalBench(K = exp$K.method,
                            lambda = 1e-5,
                            sparse.prop = p,
                            calibrate = FALSE,
                            fast.only = exp$fast.only, with.missing = FALSE,
                            correctionByC = exp$correctionByC)
      } else {
        bench <- exp$methods
      }
      exp.aux <- do.call(FDRControlExperiment,c(list(nb.rep = exp$nb.rep, s = s), bench))
      exp.aux <- runExperiment(exp.aux)

      exp$df <- exp.aux$result$df.pvalue %>%
        dplyr::mutate(outlier.prop = p) %>%
        dplyr::mutate(`cor(U,X)` = paste0(c, collapse="|")) %>%
        rbind(exp$df)
    }
  }
  exp
}


#' Article3_MethodComparison
#'
#' @param outlier.props proportion of outlier simulated
#' @param cs if cs.sum = FALSE, correlation between PC1 and X. If cs.sum = TRUE, sum(PC,X) = cs
#'
#' @export
Article3_MethodComparison <- function(G.file,
                                      outlier.props = c(0.01, 0.05, 0.1),
                                      n = NULL, L = 10000,
                                      K = 4,
                                      K.method = K,
                                      correctionByC = FALSE,
                                      cs = c(0.2, 0.4, 0.6, 0.8),
                                      cs.sum = TRUE,
                                      sd.V.rho = 1.0,
                                      rho.E = 1.0,
                                      nb.rep = 5,
                                      fast.only = TRUE,
                                      cluster.nb = NULL,
                                      save = TRUE, bypass = FALSE,
                                      methods = NULL) {


  cl <- long_init(cluster.nb,
                  bypass, log.file = "Article3_MethodComparison.log")

  exp <- Experiment()
  exp$name <- "Article3_MethodComparison"
  exp$description <- make_description("Article3_MethodComparison",
                                     G.file = G.file,
                                     K = K,
                                     K.method = K.method,
                                     correctionByC = correctionByC,
                                     fast.only = fast.only,
                                     n = n, L = L,
                                     cs = cs,
                                     cs.sum = cs.sum,
                                     sd.V.rho = sd.V.rho,
                                     rho.E = rho.E,
                                     outlier.props = outlier.props,
                                     nb.rep = nb.rep)
  exp$fast.only <- fast.only
  exp$nb.rep <- nb.rep
  exp$outlier.props  <- outlier.props
  exp$cs  <- cs
  exp$cs.sum  <- cs.sum
  exp$sd.V.rho <- sd.V.rho
  exp$rho.E <- rho.E
  exp$K.method <- K.method
  exp$correctionByC <- correctionByC

  ## sampler
  B.outlier.sampler <- function(n, mean, sd) {
    res = 1:n
    for (i in 1:n) {
      res[i] <- rnorm(1, mean, sd)
      while (abs(res[i]) < 1 * sd) {
        res[i] <- rnorm(1, mean, sd)
      }
    }
    res
  }

  exp$s <- FromTrueSampler(G.file = G.file,
                           n = n,
                           L = L,
                           K = K,
                           prop.outlier = NULL,
                           rho = NULL,
                           cs = NULL,
                           sd.V.rho = sd.V.rho,
                           rho.E = rho.E,
                           round = FALSE,
                           B.outlier.sampler = B.outlier.sampler)
  exp$methods <- methods
  exp <- Article3_MethodComparison_main(exp)

  ## return
  long_return(cl, save, exp)
}

#' @export
Article3_MethodComparison_plot_pvalueGrid <- function(exp, c) {
  assertthat::assert_that(exp$name == "Article3_MethodComparison")

  toplot <- exp$df %>%
    dplyr::filter(`cor(U,X)` == c)
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
    ggtitle(paste0("cor(U,X)=",c))
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
    facet_grid(`cor(U,X)` ~ outlier.prop, scales = "free")
  p
}

#' @export
Article3_MethodComparison_plot_AUC <- function(exp) {
  assertthat::assert_that(exp$name == "Article3_MethodComparison")
  TestRequiredPkg("DescTools")

  ## compute AUC
  toplot <- exp$df %>%
    group_by(method, outlier.prop, `cor(U,X)`, rep) %>%
    summarise(auc = DescTools::AUC(x = true.power, y = 1 - true.fdr)) %>%
    ungroup()

  ## compute mean and standard error
  ## toplot <- toplot %>%
  ##   group_by(method, outlier.prop, `cor(U,X)`) %>%
  ##   summarise(auc.mean = mean(auc), mean.sd = sd(auc), mean.se = mean.sd / sqrt(length(auc))) %>%
  ##   ungroup()

  ggplot(toplot, aes(x = method, y = auc, color = method)) +
    geom_boxplot() +
    facet_grid(`cor(U,X)` ~ outlier.prop)

}

#' @export
Article3_MethodComparison_plot_relative_diff_AUC <- function(exp) {
  assertthat::assert_that(exp$name == "Article3_MethodComparison")
  TestRequiredPkg("DescTools")

  ## compute AUC
  toplot <- exp$df %>%
    group_by(method, outlier.prop, `cor(U,X)`, rep) %>%
    summarise(auc = DescTools::AUC(x = true.power, y = 1 - true.fdr)) %>%
    ungroup()

  ## compute relative diff
  oracle.auc <- toplot %>%
    dplyr::filter(method == "Oracle")

  print(oracle.auc)

  toplot <- dplyr::inner_join(toplot, oracle.auc, by = c("outlier.prop", "cor(U,X)", "rep")) %>%
    dplyr::mutate(auc.relative.diff = (auc.y - auc.x) / auc.y,
                  method = method.x) %>%
    dplyr::filter(method != "Oracle")

  ## compute mean and standard error
  ## toplot <- toplot %>%
  ##   group_by(method, outlier.prop, `cor(U,X)`) %>%
  ##   summarise(auc.mean = mean(auc), mean.sd = sd(auc), mean.se = mean.sd / sqrt(length(auc))) %>%
  ##   ungroup()

  ggplot(toplot, aes(x = method, y = auc.relative.diff, color = method)) +
    geom_boxplot() +
    facet_grid(`cor(U,X)` ~ outlier.prop)

}

#' This function compute GIF on null loci, so the GIF must be equal to 1
#'
#' We use use a quantile function to compute score2 from pvalue... ;)
#'
#' @export
Article3_MethodComparison_plot_GIF <- function(exp) {
  assertthat::assert_that(exp$name == "Article3_MethodComparison")

  aux.f <- function(pvalue) {
    score2 <- qchisq(pvalue, lower.tail = FALSE, df = 1)
    median(score2, na.rm = TRUE) / qchisq(0.5, df = 1)
  }

  ## compute AUC
  toplot <- exp$df %>%
    group_by(method, outlier.prop, `cor(U,X)`, rep) %>%
    summarise(gif = aux.f(pvalue[!outlier])) %>%
    ungroup()

  ggplot(toplot, aes(x = method, y = gif, color = method)) +
    geom_boxplot() +
    facet_grid(`cor(U,X)` ~ outlier.prop)

}
