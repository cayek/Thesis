#' FDRControlExperiment
#'
#'
#' @export
FDRControlExperiment <- function(nb.rep, sampler,
                                 ...,
                                 seed = sample.int(.Machine$integer.max, 1)) {
  exp <- Experiment(name = "ComparisonExperiment",
                    description = "comparison of algorithm on a sampler",
                    seed = seed)
  class(exp) <- c("FDRControlExperiment", class(exp))
  exp$methods <- list(...)
  exp$sampler <- sampler
  exp$nb.rep <- nb.rep
  exp
}


#' @export
runExperiment.FDRControlExperiment <- function(exp) {

  # set the seed
  DebugMessage("We set the seed")
  set.seed(exp$seed)

  df.pvalue <- tibble()

  start.time <- Sys.time()
  for (r in 1:exp$nb.rep) {
    dat <- sampl(exp$sampler)
    df.pvalue <- foreach(method = exp$methods, .combine = 'rbind') %dopar%
    {
      method.name <- name(method)
      res <- run(method, dat)
      tidy_fdr(res$pvalue,
               outlier = dat$outlier) %>%
        mutate(rep = r,
               method = method.name)
    } %>%
      rbind(df.pvalue)
  }
  end.time <- Sys.time()
  exp$runtime <- end.time - start.time


  exp$result <- list(df.pvalue = df.pvalue)
  exp
}



#' @export
plot.FDRControlExperiment <- function(exp,
                                      plot.type = c("pvalue.grid", "precision.recall", "qqplot"),
                                      rep.indices = "all",
                                      summary_bin = FALSE,
                                      geom = "point") {
  if (is.numeric(rep.indices)) {
    exp$result$df.pvalue <- exp$result$df.pvalue %>%
      dplyr::filter(rep %in% rep.indices)
  }

  if (plot.type[1] == "pvalue.grid") {
    p <- ggplot(exp$result$df.pvalue ,
                aes(x = expected.fd, y = true.fd  - expected.fd )) +
      facet_grid(method ~ pvalue.index) +
      layer(geom = geom, mapping = aes(group = as.factor(rep)), params = list(color = "grey", size = 0.1),
            stat = "identity", position = "identity")
    if (summary_bin) {
      p <- p +
        stat_summary_bin(fun.y = median, geom = geom) +
        stat_summary_bin(fun.y = function(x) quantile(x,0.9), geom = geom, color = "blue4")  +
        stat_summary_bin(fun.y = function(x) quantile(x,0.1), geom = geom, color = "blue4")
    }
    p
  } else if (plot.type[1] == "precision.recall") {
    p <- ggplot(exp$result$df.pvalue, aes(x = true.power, y = 1 - true.fdr,
                                          color = method)) +
      facet_grid( ~ pvalue.index)
    if (summary_bin) {
      p <- p + stat_summary_bin(fun.y = median, geom = geom)
    } else {
      p <- p +
        layer(geom = geom, mapping = aes(group = as.factor(rep)),
              stat = "identity", position = "identity")
    }
    p
  } else if (plot.type[1] == "qqplot") {
    ggplot(exp$result$df.pvalue, aes(sample = -log10(pvalue), color = method)) +
      stat_qq(distribution = stats::qexp, dparams = list(rate = log(10))) +
      facet_grid( ~ pvalue.index) +
      geom_abline(slope = 1, intercept = 0) +
      ggtitle("-log10(pvalue) qqplot")
  }
}



