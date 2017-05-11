#' Run a batch of method on a dataset
#'
#' @export
MethodBatchExperiment <- function(data.name,
                                  s,
                                  method.batch,
                                  cluster.nb = NULL) {
  description <- paste0("Run on ", data.name, " with ")
  for(m in method.batch) {
    description <- paste0(description, m$nickname, "|")
  }

  expr <- Experiment(name = "MethodBatchExperiment",
                     description = description)
  class(expr) <- c("MethodBatchExperiment", class(expr))
  expr$method.batch <- method.batch
  expr$s <- s
  expr$cluster.nb <- cluster.nb
  expr
}


################################################################################
# Method


#' @export
runExperiment.MethodBatchExperiment <- function(expr, save = FALSE) {
  ## set the seed
  DebugMessage("We set the seed")
  set.seed(expr$seed)

  ## init
  cl <- long_init(expr$cluster.nb, bypass = TRUE)

  ## dataset
  dat <- sampl(expr$s)
  expr$outlier <- dat$outlier

  start.time <- Sys.time()
  foreach(i = seq_along(expr$method.batch)) %dopar%
    {
      expr$method.batch[[i]] <- run(expr$method.batch[[i]], dat)
    }

  end.time <- Sys.time()
  expr$runtime <- end.time - start.time

  long_return(cl, save, expr)
}

#' @export
MethodBatchExperiment_qqplot <- function(expr) {
  assertthat::assert_that(class(expr)[1] == "MethodBatchExperiment")

  L <- ncol(expr$method.batch[[1]]$pvalue)
  d <- nrow(expr$method.batch[[1]]$pvalue)
  col.names <- sapply(1:d, function(i) paste0('pvalue',i))


  toplot <- tibble()
  for (m in expr$method.batch) {
    aux <- tibble::as_tibble(t(m$pvalue))
    colnames(aux) <- col.names
    toplot <- aux %>%
      dplyr::mutate(method = m$nickname) %>%
      rbind(toplot)
  }

  ## print outlier
  if (!is.null(expr$outlier)) {
    aux <- toplot %>%
      group_by(method) %>%
      mutate(id = row_number()) %>%
      arrange(pvalue1) %>%
      mutate(rank  = row_number())%>%
      ungroup() %>%
      dplyr::filter(id %in% expr$outlier)
    print.data.frame(aux)
  }

  ## plot
  toplot <- toplot %>%
    tidyr::gather_(key_col = "pvalue.ind", value_col = "pvalue", gather_cols = col.names) 
  ggplot(toplot, aes(sample = -log10(pvalue))) +
    stat_qq(distribution = stats::qexp, dparams = list(rate = log(10))) +
    geom_abline(slope = 1, intercept = 0) +
    facet_grid(method~pvalue.ind) + 
    ggtitle("-log10(pvalue) qqplot")
}

#' @export
MethodBatchExperiment_calibrate <- function(expr) {
  assertthat::assert_that(class(expr)[1] == "MethodBatchExperiment")

  for (i in seq_along(expr$method.batch)) {
    expr$method.batch[[i]] <- calibrate(expr$method.batch[[i]])
  }
  expr
}
