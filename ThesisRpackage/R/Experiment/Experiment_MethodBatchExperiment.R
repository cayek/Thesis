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
  expr$G.colnames <- colnames(dat$G)
  if (is.null(expr$G.colnames)) {
    expr$G.colnames <- sapply(1:ncol(dat$G), function(i) paste0('V',i))
  }


  start.time <- Sys.time()
  res <- foreach(m = expr$method.batch) %dopar%
    {
      run(m, dat)
    }

  end.time <- Sys.time()
  expr$runtime <- end.time - start.time

  expr$method.batch <- res

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
    print(expr$method.batch[[i]]$nickname)
    expr$method.batch[[i]] <- calibrate(expr$method.batch[[i]])
  }
  expr
}

#' @export
MethodBatchExperiment_qvalue <- function(expr, threshold) {
  assertthat::assert_that(class(expr)[1] == "MethodBatchExperiment")

  res.df <- tibble()
  for (m in expr$method.batch) {
    res.df <- tibble(pvalue = m$pvalue[1,]) %>%
      mutate(qvalue = qvalue::qvalue(pvalue)$qvalues,
             method = m$nickname,
             varnames = expr$G.colnames,
             index = row_number()) %>%
      rbind(res.df)
  }

  if (!is.null(expr$outlier)) {
    res.df <- res.df %>%
      dplyr::group_by(method) %>%
      dplyr::mutate(outlier = index %in% expr$outlier) %>%
      dplyr::ungroup()
  }

  ## print.data.frame(res.df %>% dplyr::filter(qvalue < threshold))
  res.df
}

#' @export
MethodBatchExperiment_candidates <- function(expr, top = NULL, fdr.threshold = NULL, print = TRUE) {
  assertthat::assert_that(class(expr)[1] == "MethodBatchExperiment")

  res.df <- MethodBatchExperiment_qvalue(expr, NULL)

  if(!is.null(fdr.threshold)) {
    res.df <- res.df %>%
      dplyr::filter(qvalue <= fdr.threshold)
  }

  if(!is.null(top)) {
    res.df <- res.df %>%
      dplyr::group_by(method) %>%
      dplyr::arrange(pvalue) %>%
      dplyr::filter(row_number() <= top) %>%
      dplyr::ungroup()
  }

  ## print
  if (print && !is.null(expr$outlier)) {
  res.df %>%
    group_by(method) %>%
    summarise(nb = n(), observed.fdr = 1 - mean(outlier),
              observed.puissance = sum(outlier) / length(expr$outlier)) %>%
    print.data.frame()
  }

  res.df
}

#' @export
MethodBatchExperiment_count_intersect <- function(expr, top = NULL, fdr.threshold = NULL,
                                                  plot = c("point", "tile")) {
  assertthat::assert_that(class(expr)[1] == "MethodBatchExperiment")

  aux <- MethodBatchExperiment_candidates(expr, top, fdr.threshold)
  aux <- dplyr::inner_join(x = aux, y = aux, by = 'varnames') %>%
    dplyr::group_by(method.x, method.y) %>%
    dplyr::summarise(count = n())

  if (!is.null(plot) && plot == "point") {
    ## from http://stackoverflow.com/questions/32743004/improved-mosaic-plot-like-heatmap-or-bubbles-in-r
    ggplot(aux, aes(method.x, method.y)) +
      geom_point(aes(size = count), alpha=0.8, color="darkgreen", show.legend = FALSE) +
      geom_text(aes(label = count), color="white") +
      scale_size(range = c(15,50)) +
      theme_bw()
  } else if (!is.null(plot) && plot == "tile") {
    ggplot(aux, aes(method.x, method.y)) +
      geom_tile(aes(fill = count)) +
      geom_text(aes(label = count), color="white") +
      scale_x_discrete(expand = c(0,0)) +
      scale_y_discrete(expand = c(0,0)) +
      scale_fill_gradient("Legend label", low = "lightblue", high = "blue") +
      theme_bw()
  } else {
    aux %>%
      tidyr::spread("method.x", "count") %>%
      dplyr::rename(method = method.y)
  }

}
