Article3_runExp_main <- function(m, dat) {

  m <- run(m, dat)

  ## compute qvalue
  B <- as.numeric(m$B)
  score <- as.numeric(m$score)
  pval <- as.numeric(m$pvalue)
  out <- capture.output(qval <- qvalue::qvalue(pval)$qvalue)
  DebugMessage("qvalue", out)

  ## colname
  col.name <- colnames(dat$G)
  index = 1:ncol(dat$G)
  if (is.null(col.name)) {
    col.name <- sapply(index, function(i) paste0("V",i))
  }

  tibble(index = index,
         col.name = col.name,
         pvalue = pval,
         score = score,
         B = B,
         qvalue = qval,
         method = strsplit(m$nickname, "\\|")[[1]][1],
         lambda = ifelse(!is.null(m$lambda),m$lambda, NA),
         K = ifelse(!is.null(m$K),m$K, NA),
         sparse.prop = ifelse(!is.null(m$sparse.prop),m$sparse.prop, NA))
}


#' @export
Article3_runExp <- function(dat,
                            dat.name,
                            methods,
                            cluster.nb = NULL,
                            save = TRUE, bypass = FALSE) {

  ## init
  cl <- long_init(cluster.nb = cluster.nb,
            bypass = bypass)

  assertthat::assert_that(ncol(dat$X) == 1)

  ## exp
  exp <- Experiment(name = "Article3_runExp")

  class(exp) <- c("Article3_runExp", class(exp))

  ## main
  exp$df.res <-foreach (m = methods, .combine = 'rbind') %dopar%
    {
      Article3_runExp_main(m, dat)
    }

  ## description
  exp$description = make_description("Article3_runExp",
                                     methods = unique(exp$df.res$method),
                                     lambdas = unique(exp$df.res$lambda),
                                     Ks = unique(exp$df.res$K),
                                     sparse.prop = unique(exp$df.res$sparse.prop),
                                     dat.name = dat.name)

  ## return
  long_return(cl, save, exp)

}


#' @export
Article3_runExp_plotB <- function(exp, threshold, lambda) {

  assertthat::assert_that(exp$name == "Article3_runExp")

  toplot <- exp$df.res %>%
    dplyr::filter(lambda == lambda)
  ggplot(toplot, aes(x = index, y = B, color = qvalue < threshold)) +
    geom_point() +
    facet_grid(method ~ K, scales = "free")

}

#' @export
Article3_runExp_manhattan <- function(exp, threshold, lambda) {

  assertthat::assert_that(exp$name == "Article3_runExp")

  toplot <- exp$df.res %>%
    dplyr::filter(lambda == lambda)
    ggplot(toplot, aes(x = index, y = -log10(pvalue), color = qvalue < threshold)) +
      geom_point() +
      facet_grid(method ~ K, scales = "free")
}
