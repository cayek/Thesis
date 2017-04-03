Article3_runExp_main <- function(m, dat, lambdas, Ks) {
  foreach(lambda = lambdas, .combine = 'rbind') %:%
    foreach(K = Ks, .combine = 'rbind') %dopar%
    {
      m.tmp <- m
      m.tmp$lambda <- lambda
      m.tmp$K <- K

      m.tmp <- run(m.tmp, dat)

      ## compute qvalue
      B <- as.numeric(m.tmp$B)
      score <- as.numeric(m.tmp$score)
      pval <- as.numeric(m.tmp$pvalue)
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
             lambda = lambda,
             K = K,
             pvalue = pval,
             score = score,
             B = B,
             qvalue = qval,
             method = m$nickname)
    }
}


#' @export
Article3_runExp <- function(dat,
                            dat.name,
                            Ks,
                            lambdas,
                            methods = list(ridgeLfmm = finalLfmmRdigeMethod(K = NULL,
                                                                            lambda = NULL,
                                                                            calibrate = TRUE)),
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
  exp$df.res <- tibble()
  for (m in methods) {
    exp$df.res <- Article3_runExp_main(m, dat, lambdas, Ks) %>%
      rbind(exp$df.res)
  }

  ## description
  exp$description = make_description("Article3_runExp",
                                     methods = unique(exp$df.res$method),
                                     dat.name = dat.name,
                                     lambdas = lambdas,
                                     Ks = Ks)

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
