Article3_runExp_main <- function(m, dat, lambdas, Ks, r) {
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
      qval <- qvalue::qvalue(pval)$qvalue

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
             rep = r)
    }
}


#' @export
Article3_runExp <- function(s,
                            s.name,
                            Ks,
                            lambdas,
                            nb.rep,
                            m = finalLfmmRdigeMethod(K = NULL,
                                                     lambda = NULL,
                                                     calibrate = TRUE),
                            m.name = "finalLfmmRdigeMethod",
                            cluster.nb = NULL,
                            save = TRUE, bypass = FALSE) {

  ## init
  long_init(cluster.nb = cluster.nb,
            bypass = bypass)
  ## exp
  exp <- Experiment(name = "Article3_runExp",
                    description = make_description("Article3_runExp",
                                                   m.name = m.name,
                                                   s.name = s.name,
                                                   lambdas = lambdas,
                                                   Ks = Ks))
  class(exp) <- c("Article3_runExp", class(exp))

  ## main
  exp$df.res <- tibble()
  for (r in 1:nb.rep) {
    dat <- sampl(s)
    assertthat::assert_that(ncol(dat$X) == 1)
    exp$df.res <- Article3_runExp_main(m, dat, lambdas, Ks, r) %>%
      rbind(exp$df.res)
  }


  ## save exp
  if (save) {
    dumpExperiment(exp)
  }
  exp
}


#' @export
plot.Article3_runExp <- function(exp, threshold) {
  ggplot(exp$df.res, aes(x = index, y = B, color = qvalue < threshold)) +
    geom_point() +
    facet_grid(lambda ~ K, scales = "free")
}
