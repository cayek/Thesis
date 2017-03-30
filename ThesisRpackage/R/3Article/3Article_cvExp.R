#' @export
Article3_cvExp <- function(dat,
                           dat.name,
                           Ks,
                           lambdas,
                           row.left.out.func = left.out.kfold(5),
                           col.left.out.func = left.out.sample(5, 0.2),
                           cluster.nb = NULL,
                           m = finalLfmmRdigeMethod(K = NULL, lambda = NULL),
                           save = TRUE, bypass = FALSE) {

  ## init
  long_init(cluster.nb = cluster.nb,
            bypass = bypass)
  ## exp
  exp <- Experiment(name = "Article3_cvExp",
                    description = make_description("Article3_cvExp",
                                                   dat.name = dat.name,
                                                   lambdas = lambdas,
                                                   Ks = Ks,
                                                   m = m$nickname))
  class(exp) <- c("Article3_cvExp", class(exp))

  ## We remove snip with na
  if (anyNA(dat$G)) {
    DebugMessage("Missing values detected")
    locus.na <- apply(dat$G, 2, anyNA)
    message("== Removing ", mean(locus.na), "% col")
    dat$G <- dat$G[,!locus.na]
  }



  ## crossvalid lambda
  exp$cv <- CrossValidation_rowwise(m, dat,
                                    row.left.out.func = row.left.out.func,
                                    col.left.out.func = col.left.out.func,
                                    lambdas = lambdas,
                                    Ks = Ks)

  ## save exp
  if (save) {
    dumpExperiment(exp)
  }
  exp
}
