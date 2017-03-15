#' @export
Article3_cvExp <- function(s,
                           s.name,
                           Ks,
                           lambdas,
                           row.left.out.func = left.out.kfold(5),
                           col.left.out.func = left.out.sample(5, 0.2),
                           cluster.nb = NULL,
                           save = TRUE, bypass = FALSE) {

  ## init
  long_init(cluster.nb = cluster.nb,
            bypass = bypass)
  ## exp
  exp <- Experiment(name = "Article3_cvExp",
                    description = make_description("Article3_cvExp",
                                                   s.name = s.name,
                                                   lambdas = lambdas,
                                                   Ks = Ks))
  class(exp) <- c("Article3_cvExp", class(exp))

  ## run exp
  set.seed(exp$seed)
  dat <- sampl(s)

  ## We remove snip with na
  if (anyNA(dat$G)) {
    DebugMessage("Missing values detected")
    locus.na <- apply(dat$G, 2, anyNA)
    message("== Removing ", mean(locus.na), "% col")
    dat$G <- dat$G[,!locus.na]
  }

  ## cv
  m <- finalLfmmRdigeMethod(K = NULL, lambda = NULL)

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
