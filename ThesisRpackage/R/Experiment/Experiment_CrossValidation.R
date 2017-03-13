#' An experiment to run crossvalidation on a method
#'
#'
#' @export
CrossValidationExperiment <- function(m, s,
                                      lambdas, kfold,
                                      crossvalidation.func = crossvalidation_kfold_indiv,
                                      description = paste("Cross validation of ", s$nickname, "by", m$nickname, "for lambda =",
                                                          paste(lambdas,collapse = '|'))) {
  exp <- Experiment(name = "CrossValidationExperiment",
                    description = description)
  class(exp) <- c("CrossValidationExperiment", class(exp))
  exp$s <- s
  exp$m <- m
  exp$lambdas <- lambdas
  exp$kfold <- kfold
  exp$crossvalidation.func <- crossvalidation.func
  exp
}


#' @export
runExperiment.CrossValidationExperiment <- function(exp) {

  # set the seed
  DebugMessage("We set the seed")
  set.seed(exp$seed)

  dat <- sampl(exp$s)

  # We remove snip with na
  locus.na <- apply(dat$G, 2, anyNA)
  DebugMessage("Remove of NA locus")
  dat$G <- dat$G[,!locus.na]

  # crossvalidation
  DebugMessage("Run of cross validation")
  exp$crossvalidation.res <- exp$crossvalidation.func(m = exp$m,
                                                      dat = dat,
                                                      kfold = exp$kfold,
                                                      lambdas = exp$lambdas)

  exp
}


#' @export
plot.CrossValidationExperiment <- function(exp, stat = "loss.predict") {
  plot.CrossValidation(exp$crossvalidation.res, stat)
}
