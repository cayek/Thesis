################################################################################
# helpers


make_folds <- function(kfold, m) {

  if (kfold == 1) {
    folds <- rep(factor("[1,m]"),m)
  } else {
    folds <- cut(sample.int(m), breaks = kfold)
  }
  folds
}

errAssumingU <- function(Gtest, U) {
  m <- list(center = TRUE)
  m <- mu(m = m, G = Gtest)
  res <- classicLm(center(m, Gtest), U, center = TRUE)
  mean(res$epsilon ^ 2)
}


################################################################################
# crossvalidation

#' Cross validation on indiv
#'
#' @export
crossvalidation_kfold_indiv <- function(m, dat, kfold, lambdas) {

  res <- list()
  res$errs <- tibble()

  folds <- make_folds(kfold, nrow(dat$G))

  for (interval in levels(folds)) {

    # data
    ind <- folds == interval
    dat.train <- dat
    dat.train$G <- dat.train$G[ind,,drop = FALSE]
    dat.train$X <- dat.train$X[ind,,drop = FALSE]

    dat.test <- dat
    dat.test$G <- dat.test$G[ind,,drop = FALSE]
    dat.test$X <- dat.test$X[ind,,drop = FALSE]

    res$errs  <-
      foreach(lambda = lambdas, .combine = 'rbind') %dopar%
      {
        # method
        method.res <- m
        method.res$lambda <- lambda
        method.res <- fit(method.res, dat.train)
        tibble(lambda = lambda,
               loss = loss(method.res),
               loss.predict =  prediction_error(method.res, dat.test),
               meanUXabscor = UXlink(method.res, dat.test),
               sumUXabscor = UXlink(method.res, dat.test, summary.stat = sum),
               interval = interval)
      } %>%
      rbind(res$errs)
  }
  class(res) <- "CrossValidation"
  res

}


#' Cross validation on locus
#'
#' @export
crossvalidation_kfold_locus <- function(m, dat, kfold, lambdas) {

  res <- list()
  res$errs <- tibble()

  folds <- make_folds(kfold, ncol(dat$G))


  for (interval in levels(folds)) {

    # data
    loc <- folds == interval
    dat.train <- dat
    dat.train$G <- dat.train$G[,!loc,drop = FALSE]

    dat.test <- dat
    dat.test$G <- dat.test$G[,loc,drop = FALSE]

    res$errs  <-
      foreach(lambda = lambdas, .combine = 'rbind') %dopar%
      {
        # method
        method.res <- m
        method.res$lambda <- lambda
        method.res <- fit(method.res, dat.train)
        tibble(lambda = lambda,
               err.assuming.UX = errAssumingU(dat.test$G, cbind(method.res$U, dat$X)),
               interval = interval)
      } %>%
      rbind(res$errs)
  }
  class(res) <- "CrossValidation"
  res

}

#' Cross validation with missing value
#'
#' @export
crossvalidation_kfold_missingvalue <- function(m, dat, rep, missing.prop, lambdas) {

  res <- list()
  res$errs <- tibble()
  L <- ncol(dat$G)
  n <- nrow(dat$G)

  for (r in 1:rep) {

    missing <- sample.int(n * L, n * L * missing.prop)
    dat.missing <- dat
    dat.missing$G[missing] <- NA

    res$errs  <-
      foreach(lambda = lambdas, .combine = 'rbind') %dopar%
      {
        # method
        method.res <- m
        method.res$lambda <- lambda
        method.res <- fit(method.res, dat.missing)
        tibble(lambda = lambda,
               imputation_error = imputation_error(method.res, dat),
               interval = r)
      } %>%
      rbind(res$errs)
  }
  class(res) <- "CrossValidation"
  res

}



################################################################################
# Plots

#' @export
plot.CrossValidation <- function(c, stat = "loss.predict") {

  toplot <- c$errs
  toplot["stat"] <- toplot[stat]
  toplot <- toplot %>%
    group_by(lambda) %>%
    summarise(stat.mean = mean(stat),
              stat.se = sd(stat) / sqrt(length(stat)))

    ggplot(toplot, aes(x = lambda, y = stat.mean)) +
      geom_point() +
      geom_line() +
      geom_errorbar(aes(ymin = stat.mean - stat.se,
                        ymax = stat.mean + stat.se)) +
      scale_x_log10()

}


