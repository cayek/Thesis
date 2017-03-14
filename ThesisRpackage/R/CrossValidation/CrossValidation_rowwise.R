#' return a list of train/test indices
#'
#' @export
left.out.sample <- function(rep, prop) {
  function(J) {
    n.sample <- J * prop
    folds <- list()
    for (r in 1:rep) {
      folds[[r]] <- sample.int(J, n.sample)
    }
    folds
  }
}

#' return a list of train/test indices
#'
#' @export
left.out.kfold <- function(kfold) {
  function(J) {
    if (kfold == 1) {
      cuts <- rep(factor("[1,m]"), J)
    } else {
      cuts <- cut(sample.int(J), breaks = kfold)
    }
    folds <- list()
    for (l in seq_along(levels(cuts))) {
      folds[[l]] <- which(cuts == levels(cuts)[l])
    }
    folds
  }
}


#' Cross validation on row
#'
#' @export
CrossValidation_rowwise <- function(m, dat, row.left.out.func, col.left.out.func, lambdas, Ks) {

  res <- list()
  n <- nrow(dat$G)
  L <- ncol(dat$G)

  ## main loops
  res$errs  <-
    foreach(lambda = lambdas, .combine = 'rbind') %:%
    foreach(K = Ks, .combine = 'rbind') %dopar%
    {
      errs <- tibble()
      DebugMessage(paste0("lambda = ", lambda, "|K = ", K))
      ## row folds
      row.folds <- row.left.out.func(n)
      for (row.fold.ind in seq_along(row.folds)) {
        row.fold <- row.folds[[row.fold.ind]]

        ## train/test
        dat.train <- dat
        dat.train$G <- dat.train$G[-row.fold,,drop = FALSE]
        dat.train$X <- dat.train$X[-row.fold,,drop = FALSE]
        dat.test <- dat
        dat.test$G <- dat.test$G[row.fold,,drop = FALSE]
        dat.test$X <- dat.test$X[row.fold,,drop = FALSE]

        ## method
        method.res <- m
        method.res$lambda <- lambda
        method.res$K <- K

        ## fit method
        method.res <- fit(method.res, dat.train)

        ## compute err
        col.folds <- col.left.out.func(L)
        err <- c()
        for (col.fold in col.folds) {
          G_i_pred <- Predict_row(m = method.res,
                      X_i = dat.test$X,
                      G_i = dat.test$G,
                      j.assumed = (1:L)[-col.fold],
                      j.unknow = col.fold)
          err <- c(mean((G_i_pred - dat.test$G[,col.fold]) ^2), err)
        }
        errs <- tibble(lambda = lambda,
                       K = K,
                       PRESS =  mean(err),
                       row.fold.ind = row.fold.ind) %>%
          rbind(errs)
      }
      errs
    }

  class(res) <- "CrossValidation"
  res
}
