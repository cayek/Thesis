################################################################################
# With Bootstrap

#' boostrap method described in 8.3 of http://www.stat.ncsu.edu/people/lu/courses/ST505/Ch8.pdf
#'
#' @export
PairedBoostrap <- function(dat, m) {
  n <- nrow(dat$G)
  sample.index <- sample.int(n, replace = TRUE)
  dat$G <- dat$G[sample.index,, drop = FALSE]
  dat$X <- dat$X[sample.index,, drop = FALSE]
  dat
}

#' boostrap method described in 8.3 of http://www.stat.ncsu.edu/people/lu/courses/ST505/Ch8.pdf
#'
#' @export
residualBootstrapLFMM <- function(dat, m) {
  n <- nrow(dat$G)
  sample.index <- sample.int(n, replace = TRUE)
  dat$U <- m$U[sample.index,, drop = FALSE]
  dat$epsilon <- m$epsilon[sample.index,, drop = FALSE]
  # we create a new G
  dat$G <- matrix(1, n, 1) %*% m$mu + tcrossprod(dat$U, m$V) +
    dat$X %*% m$B +
    dat$epsilon
  dat
}

#' boostrap method described in 8.3 of http://www.stat.ncsu.edu/people/lu/courses/ST505/Ch8.pdf
#'
#' We sample a dataset with missing value at same place than in G
#'
#' @export
residualBootstrapLFMMWithMissing <- function(dat, m) {
  n <- nrow(dat$G)
  sample.index <- sample.int(n, replace = TRUE)
  dat$U <- m$U[sample.index,, drop = FALSE]
  dat$epsilon <- m$epsilon[sample.index,, drop = FALSE]
  # we create a new G
  dat$G <- matrix(1, n, 1) %*% m$mu + tcrossprod(dat$U, m$V) +
    dat$X %*% m$B +
    dat$epsilon
  if (!is.null(dat$missing.index)) {
    dat$G[dat$missing.index] <- NA
  }
  dat
}

#' boostrap method described in 8.3 of http://www.stat.ncsu.edu/people/lu/courses/ST505/Ch8.pdf
#'
#' @export
residualBootstrapLM <- function(dat, m) {
  n <- nrow(dat$G)
  sample.index <- sample.int(n, replace = TRUE)
  dat$epsilon <- m$epsilon[sample.index,, drop = FALSE]
  # we create a new G
  dat$G <- matrix(1, n, 1) %*% m$mu + dat$X %*% m$B +
    dat$epsilon
  dat
}

#' @export
bootstrapIt <- function(dat, m, bootstrap.func, bootstrap.rep) {
  res <- list()
  # param
  L <- ncol(dat$G)
  d <- ncol(dat$X)

  # run bootstrap
  res$B.sample <- array(NA, dim = c(d, L, bootstrap.rep))
  for (p in 1:bootstrap.rep) {
    DebugMessage(paste("== Boot it:", p,"/", bootstrap.rep))
    dat.boot <- bootstrap.func(dat, m)
    m.boot <- fit(m, dat.boot, reuse = TRUE)
    res$B.sample[,,p] <- m.boot$B
  }
  tmp <- compute_mean_var_B_sample(res$B.sample)
  res[names(tmp)] <- tmp
  res
}

#' normal_zscore functor with boostrap
#'
#' B.Sigma2 are computed with bootstrap.func.
#' We assume that zscore~N(0,1) under H0
#'
#'
#' @export
Bootstrap <- function(bootstrap.func, bootstrap.rep) {
  Functor(fun = function(m, dat) {
    bootstrapIt(dat, m, bootstrap.func, bootstrap.rep)$B.sigma2
  },
  name = "Bootstrap")
}
