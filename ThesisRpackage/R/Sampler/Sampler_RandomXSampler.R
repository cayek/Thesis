#' Sample random X variable.
#'
#'
#' @export
RandomXSampler <- function(sampler, d = 1,
                           X.sampler = function(n,d) MASS::mvrnorm(n = n,
                                                                   Sigma = 1 * diag(1,d,d),
                                                                   mu = rep(0,d))) {
  s <- Sampler()
  s$sampler <- sampler
  s$X.sampler <- X.sampler
  s$d <- d
  class(s) <- c("RandomXSampler", class(s))
  s
}


#' @export
sampl.RandomXSampler <- function(s){
  dat <- sampl(s$sampler)
  dat$X <- s$X.sampler(nrow(dat$G), s$d)
  dat$outlier <- c()
  dat
}
