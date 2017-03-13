################################################################################
# When you can compute the B.sigma2

#' analytic Sigma2 functor
#'
#' B.Sigma2 was computed during the fit step.
#'
#'
#' @export
AnalyticSigma2Functor <- function() {
  Functor(fun = function(m, dat) {
    # B.sigma2 was already computed
    m$B.sigma2
  },
  name = "AnalyticSigma2")
}

#' Mad Sigma 2 functor
#'
#' B.Sigma2 is calculated as mad(B)
#'
#'
#' @export
MadBSigma2Functor <- function() {
  Functor(fun = function(m, dat) {
    # B.sigma2 was already computed
    sigma <- m$B %>%
      purrr::array_branch(1) %>%
      purrr::map_dbl(mad)
    L <- ncol(m$B)
    d <- nrow(m$B)
    m$B.sigma2 <- matrix(sigma ^ 2, d, 1) %*%
      matrix(1, 1, L)
  },
  name = "AnalyticSigma2")
}

################################################################################
# calibration

#' GifCalibratedZscore functor
#'
#' We assume that zscore~N(0,sd) under H0 so (find ref...)
#' z^2 / gif  ~ chi2(df = 1) with gif = median(z^2) / qchist(0.5)
#'
#'
#' @export
GifCalibratedZscore <- function() {
  Functor(fun = function(score) {
    score2 <- score ^ 2
    gif <- gif(score)
    score2 <- sweep(score2, 1, gif, FUN = "/")
    score2 %>%
      apply(1:2,function(z) pchisq(z, lower.tail = FALSE, df = 1))
  },
  name = "GifCalibratedZscore")
}

#' MadCalibratedZscore functor
#'
#' We assume that zscore~N(0,sd) under H0 and try to estimate sd with the mad
#'
#'
#' @export
MadCalibratedZscore <- function() {
  Functor(fun = function(score) {
    stopifnot(nrow(score) == 1) # what follow not coded for d > 1 (d is the number of co variable)
    sd <- mad(score)
    score %>%
      apply(1:2,function(z) 2 * pnorm(abs(z),lower.tail = FALSE, sd = sd))
  },
  name = "MadCalibratedZscore")
}


#' pvalue computed with fdrtool package
#'
#'
#'
#' @export
FdrtoolsCalibratedZscore <- function() {
  Functor(fun = function(score) {
    pvalue <- score
    for (i in 1:nrow(score)) {
      pvalue[i, ] <- fdrtool::fdrtool(score[i, ])$pval
    }
    pvalue
  },
  name = "FdrtoolsCalibratedZscore")
}

#' normal_zscore functor
#'
#' We assume that zscore~N(0,1) under H0
#'
#'
#' @export
NormalZscore <- function() {
  Functor(fun = function(score) {
    score %>%
      apply(1:2,function(z) 2 * pnorm(abs(z),lower.tail = FALSE))
  },
  name = "NormalZscore")
}

#' return 0
#'
#'
#'
#' @export
zero  <- function(x) {
  0.0
}


################################################################################
# zscore

#' normal_zscore functor
#'
#' score = (B-0) / B.sigma2
#'
#'
#' @export
Zscore <- function(zscorepvalue.functor = NormalZscore(),
                   B.sigma2.functor = AnalyticSigma2Functor(),
                   H0.mean = zero) {
  Functor(fun = function(m, dat) {
    res <- list()
    # compute B.sigma2
    res$B.sigma2 <- B.sigma2.functor$fun(m, dat)
    # compute score and pvalue
    res$score <- (m$B - H0.mean(m$B)) / sqrt(res$B.sigma2)
    # compute p.value
    res$pvalue <- zscorepvalue.functor$fun(res$score)
    res$minus.log.pvalue <- -log(res$pvalue)
    res
  },
  name = "Zscore")
}
