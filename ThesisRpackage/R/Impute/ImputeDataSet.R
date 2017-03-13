#' @export
imputeMeanDataSet <- function(dat) {
  dat$G <- imputeByMean()$fun(dat$G)
  dat$missing.index <- c()
  dat$missing.value <- c()
  dat
}

#' @export
imputeZeroDataSet <- function(dat) {
  dat$G <- imputeByZero()$fun(dat$G)
  dat$missing.index <- c()
  dat$missing.value <- c()
  dat
}
