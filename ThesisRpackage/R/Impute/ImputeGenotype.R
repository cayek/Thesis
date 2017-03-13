#' impute genotype by mean functor
#'
#'
#'
#' @export
imputeByMean <- function() {
  Functor(fun = function(G) {
    n <- nrow(G)
    L <- ncol(G)
    missing.index <- which(is.na(G))
    # compute mean
    mu <- G %>%
      purrr::array_branch(2) %>%
      purrr::map_dbl(mean, na.rm = TRUE)
    G[missing.index] <- (matrix(1,n,1) %*% matrix(mu,1,L))[missing.index]
    G
  },
  name = "ImputeByMean")
}

#' impute genotype by zero functor
#'
#'
#'
#' @export
imputeByZero <- function() {
  Functor(fun = function(G) {
    n <- nrow(G)
    L <- ncol(G)
    missing.index <- which(is.na(G))
    G[missing.index] <- 0
    G
  },
  name = "ImputeByZero")
}
