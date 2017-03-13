#' TrueDataSet class
#'
#'
#' @export
TrueDataSet <- function(G = matrix(0, 1, 10),
                        X = matrix(0, 2, 1),
                        outlier = c()) {

  d <- DataSet(G = G,
               X = X)
  d$outlier = outlier
  class(d) <- c("TrueDataSet", class(d))
  d
}
