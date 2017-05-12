TrueDataSet.builder <- setRefClass("TrueDataSet", contains = "DataSet",
                                   fields = c("outlier"))

#' TrueDataSet class
#'
#'
#' @export
TrueDataSet <- function(G = matrix(0, 1, 10),
                        X = matrix(0, 2, 1),
                        outlier = c(),
                        reference = FALSE) {
  
  if (reference) {
    TrueDataSet.builder$new(G = G,
                            X = X,
                            outlier = outlier)
  } else {
    d <- DataSet(G = G,
                 X = X)
    d$outlier = outlier
    class(d) <- c("TrueDataSet", class(d))
    d
  }
}

