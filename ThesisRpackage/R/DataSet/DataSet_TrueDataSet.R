TrueDataSet.builder <- setRefClass("TrueDataSet", contains = "DataSet",
                                   fields = c("outlier", "ind.clumping"))

#' TrueDataSet class
#'
#'
#' @export
TrueDataSet <- function(G = matrix(0, 1, 10),
                        X = matrix(0, 2, 1),
                        outlier = c(),
                        ind.clumping = NULL,
                        reference = FALSE) {
  
  if (reference) {
    TrueDataSet.builder$new(G = G,
                            X = X,
                            outlier = outlier,
                            ind.clumping)
  } else {
    d <- DataSet(G = G,
                 X = X)
    d$outlier = outlier
    d$ind.clumping = ind.clumping
    class(d) <- c("TrueDataSet", class(d))
    d
  }
}

