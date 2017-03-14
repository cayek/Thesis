tess3Method <- function(K,
                        sigma = NULL,
                        lambda = 1.0,
                        max.iteration = 200,
                        tolerance = 1e-05,
                        openMP.core.num = 1,
                        Q.init = NULL
                        name = "tess3Method",
                        nickname = NULL) {
  m <- Method(name, hypothteesis.testing.method,
              nickname = nickname)
  class(m) <- c("RidgeLFMMMethod", class(m))
  m$center <- center
  m$K = K
  m$lambda = lambda
  m$reuse.V = reuse.V
  m
}


#' @export
fit.RidgeLFMMMethod <- function(
