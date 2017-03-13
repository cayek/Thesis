#' @export
plot.GenerativeDataSet <- function(dat, plot.type = c("grid", "X"), ...) {
  if (plot.type[1] == "grid") {
    cowplot::plot_grid(gplot_VB(dat$V, dat$B, dat$outlier), gplot_UX(dat$U, dat$X), ncol = 1, nrow = 2)
  } else if (plot.type[1] == "X") {
    cat("NOTHING ... \n")
    # toplot tidy_matrix(dat$U, "U"),
    #                  tidy_matrix(dat$X, "X"))
    # ggplot(toplot, aes(x = score, y = factor)) +
    #   geom_point() +
    #   facet_grid()
  }
}

#' @export
name.GenerativeDataSet <- function(obj) {
  paste0("GenerativeDataSet|K=",ncol(obj$V))
}

#' GenerativeDataSet class
#'
#'
#' @export
GenerativeDataSet <- function(n = 1,
                              L = 10,
                              G = matrix(0, 1, L),
                              X = matrix(0, n, 1),
                              U = matrix(0, n, 1),
                              V = matrix(0, L, 1),
                              B = matrix(0, 1, L),
                              epsilon = matrix(0, n, L),
                              outlier = c(),
                              mu = matrix(0, 1, L)) {

  d <- DataSet(G = G,
               X = X)
  d$U = U
  d$V = V
  d$B = B
  d$epsilon = epsilon
  d$outlier = outlier
  d$mu = mu
  class(d) <- c("GenerativeDataSet", class(d))
  d
}
