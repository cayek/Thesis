#' use G_i[j.assumed] to predict G_i[j.unknow]
#'
#' @export
Predict_row <- function(m, X_i, G_i, j.assumed, j.unknow) {
  UseMethod("Predict_row")
}

#' @export
Predict_row.LFMMMethod <- function(m, X_i, G_i, j.assumed, j.unknow) {

  n <- nrow(G_i)

  # center G_i
  G_i_ <- sweep(G_i, MARGIN = 2, m$mu)

  # Compute U
  ## center
  U_i_j <- (G_i_[,j.assumed,drop = FALSE] - X_i %*% m$B[,j.assumed, drop = FALSE]) %*%
    m$V[j.assumed, ,drop = FALSE]

  G_i_pred <- matrix(1, n, 1) %*% m$mu[,j.unknow, drop = FALSE] +
    tcrossprod(U_i_j, m$V[j.unknow,,drop = FALSE]) + X_i %*% m$B[,j.unknow, drop = FALSE]

  G_i_pred
}
