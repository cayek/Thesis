#' Sample a normaly distributed variable X such that cor(X,Y) = c
#'
#'
#'
sample_correlated_X <- function(Y, c, mu = 0.0, sd = 1.0 ) {
  n = length(Y)
  # Generate X
  theta <- acos(c)             # corresponding angle
  aux2    <- rnorm(n, mu, sd)      # new random data
  aux     <- cbind(Y, aux2)         # matrix
  auxctr  <- scale(aux, center = TRUE, scale = FALSE)   # centered columns (mean 0)

  Id   <- diag(n)                               # identity matrix
  Q    <- qr.Q(qr(auxctr[ , 1, drop = FALSE]))      # QR-decomposition, just matrix Q
  P    <- tcrossprod(Q)          # = Q Q'       # projection onto space defined by x1
  aux2o  <- (Id - P) %*% auxctr[ , 2]                 # x2ctr made orthogonal to x1ctr
  auxc2  <- cbind(auxctr[ , 1], aux2o)                # bind to matrix
  auxY    <- auxc2 %*% diag(1/sqrt(colSums(auxc2^2)))  # scale columns to length 1

  X <- auxY[ , 2] + (1 / tan(theta)) * auxY[ , 1]     # final new vector
  X <- (X - mean(X))/sd(X)# ~ N(0,1)
  X <- X * sd + mu # ~ N(mu,sd)
  return(X)

}

#' Sample X such that X = Sum(X_i) where i are corralated with Ui
#'
#'
#'
sample_X_sum_correlated_U <- function(U, cs) {
  X <- matrix(0, nrow(U), 1)
  for (i in seq_along(cs)) {
    if (!is.null(cs[[i]])) {
      X <- X +
        sample_correlated_X(Y = U[,i],
                            c = cs[[i]],
                            mu = mean(U[,i]),
                            sd = sd(U[,i]))
    }
  }
  X
}

#' Sample P(X| abs(X) > rho * sd ) ~ N(0,sd)
#'
#'
#'
sample_not_null_rnorm <- function(n, sd, rho) {
  res <- 1:n
  for (i in seq_along(res)) {
    res[i] <- rnorm(1, mean = 0, sd = sd)
    while (abs(res[i]) < rho * sd) {
      res[i] <- rnorm(1, mean = 0, sd = sd)
    }
  }
  res
}

#' run prcomp or load a file
#'
compute_PCA <- function(s, G) {

  pca <- NULL
  ## try to read
  if (!is.null(s$pca.file) && file.exists(s$pca.file)) {
      flog.trace("Reading pca from", s$pca.file)
      pca <- read_all(s$pca.file)
  } 

  ## compute if necessary
  if (is.null(pca)) {
    flog.trace("Compute PCA")
    pca <- prcomp(G)
    ## write if file
    if (!is.null(s$pca.file)) {
      flog.trace("Writting file", s$pca.file)
      saveRDS(pca, s$pca.file)
    }
  }

  pca
}
