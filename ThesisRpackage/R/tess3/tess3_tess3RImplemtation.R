################################################################################
# helpers
computeXBin <- function(X, ploidy) {
  L <- ncol(X)
  n <- nrow(X)
  XBin <- matrix(as.raw(0), n, L * (ploidy + 1))
  D <- ploidy + 1
  for (d in (0:(D - 1))) {
    XBin[,(1:(D * L) %% D) == d] = as.raw(X == d)
  }
  XBin
}


computeGraphLaplacian <- function(W) {
  D <- diag(apply(W,1,sum))
  return(D - W)
}

ProjectQ <- function(Q) {
  Q <- apply(Q, 1:2, function(e){max(e,1e-5)})
  for (i in 1:nrow(Q)) {
    s <- sum(Q[i,])
    if (s != 0.0) {
      Q[i,] <- Q[i,] / s
    }
  }
  return(Q)
}

ProjectG <- function(G, L, D) {
  G <- apply(G, 1:2, function(e){max(e,1e-5)})
  for (k in 1:ncol(G)) {
    for (l in 1:L) {
      s <- sum(G[(1 + D * (l - 1)):(D * l) ,k])
      if (s != 0.0) {
        G[(1 + D * (l - 1)):(D * l) ,k] <- G[(1 + D * (l - 1)):(D * l) ,k] / s
      }
    }
  }
  return(G)
}

computeRXi <- function(Ri, XBin) {
  res <- apply(XBin, 2, function(c) crossprod(Ri, as.double(c)))
  matrix(res, ncol(XBin), 1)
}

computeXBinNorm <- function(XBin, Q, G) {
  aux <- sapply(1:ncol(XBin),
                function(j) sum((as.double(XBin[j]) - tcrossprod(Q, G[j,, drop = FALSE])) ^ 2))
  sqrt(sum(aux))
}

################################################################################
# tess3R

#' @export
tess3RImplementationMethod <- function(K,
                        sigma = NULL,
                        lambda = 1.0,
                        max.iteration = 200,
                        tolerance = 1e-05,
                        Q.init = NULL,
                        name = "tess3RImplementationMethod",
                        nickname = NULL) {
  m <- Method(name = name,
              nickname = nickname)
  class(m) <- c("tess3RImplementationMethod", "tess3Method", class(m))
  m$K = K
  m$lambda = lambda
  m$sigma = sigma
  m$max.iteration = max.iteration
  m$tolerance = tolerance
  m$Q.init = Q.init

  m
}


#' @export
fit.tess3RImplementationMethod <- function(m, dat) {

  assertthat::assert_that(!is.null(dat$coord))

  ## W
  if (is.null(m$sigma)) {
    dat$W <- tess3r::ComputeHeatKernelWeight(dat$coord, tess3r.env$ComputeMeanDist(dat$coord) * 0.05)
  }

  ## ploidy
  ploidy <- computePloidy(dat$G)

  ## compute XBin
  DebugMessage("Compute XBin")
  XBin <- computeXBin(dat$G, ploidy)

  DebugMessage("Compute Lapl")
  Lapl <- computeGraphLaplacian(dat$W)

  L = ncol(dat$G)
  D <- ploidy + 1
  n <- nrow(dat$G)
  K <- m$K

  DebugMessage("Compute Lapl vp")
  ei <- eigen(Lapl, symmetric = TRUE)
  vps = ei$values
  R = t(ei$vectors)

  DebugMessage("Compute lambda")
  vpMax = max(vps)
  lambda = 0.0
  if (vpMax != 0.0) {
    lambda = m$lambda * (D * L * n) / (K * n * vpMax);
  }

  ## constant
  Ik <- diag(1, K, K)

  ## auxiliary variables
  err = -10.0;
  errAux = 0.0;

  ## variables
  Q <- matrix(runif(n * K), n, K)
  Q <- ProjectQ(Q)
  G <- matrix(0, L * D, K)
  XBin.norm <- computeXBinNorm(XBin,
                               Q = matrix(0, n, K),
                               G = matrix(0, L * D, K))
  it = 0;
  converg = FALSE;
  DebugMessage("main loop")
  while (!converg && it < m$max.iteration) {
    ## update G


    G <- foreach(j = 1:nrow(G), .combine = 'rbind') %dopar%
    {
      t(solve(crossprod(Q), crossprod(Q, as.double(XBin[,j]))))
    }
    G <- ProjectG(G, L, D)

    ## update Q
    RQ <- foreach(i = 1:nrow(Q), .combine = 'rbind') %dopar%
    {
      RXi <- computeRXi(R[i,], XBin)
      t(solve(crossprod(G) + lambda * vps[i] * Ik, crossprod(G, RXi)))
    }

    Q <- t(R) %*% RQ
    Q <- ProjectQ(Q)

    ## compute normalized residual error
    errAux = computeXBinNorm(XBin, Q, G) / XBin.norm
    DebugMessage(paste0("-- it ", it, " max it ", m$max.iteration))
    converg = (abs(errAux - err) < m$tolerance)
    err = errAux
    it <- it + 1

  }


  m$Q <- Q
  class(m$Q) <- "tess3Q"
  m$G <- G
  class(m$G) <- "tess3G"
  m
}
