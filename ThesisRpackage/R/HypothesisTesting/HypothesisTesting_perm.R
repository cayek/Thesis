################################################################################
# With Permutation

#' We permute X
#'
#' for simplicity we assume that ncol(X) = 1
#'
#'   @export
PermutationZscore <- function(perm.nb) {
  Functor(fun = function(m, dat) {
    stopifnot(ncol(dat$X) == 1) # what follow not coded for d > 1 (d is the number of co variable)
    res <- list()
    # param
    L <- ncol(dat$G)
    n <- nrow(dat$G)
    d <- ncol(dat$X)

    # run permutation
    B.perm <- array(NA, dim = c(d, L, perm.nb))
    for (p in 1:perm.nb) {
      DebugMessage(paste("== Perm it:", p,"/", perm.nb))
      dat$X <- dat$X[sample.int(n, replace = TRUE),, drop = FALSE] # permutation of X
      m.perm <- fit(m, dat, reuse = TRUE)
      B.perm[,,p] <- m.perm$B
    }
    res <- compute_mean_var_B_sample(B.perm)

    # score, we assume that sigma2 under H0 is the same that not under H0
    res$score <- (m$B - 0) / sqrt(res$B.sigma2)

    # compute p.value
    index <- 1:ncol(m$B) # can do that because d = 1
    res$pvalue <- index %>%
      sapply(function(i) 2 * pnorm(abs(m$B[i]),
                                   lower.tail = FALSE,
                                   mean = res$B.mean[i],
                                   sd = sqrt(res$B.sigma2[i])))
    res$pvalue <- matrix(res$pvalue, 1, ncol(m$B))
    res$minus.log.pvalue <- -log(res$pvalue)
    res


  },
  name = "Bootstrap")
}

