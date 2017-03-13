################################################################################
# Helpers

#' compute lambda_max such as B = 0
lambda_max <- function(G_, dat, d, L, m) {
  # C
  svd.res <- svd(G_, nu = m$K, nv = m$K)
  m$U <- svd.res$u %*% diag(svd.res$d[1:m$K], m$K, m$K)
  m$V <- svd.res$v
  m$C <- tcrossprod(m$U, m$V) # init C with Ksvd

  # B
  B <- B_ridge(A = G_ - m$C, X = dat$X, lambda = 0.0)
  m$B <- matrix(0, d, L) # B init with 0
  m$lambda.max <- max(B)

  m
}


#' main loop of lfmm lasso
LassoLFMM_main <- function(m, G_, dat, lambda) {

  it <- 0
  m$epsilon <- G_ - dat$X %*% m$B - m$C
  err.new <- mean((m$epsilon) ^ 2)
  stop <- FALSE
  # main loop
  while ((it < m$it.max) && !stop) {

    DebugMessage(paste("it = ",it, "| err = ", err.new, "\n"))
    err.old <- err.new
    it <- it + 1

    ## calculate C
    svd.res <- svd(G_ - dat$X %*% m$B, nu = m$K, nv = m$K)
    m$U <- svd.res$u %*% diag(svd.res$d[1:m$K], m$K, m$K)
    m$V <- svd.res$v
    m$C <- tcrossprod(m$U, m$V)
    m$C.nuclear.norm <- sum(svd.res$d[1:m$K])

    ## calculate B
    m$B <- B_lasso(A = G_ - m$C, X = dat$X, lambda = lambda)

    ## calculate epsilon
    est <- dat$X %*% m$B + m$C
    m$epsilon <- G_ - est

    err.new <- mean(m$epsilon ^ 2)

    if ((abs(err.old - err.new) / err.old) < m$err.max) {
      stop <- TRUE
    }
  }
  m
}



################################################################################
# LassoLFMMMethod

#' ||G  - U V^t - X B || + lambda |B|
#'
#' @export
LassoLFMMMethod <- function(K,
                            it.max,
                            err.max,
                            lambda = 0.0, # if null regularization path
                            lambda.K = 100, # default value used in Friedman et al. 2010
                            lambda.eps = 0.001, # default value used in Friedman et al. 2010
                            sparse.prop = NULL, # try to find the lambda such that not null lambda proportion equal this param
                            center = TRUE,
                            name = "LassoLFMMMethod",
                            nickname = NULL,
                            hypothesis.testing.method = NULL) {
  m <- Method(name,
              hypothesis.testing.method = hypothesis.testing.method,
              nickname = nickname)
  class(m) <- c("LassoLFMMMethod", class(m))
  m$center <- center
  m$K = K
  m$lambda = lambda
  m$lambda.K = lambda.K
  m$lambda.eps = lambda.eps
  m$it.max = it.max
  m$err.max = err.max
  m$sparse.prop = sparse.prop
  m
}


#' @export
fit.LassoLFMMMethod <- function(m, dat, reuse = FALSE) {

  n <- nrow(dat$G)
  L <- ncol(dat$G)
  d <- ncol(dat$X)
  one <- matrix(1, n, 1)

  # compute center
  if (m$center) {
    m$mu <- matrix(dat$G %>% purrr::array_branch(2) %>%
                     purrr::map_dbl(mean, na.rm = TRUE),
                   1, L)
    G_ <- dat$G - one %*% m$mu
  } else {
    G_ <- dat$G
  }

  # impute missing value
  if (anyNA(G_)) {
    DebugMessage("Missing values detected")
    G_ <- m$impute.genotype.method$fun(G_)
  }

  # init algo
  if (!reuse) {
    m$B <- matrix(0, d, L)
    # m$B <- matrix(rnorm(d * L), d, L)
    m$C <- matrix(0, n, L)
  }

  if (is.null(m$lambda)) {
    # reg path

    m <- lambda_max(G_ = G_, dat = dat, d = d, L = L, m = m)

    # lambda.min = lambda.eps * lambda.max like in Friedman et al. 2010
    m$lambda.min <- m$lambda.eps * m$lambda.max
  } else {
    m$lambda.max <- m$lambda
    m$lambda.min <- m$lambda
    m$lambda.K <- 1
  }

  lambdas <- seq(m$lambda.max, m$lambda.min, length.out = m$lambda.K)

  B.all <- array(dim = c(d, L, m$lambda.K)) # keep all B result

  for (i in seq_along(lambdas)) {
    # main loop
    DebugMessage(paste0("loop number ",i," / ", m$lambda.K, "|lambda = ",lambdas[i]))
    m <- LassoLFMM_main(m = m,
                        G_ = G_,
                        dat = dat,
                        lambda = lambdas[i])
    B.all[,,i] <- m$B

    if (!is.null(m$sparse.prop) && (mean(m$B != 0.0) >= m$sparse.prop)) break() #if a sparse proportion is define, we leave the loop if this proportion is reached

  }

  # compute analytic variance
  m$epsilon.sigma2 <- epsilon.sigma2(m$epsilon,
                                     reduced.df = ifelse(m$center,1,0))


  # B.sigma2 to one can not be computed

  # score
  m$score <- B.all %>%
    apply(1:2, function(r) length(which(r != 0.0))) # number of time the coef is not null
  # pvalue: artificial, just a way to have same output than other methods !!!
  m$pvalue <- t(m$score %>%
                apply(1, function(r) (max(r) - r) / (max(r) - min(r))))

  # return
  m
}

#' @export
run.LassoLFMMMethod <- function(m, dat) {

  m <- fit.LassoLFMMMethod(m, dat)
  run.Method(m, dat)
}

