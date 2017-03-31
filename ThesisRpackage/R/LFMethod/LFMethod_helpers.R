#' Compute the column wise mean if center is true
mu <- function(m, G) {
  L <- ncol(G)

  if (m$center) {
    m$mu <- matrix(G %>% purrr::array_branch(2) %>%
                     purrr::map_dbl(mean, na.rm = TRUE),
                   1, L)
  }
  m
}

#' Center if center is true
center <- function(m, G) {

  if (m$center) {
    G_ <- sweep(G, 2, m$mu)
  } else {
    G_ <- G
  }
  G_
}

#' @export
gif <- function(score) {
  score2 <- score ^ 2
  apply(score2, 1, median, na.rm = TRUE) / qchisq(0.5, df = 1)
}

#' @export
missingValueImputationLoop <- function(m, G_, update.func, dat, reuse) {

  ## param
  n <- nrow(dat$G)
  L <- ncol(dat$G)
  d <- ncol(dat$X)

  if (anyNA(G_)) {
    flog.debug("missingValueImputationLoop: Missing values detected")
    any.missing <- TRUE
    m$missing.index <- which(is.na(dat$G))
    G_[m$missing.index] <- sample(dat$G[-m$missing.index], length(m$missing.index)) # maybe we can impute at random column wise ?
  } else {
    any.missing <- FALSE
    m$missing.index <- c()
  }


 # init algo
  if (!reuse) {
    m$B <- matrix(0, d, L)
    m$C <- matrix(0, n, L)
  }
  it <- 0
  m$epsilon <- G_ - dat$X %*% m$B - m$C
  err.new <- mean((m$epsilon) ^ 2)
  stop <- FALSE
  # main loop
  while ((it < m$it.max) && !stop) {

    flog.debug(paste("missingValueImputationLoop: it = ",it, "| err = ", err.new))
    err.old <- err.new
    it <- it + 1

    m <- update.func(m, G_, dat)

    ## calculate epsilon
    est <- dat$X %*% m$B + m$C
    m$epsilon <- G_ - est
    m$imputed.values <- est[m$missing.index]

    ## impute if necessary
    if (any.missing) {
      G_[m$missing.index] <- m$imputed.values
    }

    err.new <- mean(m$epsilon ^ 2)

    if ((abs(err.old - err.new) / err.old) < m$err.max) {
      stop <- TRUE
    }
  }
  m

}
