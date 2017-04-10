################################################################################
# Helpers

compute_mean_var_B_sample <- function(B.sample) {
  res <- list()
  d <- dim(B.sample)[1]
  L <- dim(B.sample)[2]
  res$B.mean <- matrix(NA, d, L)
  res$B.sigma2 <- matrix(NA, d, L)
  for (m in 1:d) {
    res$B.mean[m,] <- B.sample[m,,] %>%
      purrr::array_branch(1) %>%
      purrr::map_dbl(mean)
    res$B.sigma2[m,] <- B.sample[m,,] %>%
      purrr::array_branch(1) %>%
      purrr::map_dbl(var)
  }
  res
}

tscoreToPvalue <- function(score, df) {
  score %>%
    apply(1:2,function(z) 2 * pt(abs(z), df = df, lower.tail = FALSE))
}
