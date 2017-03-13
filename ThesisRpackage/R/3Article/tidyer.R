################################################################################
# helpers

tidy_matrix <- function(matrix, name) {
  d <- ncol(matrix)
  res <- as_tibble(matrix)
  names(res) <- sapply(1:d, function(i) paste0(name,i))
  res %>% gather(key = variable.name, value = estimate)
}


################################################################################
# methods

#' @export
getTidy_MethodOutput <- function(obj, outlier = c()) {

  # along G row
  res.n <- tibble()
  # U
  if (!is.null(obj$U)) {
    res.n <- tidy_matrix(obj$U, name = "U") %>%
      rbind(res.n)
  }
  # X
  if (!is.null(obj$X)) {
    res.n <- tidy_matrix(obj$X, name = "X") %>%
      rbind(res.n)

  }
  if (length(res.n) != 0) {
    res.n <- res.n %>%
      dplyr::group_by(variable.name) %>%
      dplyr::mutate(index = seq_along(estimate)) %>%
      dplyr::mutate(outlier = FALSE) %>%
      dplyr::ungroup()
  }
  # along G col
  res.L <- tibble()
  # V
  if (!is.null(obj$V)) {
    res.L <- tidy_matrix(obj$V, name = "V") %>%
      rbind(res.L)
  }

  # B
  if (!is.null(obj$B)) {
    res.L <- tidy_matrix(t(obj$B), name = "B") %>%
      rbind(res.L)
  }
  # zscore
  if (!is.null(obj$score)) {
    res.L <- tidy_matrix(t(obj$score), name = "score") %>%
      rbind(res.L)
  }

  # pvalue
  if (!is.null(obj$pvalue)) {
    res.L <- tidy_matrix(t(obj$pvalue), name = "pvalue") %>%
      rbind(res.L)
  }
  if (length(res.L) != 0) {
    res.L <- res.L %>%
      dplyr::group_by(variable.name) %>%
      dplyr::mutate(index = seq_along(estimate)) %>%
      dplyr::mutate(outlier = index %in% outlier) %>%
      dplyr::ungroup()
  }

  rbind(res.n, res.L)
}
