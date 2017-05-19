DataSet.builder <- setRefClass("DataSet", fields = c("G",
                                                     "X"))

#' Sampler class
#'
#'
#' @export
DataSet <- function(G, X) {
  structure(list(G = G,
                 X = X), class = "DataSet")
}


#' @export
plot.DataSet <- function(dat, m = NULL, d = 1)  {
  if (is.null(m)) {
    toplot <- tidy_matrix(dat$X, "X") %>%
      group_by(variable.name) %>%
      mutate(index = seq_along(estimate))
    ggplot(toplot, aes(x = index, y = estimate)) +
      geom_point() +
      facet_grid(.~variable.name)
  } else {
    if (!is.null(m$U)) {
      K <- ncol(m$U)
      toplot <- as_tibble(m$U)
      names(toplot) <- sapply(1:K, function(i) paste0("U",i))
      ggplot(toplot, aes(x = U1, y = U2, color = dat$X[,d])) +
        geom_point()
    }
  }
}


#' @export
acp <- function(dat) {
  UseMethod("acp")
}

