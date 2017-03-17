#' An experiment to run PCA on a data set.
#'
#' Then compute correlation between U and X.
#'
#'
#' @export
PCAExperiment <- function(s, description = "Run PCA on dataset", seed = sample.int(.Machine$integer.max, 1)) {
  exp <- Experiment(name = "PCAExperiment",
                    description = description,
                    seed = seed)
  class(exp) <- c("PCAExperiment", class(exp))
  exp$s <- s
  exp
}


#' @export
runExperiment.PCAExperiment <- function(exp) {

  # set the seed
  DebugMessage("We set the seed")
  set.seed(exp$seed)

  dat <- sampl(exp$s)

  # We remove snip with na
  if (anyNA(dat$G)) {
    DebugMessage("Missing values detected")
    locus.na <- apply(dat$G, 2, anyNA)
    message("== Removing ", mean(locus.na), "% col")
    G_ <- dat$G[,!locus.na]
  } else {
    G_ <- dat$G
  }

  # index
  exp$res.df <- tibble(index = 1:nrow(G_))

  # run PCA
  prcomp.res <- prcomp(G_, center = TRUE, scale. = FALSE)

  exp$res.df <- exp$res.df %>%
    dplyr::mutate(sdev = prcomp.res$sdev)

  # compute correlation between Ui and X
  UXcor <- apply(prcomp.res$x, 2, function(c) cor(dat$X[,1], c))
  exp$res.df <- exp$res.df %>%
    dplyr::mutate(UXcor = UXcor)
  exp
}


#' @export
plot.PCAExperiment <- function(exp) {
  ggplot(exp$res.df, aes(x = index, y = sdev, size = abs(UXcor), color = abs(UXcor))) +
    geom_point()
}
