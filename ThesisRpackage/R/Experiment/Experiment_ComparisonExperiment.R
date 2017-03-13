#' ComparisonExperiment class
#'
#'
#' @export
ComparisonExperiment <- function(sampler,
                                 ...,
                                 description = "comparison of algorithm on a sampler") {
  exp <- Experiment(name = "ComparisonExperiment",
                    description = description)
  class(exp) <- c("ComparisonExperiment", class(exp))
  exp$methods <- list(...)
  exp$sampler <- sampler
  exp
}


################################################################################
# Method


#' @export
runExperiment.ComparisonExperiment <- function(exp) {
  # set the seed
  DebugMessage("We set the seed")
  set.seed(exp$seed)

  # dataset
  dat <- sampl(exp$sampler)
  outlier <- dat$outlier
  exp$df.res <- getTidy_MethodOutput(dat, outlier) %>%
    dplyr::mutate(method.name = "oracle")

  start.time <- Sys.time()
  exp$df.res <- foreach(method = exp$methods, .combine = 'rbind') %dopar%
  {
    res <- run(method, dat)
    getTidy_MethodOutput(res, outlier) %>%
      dplyr::mutate(method.name = name(method))

  }
  end.time <- Sys.time()
  exp$runtime <- end.time - start.time

  exp
}

#' @export
plot.ComparisonExperiment <- function(exp, variable.name.regexp = c("B|score|V")) {

  if (!is.null(variable.name.regexp)) {
    toplot <- exp$df.res %>% dplyr::filter(grepl(variable.name.regexp,variable.name))
  } else {
    toplot <- exp$df.res
  }
  ggplot(toplot) +
    geom_point(aes(x = index,
                   y = estimate,
                   color = outlier)) +
    facet_grid(method.name ~ variable.name, scales = "free")

}
