#' An experiment to run a method on several sampler and see results
#'
#'
#' @export
DebugExperiment <- function(method, sampler.list, parameter.list) {
  exp <- Experiment(name = "DebugExperiment",
                    description = "Debug of method on several dataset")
  class(exp) <- c("DebugExperiment", class(exp))
  exp$method <- method
  exp$sampler.list <- sampler.list
  exp$parameter.list <- parameter.list
  exp
}


#' @export
runExperiment.DebugExperiment <- function(exp) {

  res.df <- tibble()

  # cartesian cross product of parameter
  parameters.list <- purrr::cross_n(exp$parameter.list)


  # sample all dataset
  dat.list <- list()
  for (i in seq_along(exp$sampler.list)) {
    dat.list[[i]] <- sampl(exp$sampler.list[[i]])
  }

  start.time <- Sys.time()
  res.df <-
    foreach(dat = dat.list, .combine = 'rbind') %:%
    foreach(p = parameters.list, .combine = 'rbind') %do%
    {
      cat("==Running dat:", name(dat), "| param: ", as.character(p), "\n")
      res <- exp$method
      # pass parameters
      res[names(p)] <- p
      res <- run(res, dat)
      # tidy output
      res.df <- getTidy_MethodOutput(res) %>%
        dplyr::mutate(data.name = name(dat)) %>%
        dplyr::group_by(variable.name) %>%
        dplyr::mutate(index = seq_along(estimate),
                      outlier = index %in% dat$outlier) %>%
        dplyr::ungroup()
      # add argument column
      do.call(dplyr::mutate,
              c(list(.data = res.df), p))
    }
  end.time <- Sys.time()
  exp$runtime <- end.time - start.time


  exp$result <- list(res.df = res.df)
  exp
}


#' @export
plot.DebugExperiment <- function(exp, filter.exp,
                                 grid.formula = variable.name ~ data.name,
                                 geom = function() geom_point(aes(x = index, y = estimate, fill = outlier, color = outlier))) {
  toplot <- exp$result$res.df %>%
    dplyr::filter_(filter.exp)
    ggplot(toplot) +
      geom() +
      facet_grid(grid.formula, scales = "free")
}

