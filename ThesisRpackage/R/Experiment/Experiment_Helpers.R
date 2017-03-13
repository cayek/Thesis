#' run ComparisonExperiment on a grid
#'
ComparisonExperiment_GridRun <- function(method.constructor, s, ms, Ks, lambdas, calibrate, save) {
  listOfMethods <- paramGrid(method.constructor = method.constructor,
                             nickname.base = "RidgeLfmm",
                             lambda = lambdas, K = Ks, calibrate = calibrate)


  listOfMethods <- c(listOfMethods, ms)
  # run exp
  description <- paste0("ComparisonExperiment on ", basename(s$G.file), " and ", basename(s$X.file))
  description <- paste0(description, " with methods: ",
                        paste(sapply(1:length(listOfMethods), function(i,j) paste0(listOfMethods[[i]]$nickname,"||")), collapse = ''))
  exp <- do.call(ComparisonExperiment, c(list(s = s,
                                              description = description),
                                         listOfMethods))
  exp <- runExperiment(exp)

  # save exp
  if (save) {
    dumpExperiment(exp)
  }
  exp
}
