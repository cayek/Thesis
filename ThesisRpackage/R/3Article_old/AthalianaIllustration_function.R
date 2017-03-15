################################################################################
# plot function

#' @export
Athaliana_runs_plot <- function(exp, fdr.threshold, df = 8) {
  require(locfdr)

  method.names <- unique(exp$df.res$method.name)

  ## score for locfdr
  score <- exp$df.res %>%
    dplyr::filter(variable.name == "score1")

  ## score which are to NA
  na.score <- which(is.na(score$estimate))

  ## we want to plot B
  toplot <- exp$df.res %>%
    dplyr::filter(variable.name == "B1") %>%
    mutate(fdr = NA)


  ## remove na
  toplot <- toplot[-na.score,]
  score <- score[-na.score,]

  ## must be equal
  testthat::expect_equal(toplot$index, score$index)

  ## run of locfdr
  for (name in method.names) {
    z <- score$estimate[score$method.name == name]
    lfdr <- locfdr(z, df = df)$fdr
    cat(paste0("\n==",name, ", lfdr < ", fdr.threshold, " = ", mean(lfdr < fdr.threshold) * 100, " %\n"))
    toplot$fdr[toplot$method.name == name] = lfdr
  }

  ggplot(toplot, aes(x = index, y = estimate, color = fdr < fdr.threshold)) +
    geom_point() +
    geom_point(data = toplot %>% dplyr::filter(fdr < fdr.threshold)) +
    facet_grid(method.name ~ variable.name, scales = "free")
}


################################################################################
# Long running

#' Run of lm and lfmm
#'
#'
#' @export
long_Athaliana_runs <- function(Ks = c(5, 6, 7, 10), lambdas = c(1e-1), n = NULL, L = NULL, save = TRUE, gif = FALSE) {
  library(Article3Package)

  G.file <- "~/Projects/Data2016_2017/AthalianaGegMapLines/call_method_75/call_method_75_TAIR9.G.rds"
  X.file <- "~/Projects/Data2016_2017/AthalianaGegMapLines/call_method_75/NorthSouthX.rds"

  s <- TrueSampler(G.file = G.file,
                   X.file = X.file,
                   outlier.file = NULL,
                   n = n,
                   L = L)

  lm <- finalLm(gif = gif)

  # parallel
  cl <- parallel::makeCluster(4)
  doParallel::registerDoParallel(cl)

  ComparisonExperiment_GridRun(method.constructor = finalLfmmRdigeMethod,
                               s = s,
                               ms = list(lm = lm),
                               Ks = Ks,
                               lambdas = lambdas,
                               gif = c(gif),
                               save = save)
}


#' Run of PCA on thaliana
#'
#'
#' @export
long_Athaliana_PCA <- function(save = TRUE, n = NULL, L = NULL) {

  library(Article3Package)

  G.file <- "~/Projects/Data2016_2017/AthalianaGegMapLines/call_method_75/call_method_75_TAIR9.G.rds"
  X.file <- "~/Projects/Data2016_2017/AthalianaGegMapLines/call_method_75/NorthSouthX.rds"

  s <- TrueSampler(G.file = G.file,
                   X.file = X.file,
                   outlier.file = NULL,
                   n = n,
                   L = L)

  exp <- PCAExperiment(s = s, description = paste0("PCAexperiment on ", basename(s$G.file), " and ", basename(s$X.file)))
  exp <- runExperiment(exp)

  # save exp
  if (save) {
    dumpExperiment(exp)
  }

  exp
}

