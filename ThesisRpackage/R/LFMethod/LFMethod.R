#' Sampler class
#'
#'
#' @export
Method <- function(name = "A method", hypothesis.testing.method = NULL,
                   impute.genotype.method = imputeByMean(),
                   nickname = NULL) {
  structure(list(name = name,
                 hypothesis.testing.method = hypothesis.testing.method,
                 impute.genotype.method = impute.genotype.method,
                 nickname = nickname), class = "Method")
}



################################################################################
# Methods

#' likelyhood ratio
#'
#' give the likelyhood ratio of the fitted method, using B_j = 0 for 0
#' hypothesis
#'
#' @export
loglr <- function(m, dat){
  UseMethod("loglr")
}

#' fit
#'
#' @export
fit <- function(m, dat, reuse = FALSE, ...){
  UseMethod("fit")
}

#' name
#'
#' @export
name <- function(m){
  UseMethod("name")
}

#' Estimated the number of latent variable
#'
#' @export
numLatentVarEstimation <- function(m, dat, ...){
  UseMethod("numLatentVarEstimation")
}


#' Run the association scan method
#'
#' @export
run <- function(m, dat, ...){
  UseMethod("run")
}

#' Clean all estimates
#'
#' @export
clean <- function(m) {
  UseMethod("clean")
}

#' Loss
#'
#' @export
loss <- function(m) {
  UseMethod("loss")
}

#' prediction error
#'
#' @export
prediction_error <- function(m, dat) {
  UseMethod("prediction_error")
}

#' error commit on imputed values
#'
#' @export
imputation_error <- function(m, dat) {
  UseMethod("imputation_error")
}

#' average correlation between U and X
#'
#' @export
UXlink <- function(m, dat, ...) {
  UseMethod("UXlink")
}

#' Run the association scan method
#'
#' On all hypothesis
#' method in the list hypothesis.testing.method.list
#'
#' @export
run.Method <- function(m, dat){
  if (!is.null(m$hypothesis.testing.method)) {
    flog.debug(paste("run.Method: running ", m$hypothesis.testing.method$name))
    tmp <- m$hypothesis.testing.method$fun(m, dat)
    m[names(tmp)] <- tmp
  }
  return(m)
}

#' Title
#'
#'
#' @export
print.Method <- function(x) {
  str(x)
}


#' Title
#'
#'
#' @export
plot.Method <- function(m, dat, d = 1) {
  pl.list <- list()
  if (!is.null(m$B)) {
    pl.list$B.plot <- gplot_B(m$B[d,], dat$outlier)
  }
  if (!is.null(m$zscore)) {
    pl.list$zscore.plot <- gplot_zscore(m$score[d,], dat$outlier)
  }
  if (!is.null(m$pvalue)) {
    pl.list$pvalue.hist <- gplot_hist(m$pvalue[d,], dat$outlier)
    pl.list$pvalue.manhattan <- gplot_manhattan(m$pvalue[d,], dat$outlier)
  }
  cowplot::plot_grid(plotlist = pl.list, ncol = 1)
}


#' @export
name.Method <- function(m) {

  if (!is.null(m$nickname)) {
    return(m$nickname)
  }
  if (!is.null(m$name)) {
    paste0(m$name, "|",name(m$hypothesis.testing.method))
  } else {
    "A method"
  }
}

#' Clean all estimates
#'
#' @export
clean.Method <- function(m) {
  m$V <- NULL
  m$U <- NULL
  m$C <- NULL
  m$score <- NULL
  m$pvalue <- NULL
  m
}


#' Loss function
#'
#' @export
loss.Method <- function(m, dat) {
  mean(m$epsilon ^ 2)
}


#' prediction error
#'
#' @export
prediction_error.Method <- function(m, dat) {
  G.predicted <- predict(m, dat)

  # rmse
  sqrt(mean((dat$G - G.predicted) ^ 2))

}
#' prediction error
#'
#' @export
imputation_error.Method <- function(m, dat) {
  # rmse
  sqrt(mean((dat$G[m$missing.index] - m$imputed.values) ^ 2))
}


#' average abs correlation between U and X
#'
#' @export
UXlink.Method <- function(m, dat, link.stat = function(x, y) abs(cor(x, y)), summary.stat = mean) {
  summary.stat(apply(m$U, 2, function(c) link.stat(dat$X, c)))
}


################################################################################
## Plots

#' @export
qqplott <- function(m, ...) {
  UseMethod("qqplott")
}

#' @export
qqplott.Method <- function(m, outlier = c()) {
  L <- ncol(m$pvalue)
  d <- nrow(m$pvalue)
  toplot <- as.data.frame(t(m$pvalue)) %>%
    gather()

  ## plot outlier
  if (d == 1) {
  pvalue.df <- tibble(pvalue = m$pvalue[1,]) %>%
    mutate(rank  = rank(pvalue))
  print(pvalue.df[outlier, ])
  } else {
    print(t(m$pvalue[,outlier]))
  }

  ggplot(toplot, aes(sample = -log10(value))) +
    stat_qq(distribution = stats::qexp, dparams = list(rate = log(10))) +
    geom_abline(slope = 1, intercept = 0) +
    facet_grid(key~.) + 
    ggtitle("-log10(pvalue) qqplot")
}

################################################################################
## pvalue

#' @export
calibrate <- function(m) {
  UseMethod("calibrate")
}

#' @export
calibrate.Method <- function(m) {
  ## compute median
  m$med <- apply(m$score, 1, median)
  print("== calibrate: median")
  print(m$med)

  ## compute mad
  m$sds <- apply(m$score, 1, mad)
  print("== calibrate: mad")
  print(m$sds)


  ## center and scale
  score <- sweep(m$score, 1, m$med)
  score <- sweep(score, 1, m$sds, FUN = "/")

  ## compute pvalue
  m$pvalue <- zscoreToPvalue(score)
  m
}
