#' Test if a package is installed.
#'
#'
#' @param pkg Package name to test.
TestRequiredPkg <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(paste(pkg,"needed for this function to work. Please install it."),
         call. = FALSE)
  }
}


#' Install bioconductor dependencies
#'
#' @export
BioConductorDep <- function(pkgs = c("sva", "impute", "qvalue")) {
  source("https://bioconductor.org/biocLite.R")
  biocLite(pkgs = pkgs)
}


#' Install CRAN dep
#'
#' @export
CRANDep <- function(pkgs = c('ggplot2',
                             "cowplot",
                             "tidyr",
                             "tibble",
                             "purrr",
                             "RSQLite",
                             "magrittr",
                             "MASS",
                             "foreach",
                             "tools",
                             "readr",
                             "knitr",
                             "gtools",
                             "doParallel",
                             "parallel",
                             "FAMT",
                             "assertthat",
                             "fdrtool",
                             "crayon")) {
  install.packages(pkgs)
}

#' Test if you are on krakenator
#'
#'
KrakTest <- function(bypass) {
  if (!(Sys.info()["nodename"] %in% c("krakenator.imag.fr","patator.imag.fr")) && !bypass) {
    stop("Your are not on krakenator or patator")
  }
}

#' Use it before running the core of a long function
long_init <- function(cluster.nb,
                      bypass, log.file = NULL) {
  KrakTest(bypass)
  cl <- NULL
  env <- new.env()
  if (!is.null(log.file)) {
    env$ap <- futile.logger::appender.file(log.file)
  } else {
    env$ap <- futile.logger::appender.console()
  }
  futile.logger::flog.appender(env$ap, name = "ThesisRpackage")
  if (!is.null(cluster.nb)) {
    cl <- parallel::makeCluster(cluster.nb, outfile = "")
    doParallel::registerDoParallel(cl)
    ## set the appender
    parallel::clusterExport(cl, varlist = c("ap"), envir = env)
    parallel::clusterEvalQ(cl, futile.logger::flog.appender(ap, name = "ThesisRpackage"))
  }
  cl
}

#' Use it after running the core of a long function
long_return <- function(cl, save, exp) {
  if (!is.null(cl)) {
    parallel::stopCluster(cl)
  }
  # save exp
  if (save) {
    dumpExperiment(exp)
  }
  futile.logger::flog.appender(futile.logger::appender.console(), name = "ThesisRpackage")
  exp
}

#' make a description string
#' @export
make_description <- function(desc, ...) {
  params <- list(...)
  ns <- names(params)
  desc <- paste0(desc, " with ")
  for (n in ns) {
    if (is.function(params[[n]])) {
      params[[n]] <- "function..."
    }
    desc <- paste0(desc, n,"=", paste0(params[[n]], collapse = "|")," ")
  }
  desc
}
