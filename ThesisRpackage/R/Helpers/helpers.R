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


#' Print a message if getOption("Article3Package.debug") is not null
#'
#'
#' @param msg The message
DebugMessage <- function(msg) {
  if (!is.null(getOption("ThesisRpackage.debug"))) {
    cat(crayon::bgGreen(paste0("= ", msg,"\n")))
  }
}

#' Install bioconductor dependencies
#'
#' @export
BioConductorDep <- function(pkgs = c("sva", "impute")) {
  source("https://bioconductor.org/biocLite.R")
  biocLite(pkgs = pkgs)
}


#' Test if you are on krakenator
#'
#'
KrakTest <- function(bypass) {
  if (Sys.info()["nodename"] != "krakenator.imag.fr" && !bypass) {
    stop("Your are not on krakenator")
  }
}

#' Use it before running the core of a long function
long_init <- function(cluster.nb,
                      bypass) {
  KrakTest(bypass)

  if (!is.null(cluster.nb)) {
    cl <- parallel::makeCluster(cluster.nb, outfile = "")
    doParallel::registerDoParallel(cl)
  }
}

#' make a description string
make_description <- function(desc, ...) {
  params <- list(...)
  ns <- names(params)
  desc <- paste0(desc, " with ")
  for (n in ns) {
    desc <- paste0(desc, n,"=", paste0(params[[n]], collapse = "|")," ")
  }
  desc
}
