################################################################################
# Data reader


#' read the G matrix
#'
#' @export
read_G <- function(G.file) {
  if (tools::file_ext(G.file) == "lfmm") {
    readr::read_delim(G.file, delim = " ",
                      col_names = FALSE,
                      col_types = readr::cols(.default = readr::col_integer())) %>%
      as.matrix()
  } else if (tools::file_ext(G.file) == "RData") {
    stop("TODO")
  } else if (tools::file_ext(G.file) == "rds") {
    readRDS(G.file)
  } else {
    stop("TODO")
  }
}

#' read the X matrix
#'
#' @export
read_X <- function(X.file) {
  if (tools::file_ext(X.file) == "env") {
    readr::read_delim(X.file, delim = " ",
                      col_names = FALSE,
                      col_types = readr::cols(.default = readr::col_number())) %>%
      as.matrix()
  } else if (tools::file_ext(X.file) == "RData") {
    stop("TODO")
  } else if (tools::file_ext(X.file) == "rds") {
    readRDS(X.file)
  } else {
    stop("TODO")
  }
}

#' read vcf files
#'
#' Rmk: it uses LEA::vcf2geno
#'
#' @param file.pattern
#' @param keep.indiv keep only this indiv
#' @param maf.threshold filter with this maf threshold
#' @param subsample.rate subsample with this rate
#'
#' @export
read_vcf <- function(file.pattern, maf.threshold = NULL, subsample.rate = NULL) {
  ## assert
  TestRequiredPkg("LEA")

  files <- list.files()
  files <- grep(file.pattern, files, value = TRUE)

  res <- list()
  res$G <- NULL
  res$snps.info <- tibble()
  for (f in files) {
    output.tmp <- tempfile(tmpdir = ".", fileext = ".geno")
    LEA::vcf2geno(f, output.file = output.tmp, force = TRUE)
    geno <- LEA::read.geno(output.tmp)
    snps.info <- readr::read_delim(sub(pattern = "\\.geno$", "\\.vcfsnp", output.tmp),
                                   delim = " ",
                                   col_names = FALSE,
                                   progress = FALSE)
    colnames(geno) <- snps.info$X3

    ## filter maf
    if (!is.null(maf.threshold)) {
      maf <- apply(geno, 2, function(locus) {p <- mean(locus); min(p, 1 - p)})
      cat("== Removing", mean(maf < maf.threshold),"% loci")
      geno <- geno[,maf >= maf.threshold]
    }
    res$G <- cbind(geno, res$G)
  }
}

