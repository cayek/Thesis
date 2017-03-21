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
#' Rmk: it uses readr::read_delim
#'
#' @param maf.threshold filter with this maf threshold
#'
#' @export
read_vcf_files <- function(file.pattern, maf.threshold = NULL,
                           save.rds = FALSE, block.size = 100000) {
  ## assert


  files <- list.files()
  files <- grep(file.pattern, files, value = TRUE)

  res <- list()
  res$G <- NULL
  res$snps.info <- tibble()
  for (f in files) {
    DebugMessage(paste0("== reading ",f,"\n"))


  }

}


#' read vcf
#'
#'
#' @param maf.threshold filter with this maf threshold
#'
#' @export
read_vcf <- function(f, maf.threshold = NULL,
                     n_max = Inf,
                     block.size = 100000) {

  ## read col_names
  header <- readr::read_delim(file = f,
                    delim = "\t", comment = "##",col_names = TRUE, n_max = 1,
                    col_types = readr::cols(`#CHROM` = readr::col_integer(),
                                            POS = readr::col_integer(),
                                            .default = readr::col_character()),
                    progress = FALSE)
  DebugMessage(paste0("FORMAT = ", header$FORMAT))


  ## read data
  geno <- NULL
  snps.info <- header[-1,1:9]

  EOF = FALSE
  skip = 0
  red.line = 0
  while (!EOF) {
    DebugMessage(paste0("red.line = ",red.line))
    ## read vcf
    vcf <- readr::read_delim(file = f,
                      delim = "\t", comment = "#",col_names = FALSE, n_max = block.size,
                      skip = skip,
                      col_types = readr::cols(X1 = readr::col_integer(),
                                              X2 = readr::col_integer(),
                                              .default = readr::col_character()),
                      progress = FALSE)
    names(vcf) <- names(header)

    ## update skip
    skip <- skip + block.size
    red.line <- red.line + nrow(vcf)
    if ((red.line >= n_max) || (nrow(vcf) < block.size)) {
      EOF = TRUE
    }

    ## filter line which are not snps
    variant.allow <- c("0|0", "0|1","1|0", "1|1", ".")
    remove <- apply(vcf[,-(1:9)], 1, function(l) mean(l %in% variant.allow) == 1)
    vcf <- vcf[remove,]

    ## extract snps matrix
    geno.aux <- as.matrix(vcf[,-(1:9)])
    geno.aux[geno.aux == "0|0"] <- 0
    geno.aux[geno.aux == "0|1"] <- 1
    geno.aux[geno.aux == "1|0"] <- 1
    geno.aux[geno.aux == "1|1"] <- 2
    geno.aux[geno.aux == "."] <- NA
    geno.aux <- t(matrix(as.integer(geno.aux), nrow(geno.aux), ncol(geno.aux)))

    ## maf filterring
    maf <- apply(geno.aux, 2, function(locus) {p <- mean(locus); min(p, 1 - p)})
    DebugMessage(paste0("Removing ", mean(maf <= maf.threshold),"% loci\n"))
    geno.aux <- geno.aux[,maf > maf.threshold, drop = FALSE]


    ## colnames
    colnames(geno.aux) <- vcf$ID[maf > maf.threshold]

    ## cbind
    snps.info <- rbind(snps.info,
                       vcf[maf > maf.threshold, 1:9])
    geno <- cbind(geno,geno.aux)
  }


  ## rownames
  rownames(geno) <- names(header)[-(1:9)]

  list(G = geno,
       snps.info = snps.info)

}
