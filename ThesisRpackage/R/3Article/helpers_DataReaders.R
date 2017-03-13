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
