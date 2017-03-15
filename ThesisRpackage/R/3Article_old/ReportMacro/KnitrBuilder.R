#' Expand and build a Rmd document
#'
#'
#' @export
KnitrExpandBuild <- function(file, ...) {
  out <- knitr::knit_expand(file = file, ...)
  cat(knitr::knit(text = out, quiet = TRUE))
}

