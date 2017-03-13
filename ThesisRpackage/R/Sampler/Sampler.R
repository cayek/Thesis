#' Sampler class
#'
#'
#' @export
Sampler <- function() {
 structure(list(), class = "Sampler")
}



################################################################################
# Methods

#' Title
#'
#' @export
sampl <- function(x){
  UseMethod("sampl")
}

#' Title
#'
#'
#' @export
print.Sampler <- function(x) {
  str(x)
}



