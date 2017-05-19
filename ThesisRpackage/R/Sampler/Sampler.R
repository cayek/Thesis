Sampler.builder <- setRefClass("Sampler", fields = c("loaded"))


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
#' @export
Sampler_load <- function(s){
  UseMethod("Sampler_load")
}

#' Title
#'
#' @export
Sampler_load.Sampler <- function(s){
  s$loaded <- TRUE
  s
}

#' Title
#'
#'
#' @export
print.Sampler <- function(x) {
  str(x)
}



