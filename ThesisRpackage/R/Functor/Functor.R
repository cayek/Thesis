#' Sampler class
#'
#'
#' @export
Functor <- function(fun, name) {
  structure(list(fun = fun, name = name), class = "Functor")
}


#' @export
name.Functor <- function(obj) {
  obj$name
}
