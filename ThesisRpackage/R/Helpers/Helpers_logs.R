#' Print a message if getOption("Article3Package.debug") is not null
#'
#'
#' @param msg The message
DebugMessage <- function(who, out = "") {
  flog.debug(paste0(c(paste0(who, " said:"),
                      "--------------------------------",
                      out,
                      "--------------------------------"), collapse = "\n"))
}


#' futile.logger wrapper
#'
#'
#' @param msg The message
flog.debug <- function(..., name = "ThesisRpackage", capture = FALSE) {
  msg <- paste(...)
  futile.logger::flog.debug(msg = msg, name = name, capture = capture)
}

#' futile.logger wrapper
#'
#'
#' @param msg The message
flog.warning <- function(..., name = "ThesisRpackage", capture = FALSE) {
  msg <- paste(...)
  futile.logger::flog.warn(msg = msg, name = name, capture = capture)
}


#' futile.logger wrapper
#'
#'
#' @param msg The message
flog.trace <- function(..., name = "ThesisRpackage", capture = FALSE) {
  msg <- paste(...)
  futile.logger::flog.trace(msg = msg, name = name, capture = capture)
}

#' futile.logger wrapper
#'
#'
#' @param msg The message
flog.info <- function(..., name = "ThesisRpackage", capture = FALSE) {
  msg <- paste(...)
  futile.logger::flog.info(msg = msg, name = name, capture = capture)
}
#' futile.logger
#'
#' @export
flog.threshold <- function(threshold = NULL, name = "ThesisRpackage") {
  if (!is.null(threshold)) {
    futile.logger::flog.threshold(threshold = threshold, name = name)
  } else {
    futile.logger::flog.logger(name = name)$threshold
  }
}
