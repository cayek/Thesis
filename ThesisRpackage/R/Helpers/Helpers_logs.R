#' Print a message if getOption("Article3Package.debug") is not null
#'
#'
#' @param msg The message
DebugMessage <- function(who, out) {
  flog.debug(paste0(c(paste0(who, " said:"),
                      "--------------------------------",
                      out,
                      "--------------------------------"), collapse = "\n"))
}


#' futile.logger wrapper
#'
#'
#' @param msg The message
flog.debug <- function(msg,..., name = "ThesisRpackage", capture = FALSE) {
  futile.logger::flog.debug(msg = msg, ..., name = name, capture = capture)
}

#' futile.logger wrapper
#'
#'
#' @param msg The message
flog.trace <- function(msg,..., name = "ThesisRpackage", capture = FALSE) {
  futile.logger::flog.debug(msg = msg, ..., name = name, capture = capture)
}

#' futile.logger wrapper
#'
#'
#' @param msg The message
flog.info <- function(msg,..., name = "ThesisRpackage", capture = FALSE) {
  futile.logger::flog.debug(msg = msg, ..., name = name, capture = capture)
}

#' futile.logger wrapper
#'
#'
#' @param msg The message
flog.console <- function(msg, ...) {
  futile.logger::flog.logger("console", futile.logger::INFO, futile.logger::appender.console())
  futile.logger::flog.info(msg, ..., name = "console")
}
