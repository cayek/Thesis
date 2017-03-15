#' Article 3 package
#'
#'
#' @docType package
#'
#' @name Article3Package
#' @import ggplot2
#' @import dplyr
#' @import tidyr
#' @importFrom magrittr "%>%"
#' @importFrom foreach foreach %:% %do% %dopar%
#' @import tibble
NULL

################################################################################
# common methods


#' get the name of the object
#'
#' @export
name <- function(obj){
  UseMethod("name")
}

#' get the name of the object
#'
#' @export
name.default <- function(obj) {
  as.character(class(obj)[1])
}
