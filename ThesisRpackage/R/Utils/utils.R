#' create new file
#'
#'
#' @param name filename
#' @param dir dirname
#'
#'
#' @export
new_file <- function(dir, name) {

  assertthat::assert_that(getwd() == path.expand("~/Projects/Thesis/ThesisRpackage"))

  aux <- function(dir.base, name) {
    dd <- paste0(dir.base, dir,"/")
    if (!dir.exists(dd)) {
      dir.create(dd)
    }
    file.R <- paste0(dir.base, dir,"/", name, ".R")
    assertthat::assert_that(!file.exists(file.R))
    file.create(file.R)
    system(paste0("cd ", dir.base, "; ln -s ", dir,"/", name, ".R ", name, ".R"))
    ## rstudioapi::navigateToFile(file.R)
  }

  ## create file R
  aux("R/", name = name)

  ## create test file
  aux(dir.base = "tests/testthat/", name = paste0("test_", name))

}

#' create new notebook
#'
#'
#' @param name filename
#' @param dir dirname
#'
#'
#' @export
new_nb <- function(dir, name) {

  stop("TODO")

  assertthat::assert_that(getwd() == path.expand("~/Projects/Thesis/ThesisRpackage"))
  assertthat::assert_that(dir.exists(paste0("../Rnotebook/",dir)))


}


#' save plot to png format on timc-bcm-15
#' @export
save_plot_timc_bcm_15 <- function(pl, filename, width, height) {
  png(filename, width, height)
  print(pl)
  dev.off()
}
