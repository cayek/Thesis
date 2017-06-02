#' create new file
#'
#'
#' @param name filename
#' @param dir dirname
#'
#'
#' @export
new_file <- function(dir, name) {

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
  aux("~/Projects/Thesis/ThesisRpackage/R/", name = name)

  ## create test file
  aux(dir.base = "~/Projects/Thesis/ThesisRpackage/tests/testthat/", name = paste0("test_", name))

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

#' @export
print.plotable <- function(pl) {
  pl$toplot()
}

#' @export
plotable<- function(toplot) {
  pl <- list(toplot = toplot)
  class(pl) <- "plotable"
  pl
}

#' save plot to png format on timc-bcm-15
#' @export
save_plot_timc_bcm_15 <- function(pl, filename, width = 600, height = 400,
                                  path.dir = "~/Projects/Thesis/Rplots/") {
  tmp.file <- tempfile()
  png(tmp.file, width, height)
  print(pl)
  dev.off()
  system(paste0("scp ", tmp.file,
                paste0(" cayek@timc-bcm-15:",path.dir),
                filename),
         ignore.stdout = TRUE)
  cat(paste0("[[./Rplots/",filename,"]]\n"))
}
