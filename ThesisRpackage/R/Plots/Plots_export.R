#' @export
Plots_export_tikz_pdf <- function(pl, basename.output, env,
                                  ...) {

  params <- list(...)

  if (is.null(params$width)) {
    params$width <- env$fig$width
  }

  if (is.null(params$height)) {
    params$height <- env$fig$height
  }

  TestRequiredPkg(c("tikzDevice", "tools"))

  ## go to fig dir
  bup <- getwd()
  setwd(env$fig.dir)

  ## tikz
  tikzDevice::tikz(paste0(basename.output, ".tex"), width = params$width,
                   height = params$height, standAlone = TRUE)
  print(pl)
  dev.off()

  ## compil
  tools::texi2dvi(paste0(basename.output, ".tex"), pdf = TRUE)


  ## go back
  setwd(bup)


}

#' @export
Plots_export_pdf <- function(pl, basename.output, env, ...) {

  params <- list(...)

  if (is.null(params$width)) {
    params$width <- env$fig$width
  }

  if (is.null(params$height)) {
    params$height <- env$fig$height
  }

  pdf(paste0(env$fig.dir, "/", basename.output, ".pdf"), width = params$width,
      height = params$height)
  print(pl)

  dev.off()


}

#' @export
Plots_export_png <- function(pl, basename.output, env, ...) {

  params <- list(...)

  if (is.null(params$width)) {
    params$width <- env$fig$width
  }

  if (is.null(params$height)) {
    params$height <- env$fig$height
  }

  if (is.null(params$res)) {
    params$res <- 600
  }

  png(paste0(env$fig.dir, "/", basename.output, ".png"), width = params$width,
      height = params$height, res = params$res, units = "in")
  print(pl)

  dev.off()
}
