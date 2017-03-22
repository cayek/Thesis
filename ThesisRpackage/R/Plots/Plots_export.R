#' @export
Plots_export_tikz_pdf <- function(pl, basename.output, env,
                                  ...) {

  params <- list(...)

  if (is.null(params$width)) {
    params$width <- env$page$width * env$fig.prop$width
  }

  if (is.null(params$height)) {
    params$height <- env$page$height * env$fig.prop$height
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
    params$width <- env$page$width * env$fig.prop$width
  }

  if (is.null(params$height)) {
    params$height <- env$page$height * env$fig.prop$height
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
    params$width <- env$page$width * env$fig.prop$width
  }

  if (is.null(params$height)) {
    params$height <- env$page$height * env$fig.prop$height
  }

  png(paste0(env$fig.dir, "/", basename.output, ".png"), width = params$width,
      height = params$height, res = 600, units = "in")
  print(pl)

  dev.off()
}
