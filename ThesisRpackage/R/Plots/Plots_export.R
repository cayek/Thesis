#' @export
Plots_export_tikz_pdf <- function(pl, basename.output, env) {

  TestRequiredPkg(c("tikzDevice", "tools"))

  ## go to fig dir
  bup <- getwd()
  setwd(env$fig.dir)

  ## tikz
  tikzDevice::tikz(paste0(basename.output, ".tex"), width = (env$fig.prop$width * env$page$width),
                   height = (env$fig.prop$heigth * env$page$heigth), standAlone = TRUE)
  print(pl)
  dev.off()

  ## compil
  tools::texi2dvi(paste0(basename.output, ".tex"), pdf = TRUE)


  ## go back
  setwd(bup)


}

#' @export
Plots_export_pdf <- function(pl, basename.output, env, width, height) {


  pdf(paste0(env$fig.dir, "/", basename.output, ".pdf"), width = width,
      height = height)
  print(pl)

  dev.off()


}

#' @export
Plots_export_png <- function(pl, basename.output, env, width, height) {

  png(paste0(env$fig.dir, "/", basename.output, ".png"), width = width,
      height = height, res = 600, units = "in")
  print(pl)

  dev.off()
}
