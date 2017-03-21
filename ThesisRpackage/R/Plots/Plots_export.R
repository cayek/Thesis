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
