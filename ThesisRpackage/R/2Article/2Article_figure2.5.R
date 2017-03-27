#' @export
Article2_figure2.5 <- function() {

  ## test
  TestRequiredPkg(c("cowplot"))

  ## retrieve results
  exp <- retrieveExperiment(60)
  assertthat::assert_that(exp$name == "long_tess3_noisyCoord")

  ## remove Fst <= 0.5
  exp$df.res <- exp$df.res %>%
    dplyr::filter(Fst.theorical <= 0.5) %>%
    dplyr::filter(noise.signal <= 3.0)

  toplot <- plot_tess3_noisyCoord_toplot(exp) %>%
    dplyr::mutate(Fst.theorical = format(Fst.theorical, digits = 2))


  labbeler <- function(variable, value) {
    if (as.character(variable) == "n") {
      paste0("$n = ",value, "$")
    } else if (as.character(variable) == "L") {
      paste0("$L \\approx 10^", ceiling(log(value,base = 10)), "$")
    }
  }

  pl <- ggplot(toplot, aes(x = noise.signal, y = rel.diff.rmse.Q.mean,
                           color = as.factor(Fst.theorical),
                           shape = as.factor(Fst.theorical))) +
    geom_point() +
    geom_line() +
    geom_errorbar(aes(ymin = rel.diff.rmse.Q.mean - rel.diff.rmse.Q.mean.se, ymax = rel.diff.rmse.Q.mean + rel.diff.rmse.Q.mean.se,
                      width = (max(noise.signal) - min(noise.signal)) * Article2.env$errorbar.width.ratio)) +
    facet_grid(L ~ n, labeller = labbeler) +
    theme_bw() +
    # xlab("$Fst = 1 / (1 + 4 N_0 m)$") +
    xlab("Noise-to-signal ratio") +
    ylab("Relative error") +
    Article2.env$gtheme +
    theme(legend.position = c(0.75,0.63)) +
    guides( color = guide_legend(title = "$F_{\\rm ST}$", nrow = 2),
            linetype = guide_legend(title = "$F_{\\rm ST}$", nrow = 3),
            shape = guide_legend(title = "$F_{\\rm ST}$", nrow = 3)) +
    Article2.env$cb.scale.color +
    geom_hline(yintercept = 0, alpha = 0.8, col = "grey")
  pl
}
