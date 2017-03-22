#' @export
Article2_figure2.5 <- function() {

  ## test
  TestRequiredPkg(c("cowplot"))

  ## retrieve results
  exp <- retrieveExperiment(53)
  assertthat::assert_that(exp$name == "long_tess3_noisyCoord")

  ## remove Fst <= 0.5
  exp$df.res <- exp$df.res %>%
    dplyr::filter(Fst.theorical <= 0.5) %>%
    dplyr::filter(noise.signal <= 2.0)

  toplot <- plot_tess3_noisyCoord_toplot(exp, mean.before = TRUE) %>%
    dplyr::mutate(Fst.theorical = format(Fst.theorical, digits = 2))


  pl <- ggplot(toplot, aes(x = noise.signal, y = rel.diff.rmse.Q.mean,
                           color = as.factor(Fst.theorical))) +
    geom_point() +
    geom_line() +
    facet_grid(L ~ n) +
    theme_bw() +
    # xlab("$Fst = 1 / (1 + 4 N_0 m)$") +
    xlab("noise-to-signal ratio") +
    ylab("Relative difference") +
    Article2.env$gtheme +
    theme(legend.position = c(0.85,0.55)) +
    guides( color = guide_legend(title = "$(F_{\\rm ST})$")) +
    Article2.env$cb.scale.color +
    geom_hline(yintercept = 0, alpha = 0.8, col = "grey")
  pl
}
