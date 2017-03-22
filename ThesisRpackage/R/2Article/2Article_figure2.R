#' @export
Article2_figure2 <- function() {

  ## test
  TestRequiredPkg(c("cowplot"))

  ## retrieve results
  exp <- retrieveExperiment(55)
  assertthat::assert_that(exp$name == "2Article_figure2")

  labbeler <- function(variable, value) {
    if (as.character(variable) == "n") {
      paste0("$n = ",value, "$")
    } else if (as.character(variable) == "L") {
      paste0("$L \\approx 10^", floor(log(value,base = 10)), "$")
    }
  }

  toplot <- exp$df %>%
    group_by(nsites.neutral) %>%
    dplyr::mutate(L = round(mean(L))) %>%
    group_by(method, m.neutral, n, L) %>%
    dplyr::mutate(Fst = mean(Fst), rmse.mean = mean(rmseQ), N = length(rmseQ), sd = sd(rmseQ), se = sd / sqrt(N)) %>%
    rename(Methods = method )
  levels(toplot$Methods)[1] <- "APLS"
  pl <- ggplot(toplot ,
               aes(x = Fst.theorical,
                   y = rmse.mean,
                   col = Methods,
                   shape = Methods,
                   linetype = Methods)) +
    geom_errorbar(aes(ymin = rmse.mean - se, ymax = rmse.mean + se), width = 0.0) +
    geom_line() +
    geom_point(size = Article2.env$point.size) +
    facet_grid(L ~ n, labeller = labbeler) +
    theme_bw() +
    # xlab("$Fst = 1 / (1 + 4 N_0 m)$") +
    xlab("Fixation index $(F_{\\rm ST})$") +
    ylab("RMSE") +
    Article2.env$gtheme +
    theme(legend.position = c(0.85,0.3)) +
    Article2.env$scale.color +
    Article2.env$scale.linetype
  pl
}
