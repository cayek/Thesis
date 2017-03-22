#' @export
Article2_figure3 <- function() {


  exp <- retrieveExperiment(56)
  assertthat::assert_that(exp$name == "2Article_figure3")

  toplot <- exp$df %>%
    group_by(method, m.ms) %>%
    dplyr::mutate(auc.mean = mean(auc), N = length(auc), sd = sd(auc), se = sd / sqrt(N)) %>%
    rename(Methods = method )
  levels(toplot$Methods)[1] <- "APLS"
  levels(toplot$Methods)[3] <- "before-admixure"

  pl <- ggplot(toplot ,aes(x = m.ms, y = auc.mean, col = Methods, linetype = Methods, shape = Methods)) +
    geom_errorbar(aes(ymin = auc.mean - se, ymax = auc.mean + se), width = 0.0) +
    geom_line() +
    geom_point(size = Article2.env$point.size) +
    theme_bw() +
    xlab("Intensity of selection ($m/m_s$)") +
    ylab("AUC") +
    Article2.env$gtheme +
    theme(legend.position = c(0.8,0.2)) +
    Article2.env$scale.linetype +
    Article2.env$scale.color
  pl

}
