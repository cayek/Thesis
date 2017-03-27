
#' @export
Article2_figure1 <- function() {

  ## test
  TestRequiredPkg(c("reshape2", "cowplot"))

  ## retrieve results
  exp <- retrieveExperiment(54)
  assertthat::assert_that(exp$name == "2Article_figure1")

   ## plot
  toplot <- exp$df.n  %>%
    reshape2::melt(id = c("n", "rep", "L", "method"), value.name = "rmse") %>%
    group_by(n, method, variable) %>%
    mutate(rmse.mean = mean(rmse), N = length(rmse), sd = sd(rmse), se = sd / sqrt(N)) %>%
    mutate(Methods = method)

  pl.n.A <- ggplot(toplot %>% dplyr::filter(variable == "rmseG"),aes(x = n, y = rmse.mean, col = Methods, linetype = Methods, shape = Methods)) +
    geom_errorbar(aes(ymin = rmse.mean - se,
                      ymax = rmse.mean + se,
                      width = (max(n) - min(n)) * Article2.env$errorbar.width.ratio )) +
    geom_line() +
    geom_point(size = Article2.env$point.size) +
    xlab("Number of individuals ($n$)") +
    ylab("RMSE") +
    Article2.env$gtheme +
    theme(legend.position = "none",
          plot.title = element_text(size = 16)) +
    Article2.env$scale.linetype +
    Article2.env$scale.color +
    ggtitle("$G$-matrix")

  pl.n.B <- ggplot(toplot %>% dplyr::filter(variable == "rmseQ"),
                   aes(x = n, y = rmse.mean, col = Methods, linetype = Methods, shape = Methods)) +
    geom_errorbar(aes(ymin = rmse.mean - se, ymax = rmse.mean + se,
                      width = (max(n) - min(n)) * Article2.env$errorbar.width.ratio )) +
    geom_line() +
    geom_point(size = Article2.env$point.size) +
    xlab("Number of individuals ($n$)") +
    ylab("") +
    Article2.env$gtheme +
    theme(legend.position = "none",
          plot.title = element_text(size = 16)) +
    Article2.env$scale.linetype +
    Article2.env$scale.color +
  ggtitle("$Q$-matrix")

  toplot <- exp$df.L  %>%
    reshape2::melt(id = c("nsites.neutral", "rep", "L", "method"), value.name = "rmse") %>%
    group_by(nsites.neutral, method, variable) %>%
    mutate(rmse.mean = mean(rmse), N = length(rmse), sd = sd(rmse), se = sd / sqrt(N), L = mean(L))

  pl.L.C <- ggplot(toplot %>% dplyr::filter(variable == "rmseG"), aes(x = L / 1000, y = rmse.mean, col = method, linetype = method, shape = method)) +
    geom_errorbar(aes(ymin = rmse.mean - se, ymax = rmse.mean + se,
                      width = (max(L) - min(L)) * Article2.env$errorbar.width.ratio / 1000 )) +
    geom_line() +
    geom_point(size = Article2.env$point.size) +
    xlab("Number of loci $\\times 1000$ ($L$)") +
    ylab("RMSE") +
    Article2.env$gtheme +
    theme(legend.position = "none") +
    Article2.env$scale.linetype +
    Article2.env$scale.color

  pl.L.D <- ggplot(toplot %>% dplyr::filter(variable == "rmseQ"), aes(x = L / 1000, y = rmse.mean, col = method, linetype = method, shape = method)) +
    geom_errorbar(aes(ymin = rmse.mean - se, ymax = rmse.mean + se,
                      width = (max(L) - min(L)) * Article2.env$errorbar.width.ratio / 1000 )) +
    geom_line() +
    geom_point(size = Article2.env$point.size) +
    xlab("Number of loci $\\times 1000$ ($L$)") +
    ylab("") +
    Article2.env$gtheme +
    Article2.env$scale.linetype +
    Article2.env$scale.color +
    theme(legend.position = c(0.75,0.60))


  pl <- cowplot::plot_grid(pl.n.A, pl.n.B, pl.L.C, pl.L.D, ncol = 2, labels = c("A", "B", "C", "D"))

  pl
}
