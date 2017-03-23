#' @export
Article2_figure4 <- function() {

  TestRequiredPkg(c("cowplot"))

  exp <- retrieveExperiment(57)
  assertthat::assert_that(exp$name == "2Article_figure4")


  g_legend <- function(a.gplot){
    tmp <- ggplot_gtable(ggplot_build(a.gplot))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    return(legend)}



  toplot <- exp$df.n  %>% group_by(method, n) %>%
    mutate(mean = mean(it), N = length(it), sd = sd(it), se = sd / sqrt(N)) %>%
    rename(Methods = method)
  pl.it.n <- ggplot(toplot ,aes(x = n, y = mean, col = Methods, linetype = Methods, shape = Methods)) +
    geom_line() +
    geom_point(size = Article2.env$point.size) +
    geom_errorbar(aes(ymin = mean - se,
                      ymax = mean + se,
                      width = (max(n) - min(n)) * Article2.env$errorbar.width.ratio)) +
    theme_bw() +
    xlab("") +
    ylab("Number\nof iterations") +
    Article2.env$gtheme +
    theme(legend.position = "none") +
    Article2.env$scale.linetype +
    Article2.env$scale.color

  toplot <- exp$df.n  %>% group_by(n, method) %>%
    mutate(mean = mean(time.per.it.mean), N = length(time.per.it.mean), sd = sd(time.per.it.mean), se = sd / sqrt(N)) %>%
    rename(Methods = method)
  pl.time.n <- ggplot(toplot ,aes(x = n, y = mean, col = Methods, linetype = Methods, shape = Methods)) +
    geom_line() +
    geom_point(size = Article2.env$point.size) +
    geom_errorbar(aes(ymin = mean - se, ymax = mean + se,
                      width = (max(n) - min(n)) * Article2.env$errorbar.width.ratio)) +
    theme_bw() +
    scale_y_log10() +
    xlab("Number of individuals ($n$)") +
    ylab("Time per iteration \n(seconds)") +
    Article2.env$gtheme +
    theme(legend.position = "none") +
    Article2.env$scale.linetype +
    Article2.env$scale.color

  toplot <- exp$df.L  %>% group_by(method, L) %>%
    mutate(mean = mean(it), N = length(it), sd = sd(it), se = sd / sqrt(N)) %>%
    rename(Methods = method)
  pl.it.L <- ggplot(toplot ,aes(x = L / 1000, y = mean, col = Methods, linetype = Methods, shape = Methods)) +
    geom_line() +
    geom_point(size = Article2.env$point.size) +
    geom_errorbar(aes(ymin = mean - se,
                      ymax = mean + se,
                      width = (max(L) - min(L)) * Article2.env$errorbar.width.ratio / 1000)) +
    theme_bw() +
    xlab("") +
    ylab("") +
    Article2.env$gtheme +
    theme(legend.position = "none") +
    Article2.env$scale.linetype +
    Article2.env$scale.color

  toplot <- exp$df.L  %>% group_by(L, method) %>%
    mutate(mean = mean(time.per.it.mean), N = length(time.per.it.mean), sd = sd(time.per.it.mean), se = sd / sqrt(N)) %>%
    rename(Methods = method)

  pl.time.L <- ggplot(toplot ,aes(x = L / 1000, y = mean, col = Methods, linetype = Methods, shape = Methods)) +
    geom_line() +
    geom_point(size = Article2.env$point.size) +
    geom_errorbar(aes(ymin = mean - se, ymax = mean + se,
                      width = (max(L) - min(L)) * Article2.env$errorbar.width.ratio / 1000)) +
    theme_bw() +
    scale_y_log10() +
    xlab("Number of loci $\\times 1000$ ($L$)") +
    ylab("") +
    Article2.env$gtheme +
    theme(legend.position = c(0.61,0.22)) +
    Article2.env$scale.linetype +
    Article2.env$scale.color +
    guides(linetype = guide_legend(nrow = 2))

  pl <- cowplot::plot_grid(pl.it.n, pl.it.L, pl.time.n, pl.time.L, ncol = 2, labels = c("A", "B", "C", "D"))


}
