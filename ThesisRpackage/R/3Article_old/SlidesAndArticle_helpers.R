################################################################################
# globalvar
Article3.globalVar <- list(
  palette.color.blind = c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"), # http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/,
  unit = "mm",
  slide.width = 128,
  slide.height = 96,
  scientific_10 = function(x) {
    parse(text = gsub("e", " %*% 10^", scales::scientific_format()(x)))
  }
)

################################################################################
# helpers

save_plot <- function(p, fig.width, fig.height, dir, filename, units, dpi) {

  assertthat::is.dir(dir)

  ggsave(filename = filename,
         path = dir,
         plot = p,
         device = "png",
         width = fig.width,
         height = fig.height,
         units = units,
         dpi = dpi)
}

################################################################################
# Athaliana

#' @export
Athaliana_table <- function(n = 4, L = 3) {

  library(knitr)

  ## retrieve data

  G.file <- "~/Projects/Data2016_2017/AthalianaGegMapLines/call_method_75/call_method_75_TAIR9.G.rds"
  X.file <- "~/Projects/Data2016_2017/AthalianaGegMapLines/call_method_75/NorthSouthX.rds"

  s <- TrueSampler(G.file = G.file,
                   X.file = X.file,
                   outlier.file = NULL,
                   n = n,
                   L = L)

  dat <- sampl(s)

  colnames(dat$G) <- c("chr: 1 pos: 657", "chr: 1 pos: 3102", "chr: 1 pos: 4268")

  kable(as.data.frame(dat$G), format = "latex", caption = "A little SNP matrix.")
}

#' @export
Athaliana_plotB <- function(output.name,
                            ratio.width = 0.8,
                            ratio.height = 0.4,
                            dir = "./Images/",
                            bench.dir = "~/Projects/Article3/Article3Package/BenchmarkDump/",
                            print = FALSE,
                            save = TRUE,
                            dpi = 600,
                            plot.type = c("lmB+point", "lmB+hist","lmB+locfdr", "lmB+lfmmB")) {

  ## assert
  assertthat::is.dir(bench.dir)


  exp <- retrieveExperiment(id = 15, bench.dir = bench.dir)

  method.names <- unique(exp$df.res$method.name)
  assertthat::assert_that("lm" %in% method.names)
  assertthat::assert_that(grepl("call_method_75_TAIR9.G.rds and NorthSouthX.rds", exp$description))


  ## global
  aux_locfdr <- function(m.name, df) {
    res <- list()
    fdr.threshold <- 0.05

    toplot <- exp$df.res %>%
      dplyr::filter(method.name == m.name, variable.name == "score1")
    na.score <- which(is.na(toplot$estimate))
    toplot <- toplot[-na.score,]

    locfdr.lm <- locfdr::locfdr(toplot$estimate, df = df)
    fdr <- locfdr.lm$fdr

    res$p1 <- ggplot(toplot, aes(estimate)) +
      geom_histogram(fill = Article3.globalVar$palette.color.blind[3],
                     color = Article3.globalVar$palette.color.blind[1],
                     aes(y = ..density..),
                     bins = 100) +
      stat_function(fun = dnorm, args = list( mean = locfdr.lm$fp0[3,1], sd = locfdr.lm$fp0[3,2])) +
      xlab("zscore")

    toplot <- exp$df.res %>%
      dplyr::filter(method.name == m.name, variable.name == "B1")
    toplot <- toplot[-na.score,]

    toplot <- toplot %>% mutate(color = fdr < fdr.threshold)
    res$p2 <- ggplot(toplot, aes(x = index, y = estimate, color = color)) +
      geom_point(size = 0.2) +
      geom_point(data = toplot %>% dplyr::filter(fdr < fdr.threshold), size = 0.4) +
      scale_color_manual(guide = FALSE,
                         values = c(Article3.globalVar$palette.color.blind[3],
                                    Article3.globalVar$palette.color.blind[2])) +
      scale_y_continuous(name = "B",
                         labels = scales::scientific_format(),
                         breaks = c(min(toplot$estimate), 0.0, max(toplot$estimate))) +
      xlab("locus")

    res$p <- cowplot::plot_grid(res$p1, res$p2, ncol = 1, nrow = 2)
    res
  }

  if (plot.type == "lmB+point") {
  # only lm
  toplot <- exp$df.res %>%
    dplyr::filter(method.name == "lm", variable.name == "B1")

  p <- ggplot(toplot, aes(x = index, y = estimate)) +
    geom_point(color = Article3.globalVar$palette.color.blind[3],
               size = 0.2) +
    scale_y_continuous(name = "B",
                       labels = scales::scientific_format(),
                       breaks = c(min(toplot$estimate), 0.0, max(toplot$estimate))) +
    xlab("locus")

  } else if (plot.type == "lmB+hist") {
    # only lm
    toplot <- exp$df.res %>%
      dplyr::filter(method.name == "lm", variable.name == "score1")

    p <- ggplot(toplot, aes(estimate)) +
      geom_histogram(fill = Article3.globalVar$palette.color.blind[3],
                     color = Article3.globalVar$palette.color.blind[1],
                     aes(y = ..density..),
                     bins = 40) +
      stat_function(fun = dnorm) +
      scale_x_continuous(name = "zscore", limits = c(-4,4))

  } else if (plot.type == "lmB+locfdr") {


    p <- aux_locfdr(m.name = "lm", df = 14)$p

  } else if (plot.type == "lmB+lfmmB") {
    p.lm <- aux_locfdr(m.name = "lm", df = 14)$p2 +
      ggtitle("lm") +
      theme(axis.title.x = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank())
    p.lfmm <- aux_locfdr(m.name = method.names[2], df = 8)$p2 +
      ggtitle("lm with lattent factor")
    p <- cowplot::plot_grid(p.lm, p.lfmm, ncol = 1, nrow = 2)
  }

  if (print) {
    print(p)
  }

  if (save) {
    save_plot(p, fig.width = ratio.width * Article3.globalVar$slide.width,
              fig.height = ratio.height * Article3.globalVar$slide.height,
              dir = dir, filename = output.name,
              units = Article3.globalVar$unit,
              dpi = dpi)
  }
  p
}

#' @export
Athaliana_map <- function(output.name,
                          ratio.width = 0.8,
                          ratio.height = 0.4,
                          dir = "./Images/",
                          bench.dir = "~/Projects/Article3/Article3Package/BenchmarkDump/",
                          print = FALSE,
                          save = TRUE,
                          dpi = 600,
                          point.size = 0.4,
                          annot.size = 6,
                          color = FALSE) {

  require(TESS3ArticlePackage)

  if (!color) {
    res <- Figure5_map(point.color = Article3.globalVar$palette.color.blind[3],
                       point.size = point.size)
    res$mappl$layers <- res$mappl$layers[2:3]

  } else {
    res <- Figure5_map(point.color = "black",
                       point.size = point.size)
  }

  p <- res$mappl +
    geom_segment(aes(x = -15, y = 40, xend = -15, yend = 60), arrow = arrow(length = unit(0.7, "cm"))) +
    annotate("text", x = -13.5, y = 50, label = "X", size = annot.size)

  if (print) {
    print(p)
  }

  if (save) {
    save_plot(p, fig.width = ratio.width * Article3.globalVar$slide.width,
              fig.height = ratio.height * Article3.globalVar$slide.height,
              dir = dir, filename = output.name,
              units = Article3.globalVar$unit,
              dpi = dpi)
  }
  p
}
