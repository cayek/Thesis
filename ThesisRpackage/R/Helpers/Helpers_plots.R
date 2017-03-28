
#' Plot stat
#'
#'
#' @examples
#' library(Article3Package)
#'
#' K <- 5
#' s <- NormalSampler(100, 1000, K,
#'                    prop.outlier = 0.02,
#'                    c = 0.8) %>%
#'   MissingValueSampler(missing.prop = 0.0)
#'
#' m <- ClassicLinearMethod()
#' dat <- sampl(s)
#' m <- run(m, dat)
#' gplot_stat(m$score[1,] outlier = dat$outlier) +
#'  geom_point(aes(x = index, y = stat, color = outlier)) +
#'
#' @export
gplot_stat <- function(...,outlier) {

  toplot <- tibble(...)
  toplot <- toplot %>%
    mutate(index = 1:nrow(toplot),
           outlier = index %in% outlier) %>%
    gather(key = stat.name, value = stat, -index, -outlier)
  ggplot(toplot) +
    facet_grid(stat.name ~ ., scales = "free")
}
