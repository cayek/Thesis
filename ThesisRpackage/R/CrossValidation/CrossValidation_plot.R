#' @export
plot.CrossValidation <- function(cv, color = c("lambda", "K")) {

  toplot <- cv$errs
  toplot <- toplot %>%
    group_by(lambda, K) %>%
    summarise(PRESS.mean = mean(PRESS),
              PRESS.se = sd(PRESS) / sqrt(length(PRESS))) %>%
    ungroup()

  if (color[1] == "K") {
    g <- ggplot(toplot, aes(x = K, y = PRESS.mean, color = as.factor(lambda)))
  } else if (color[1] == "lambda") {
    g <- ggplot(toplot, aes(x = lambda, y = PRESS.mean, color = as.factor(K))) +
      scale_x_log10()
  }

  g +
    geom_point() +
    geom_line() +
    geom_errorbar(aes(ymin = PRESS.mean - PRESS.se,
                      ymax = PRESS.mean + PRESS.se))

}
