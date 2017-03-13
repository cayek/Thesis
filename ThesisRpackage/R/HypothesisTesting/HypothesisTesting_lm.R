################################################################################
# lm + zscore on G - C


#' lm + zscore on G - C
#'
#' perform a linear reg on G - C then make a classic zscore hypothesis testing
#'
#'
#'
#' @export
lm_zscore <- function(calibrate = FALSE, correctionByC = TRUE,
                      sigma.computation = c("lm", "bootstrap", "lm+df"),
                      bootstrap.func = PairedBoostrap,
                      bootstrap.rep = 30) {
  Functor(fun = function(m, dat) {

    lm.method <- ClassicLinearMethod(center = FALSE)
    d <- ncol(dat$X)

    # G -C or U used as co variable ?
    if (correctionByC) {
      dat$G <- sweep(dat$G, 2, m$mu) - m$C
      # lm
    } else {
      dat$G <- sweep(dat$G, 2, m$mu)
      # lm
      dat$X <- cbind(dat$X, m$U)
    }
    lm.res <- fit(lm.method, dat)## if there is missing data, there are take into acount by lm

    # calibrate ?
    if (calibrate) {
      # zscorepvalue.functor <- GifCalibratedZscore()
      zscorepvalue.functor <- FdrtoolsCalibratedZscore()
    } else {
      zscorepvalue.functor <- NormalZscore()
    }

    # computation of sigma
    ## report computation of residual
    m$epsilon <- lm.res$epsilon
    m$epsilon.sigma2 <- lm.res$epsilon.sigma2
    if (sigma.computation[1] == "lm") {
      m$B.sigma2 <- lm.res$B.sigma2
    } else if (sigma.computation[1] == "bootstrap") {
      sigma2.func <- Bootstrap(bootstrap.func = bootstrap.func,
                               bootstrap.rep = bootstrap.rep)

      m$B.sigma2 <- sigma2.func$fun(m = lm.method,
                                    dat = dat)
    } else if (sigma.computation[1] == "lm+df") {
      reduced.df <- m$K + ifelse(m$center, 1, 0) + d

      m$epsilon.sigma2 <- epsilon.sigma2(lm.res$epsilon,
                                         reduced.df = reduced.df)
      m$B.sigma2 <- B.sigma2(epsilon.sigma2 = m$epsilon.sigma2,
                             X = dat$X,
                             lambda = 0.0)
    }

    # compute score
    m$score <- (lm.res$B[1:d,,drop = FALSE] - 0.0) / sqrt(m$B.sigma2[1:d,,drop = FALSE])
    # compute p.value
    m$pvalue <- zscorepvalue.functor$fun(m$score)
    m$minus.log.pvalue <- -log(m$pvalue)

    m

  },
  name = paste0("lm+zscore","|calibrate=",calibrate))
}

