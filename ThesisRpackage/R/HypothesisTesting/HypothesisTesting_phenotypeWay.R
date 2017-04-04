################################################################################
# phenotype way regression X ~ G_j + U


#' lm + zscore in the phenotype way i.e X ~ G_j + U
#'
#' we use R lm
#'
#' @export
phenotypeWayReg_glm_score <- function(calibrate = FALSE, family = gaussian, factorized.X1 = FALSE) {

  Functor(fun = function(m, dat) {

    if (ncol(dat$X) > 1) {
      warning("We take X = dat$X[,1], other variables are used as co-variables",
              call. = FALSE, immediate. = TRUE)
    }
    if (factorized.X1) {
      X <- as.factor(dat$X[,1])
    } else {
      X <- dat$X[,1]
    }
    CoVar <- cbind(m$U, dat$X[,-1])

    res <- list()

    n <- nrow(dat$G)
    L <- ncol(dat$G)

    res$score <- matrix(NA, 1, L)
    res$pvalue <- matrix(NA, 1, L)

    for (j in 1:L)
    {

      model <- glm(X ~ dat$G[,j] + CoVar, family = family)

      res$score[1,j] <- coef(summary(model))[2,3]
      res$pvalue[1,j] <- coef(summary(model))[2,4]
    }

    res

  },
  name = paste0("phenotypeWayLm","|calibrate=",calibrate))
}
