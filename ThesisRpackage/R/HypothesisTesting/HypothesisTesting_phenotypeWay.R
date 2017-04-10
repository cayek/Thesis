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
      flog.warning("We take X = dat$X[,1], other variables are used as co-variables")
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
    res$B <- matrix(NA, 1, L)

    ## model
    ## I assume that if there is missing value glm remove them...
    ## RMK: there is an intercept by default in glm model
    if (ncol(CoVar) == 0) {
      modl.func <- function(j) {
        glm(X ~ dat$G[,j], family = family)
      }
    } else {
      modl.func <- function(j) {
        glm(X ~ dat$G[,j] + CoVar, family = family)
      }
    }

    for (j in 1:L)
    {
      model <- modl.func(j)
      res$B[1,j] <- coef(summary(model))[2,1]
      res$score[1,j] <- coef(summary(model))[2,3]
      res$pvalue[1,j] <- coef(summary(model))[2,4]
    }
    ## calibrate ?
    if (calibrate) {
      zscorepvalue.functor <- FdrtoolsCalibratedZscore()
      res$pvalue <- zscorepvalue.functor$fun(res$score)

    } 

    res

  },
  name = paste0("phenotypeWayGlm","|calibrate=",calibrate))
}

#' lm + zscore in the phenotype way i.e X ~ G_j + U
#'
#' we use R lm
#'
#' @export
phenotypeWayReg_lm_score <- function(calibrate = FALSE) {

  Functor(fun = function(m, dat) {
    n <- nrow(dat$G)
    L <- ncol(dat$G)

    if (ncol(dat$X) > 1) {
      flog.warning("We take X = dat$X[,1], other variables are used as co-variables")
    }
    X <- dat$X[,1]
    CoVar <- cbind(matrix(1,n,1), m$U, dat$X[,-1])
    d <- 1 + ncol(CoVar)


    ## model
    res <-  phenotypeWayReg_lm(Y = dat$X,
                               X = dat$G,
                               CoVar = CoVar)

    ## score and pvalue
    res$score <- matrix((res$B - 0) / sqrt(res$B.sigma2), 1, L)
    res$pvalue <- tscoreToPvalue(res$score, n - d)

    ## calibrate ?
    if (calibrate) {
      zscorepvalue.functor <- FdrtoolsCalibratedZscore()
      res$pvalue <- zscorepvalue.functor$fun(res$score)
    } 

    res

  },
  name = paste0("phenotypeWayLm","|calibrate=",calibrate))
}
