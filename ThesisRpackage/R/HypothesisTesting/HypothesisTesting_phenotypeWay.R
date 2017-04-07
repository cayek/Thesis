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
    res <- list()
    n <- nrow(dat$G)
    L <- ncol(dat$G)

    if (ncol(dat$X) > 1) {
      flog.warning("We take X = dat$X[,1], other variables are used as co-variables")
    }
    X <- dat$X[,1]
    CoVar <- cbind(matrix(1,n,1), m$U, dat$X[,-1])
    d <- 1 + ncol(CoVar)


    ## model
    res$B <- array(NA, dim = c(1,L,d))
    res$B.sigma2 <- array(NA, dim = c(1,L,d))
    epsilon <- matrix(NA, n, 1)
    cov.aux <- matrix(NA, n, d)
    ## Compute B
    for (j in 1:L) {
      cov.aux <- cbind(dat$G[,j], CoVar)
      res$B[1,j,] <- B_ridge(X, cov.aux, 0.0)
      epsilon <- X - cov.aux %*% res$B[1,j,]
      eps.sigma2 <- epsilon.sigma2(epsilon, reduced.df = d)
      res$B.sigma2[1,j,] <- B.sigma2(epsilon.sigma2 = eps.sigma2,
                                     X = cov.aux,
                                     lambda = 0.0,
                                     A = X)
    }

    ## score and pvalue
    res$score <- matrix((res$B[1,,1] - 0) / sqrt(res$B.sigma2[1,,1]), 1, L)
    res$pvalue <- NormalZscore()$fun(res$score)

    ## calibrate ?
    if (calibrate) {
      zscorepvalue.functor <- FdrtoolsCalibratedZscore()
      res$pvalue <- zscorepvalue.functor$fun(res$score)
    } 

    res

  },
  name = paste0("phenotypeWayLm","|calibrate=",calibrate))
}
