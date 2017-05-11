#' @export
Article3_GSE42861_methods <- function(Ks,
                                      lambdas,
                                      sparse.prop) {

  ht.func <- phenotypeWayReg_glm_score(calibrate = FALSE,
                                       family = binomial,
                                       factorized.X1 = TRUE)


  methods <- list()
  ## Ridgelfmm
  ridgeLFMM.const <- function(K, lambda, nickname) {
    RidgeLFMMMethod(K = K,
                    hypothesis.testing.method = ht.func,
                    lambda = lambda,
                    nickname = nickname)
  }

  methods <- c(methods, paramGrid(ridgeLFMM.const, nickname.base = "lfmmRidge",
                                  K = Ks,
                                  lambda = lambdas))

  ## only glm
  methods <-  c(methods,
                list(ClassicLinearMethod(hypothesis.testing.method = ht.func, nickname = "glm")))

  ## refactor
  refactor.const <- function(K, nickname) {
    m <- finalRefactorMethod(K = K)
    m$hypothesis.testing.method <- ht.func
    m$nickname <- nickname
    m
  }
  methods <- c(methods,
               paramGrid(refactor.const, nickname.base = "refactor",
                         K = Ks))

  ## lasso lfmm
  lassoLFMM.const <- function(K, sparse.prop, nickname) {
    m <- finalLfmmLassoMethod(K = K, sparse.prop = sparse.prop)
    m$hypothesis.testing.method <- ht.func
    m$nickname <- nickname
    m
  }

  methods <- c(methods, paramGrid(lassoLFMM.const, nickname.base = "lfmmLasso",
                                  K = Ks,
                                  sparse.prop = sparse.prop))

  methods
}


#' @export
Article3_GSE42861 <- function(dat, dat.name, cluster.nb,
                              Ks, lambdas, sparse.prop,
                              save = TRUE, bypass = FALSE) {
  Article3_runExp(dat = dat,
                  dat.name = dat.name,
                  methods = Article3_GSE42861_methods(Ks = Ks,
                                                      lambdas = lambdas,
                                                      sparse.prop = sparse.prop),
                  cluster.nb = cluster.nb, save = save,
                  bypass = bypass)

}

#' @export
Article3_GSE42861_sampler <- function() {
  X <- readRDS("~/Projects/Thesis/Data/ThesisDataset/3Article/GSE42861/X.rds")
  X <- X[,1, drop = FALSE]
  
  s <- TrueSampler(G.file = "~/Projects/Thesis/Data/ThesisDataset/3Article/GSE42861/G.rds",
                   X.file = X,
                   outlier.file = "~/Projects/Thesis/Data/ThesisDataset/3Article/GSE42861/candidates.rds",
                   n = NULL,
                   L = NULL)
  s
}


#' @export
Article3_GSE42861_run_method <- function(m) {

  expr <- Experiment(name = "Article3_GSE42861_run_method",
                    description = make_description("Article3_GSE42861_run_method",
                                                   m.name = m$nickname,
                                                   K = m$K))
  ## run 
  s <- Article3_GSE42861_sampler()
  dat <- sampl(s)
  candidates <- dat$outlier
  m <- run(m, dat)

  ## no calibration
  expr$pl <- qqplott(m, candidates)
  print(expr$pl)

  ## with calibration
  print("== calibrate")
  expr$pl.cal <- qqplott(m.cal, candidates)
  print(expr$pl.cal)

  expr$m <- m
  expr$m.cal <- m.cal
  expr
}
