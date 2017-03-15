library(testthat)
library(Article3Package)
context("experiment on GSE42861")

test_that("Experiment on GSE42861 sample", {

  G.file <- "~/Projects/Data2016_2017/GSE42861/betanormalized_metylationlvl.sample.rds"
  X.file <- "~/Projects/Data2016_2017/GSE42861/X.sample.rds"

  skip_if_not(file.exists(G.file))

  s <- TrueSampler(G.file = G.file,
                   X.file = X.file,
                   outlier.file = NULL,
                   n = 100,
                   L = 800)

  # cl <- parallel::makeCluster(2)
  # doParallel::registerDoParallel(cl)
  exp <- GSE42861_experiment(s, save = FALSE)

  GSE42861_plot(exp)
})

test_that("lfmm on GSE42861 sample", {

  G.file <- "~/Projects/Data2016_2017/GSE42861/betanormalized_metylationlvl.sample.rds"
  X.file <- "~/Projects/Data2016_2017/GSE42861/X.sample.rds"

  skip_if_not(file.exists(G.file))

  s <- TrueSampler(G.file = G.file,
                   X.file = X.file,
                   outlier.file = NULL,
                   n = 100,
                   L = 800)

  # a run
  dat <- sampl(s)
  m <- finalLfmmRdigeMethod(5, 1e-10, gif = TRUE)
  m <- run(m, dat)


  # experiment
  # cl <- parallel::makeCluster(2)
  # doParallel::registerDoParallel(cl)
  lambdas <- c(1e-10, 1e0, 1e2, 1e10)
  Ks <- c(5,6,7)
  exp <- HGDB_runs(s, Ks = Ks, lambdas = lambdas, save = FALSE)


})

test_that("long_GSE42861_CrossVal", {

  G.file <- "~/Projects/Data2016_2017/GSE42861/betanormalized_metylationlvl.sample.rds"
  X.file <- "~/Projects/Data2016_2017/GSE42861/X.sample.rds"

  skip_if_not(file.exists(G.file))


  exp <- long_GSE42861_CrossVal(G.file = G.file,
                                X.file = X.file,
                                rep = 1,
                                save = FALSE,
                                lambdas = 1e-2,
                                bypass = TRUE)

  plot(exp$crossvalidation.res, "imputation_error")

  ## rmk : not converging... Why?

})


test_that("long_GSE42861_lfmm_glm", {
  G.file <- "~/Projects/Data2016_2017/GSE42861/betanormalized_metylationlvl.sample.rds"
  X.file <- "~/Projects/Data2016_2017/GSE42861/X.sample.rds"

  skip_if_not(file.exists(G.file))
  skip("run and see")

  exp <- long_GSE42861_lfmm_glm(K.lfmm = 6,
                                K.refactor = 6,
                                G.file = G.file,
                                X.file = X.file,
                                lambda = 1e1,
                                save = FALSE,
                                bypass = TRUE)
  expect_equal(dim(exp$df.res), c(8000, 4))


  ggplot(exp$df.res %>% dplyr::filter(variable.name == "pvalue")) +
    stat_qq(aes(sample = -log10(estimate)),
            distribution = stats::qexp, dparams = list(rate = log(10))) +
    geom_abline(slope = 1, intercept = 0) +
    facet_grid(method.name~.) +
    ggtitle("-log10(pvalue) qqplot")
  # lfmm seem not well calibrated...

})
