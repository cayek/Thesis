library(testthat)
context("3Article_GSE42861")


flog.threshold(2)

test_that("Article3_GSE42861_methods", {

  methods <-  Article3_GSE42861_methods(Ks = c(2,3,4),
                                        lambdas = c(1e-4, 1e0, 1e10),
                                        sparse.prop = c(0.1))
  expect_equal(length(methods), 9 + 3 + 3 + 1)

  sapply(methods, function(m) m$nickname)
  expect_equal(unique(sapply(methods, function(m) strsplit(m$nickname, split = "\\|")[[1]][1])),
               c("lfmmRidge", "glm", "refactor", "lfmmLasso"))

})

test_that("Article3_GSE42861", {
  G.file <- "~/Projects/Thesis/Data/GSE42861/betanormalized_metylationlvl.filtered.LMresidu.sample.rds"
  X.file <- "~/Projects/Thesis/Data/GSE42861/X.sample.rds"

  skip_if_not(file.exists(G.file))

  s <- TrueSampler(G.file = G.file,
                   X.file = X.file,
                   outlier.file = NULL)
  dat <- sampl(s)
  set.seed(54445)
  exp <- Article3_GSE42861(dat = dat,
                           dat.name = "GSE42861 sample",
                           cluster.nb = NULL,
                           Ks = c(2,3),
                           lambdas = c(1e-4, 1e0),
                           sparse.prop = c(0.1),
                           save = FALSE,
                           bypass = TRUE)

  expect_equal(unique(exp$df.res$method), c("lfmmRidge",  "glm"     ,   "refactor" ,  "lfmmLasso"))
  expect_equal(unique(exp$df.res$K), c(2, 3, NA))
  expect_equal(unique(exp$df.res$lambda), c(1e-4, 1.0, NA, 0.010082447,0.009442308))
  expect_equal(unique(exp$df.res$sparse.prop), c(NA, 0.1))

  Article3_runExp_plotB(exp, 0.05, "lfmmRidge")
  Article3_runExp_manhattan(exp, 0.05, "lfmmRidge")

  Article3_runExp_plotB(exp, 0.05, "glm")
  Article3_runExp_manhattan(exp, 0.05, "glm")

  Article3_runExp_plotB(exp, 0.05, "refactor") ## NA because B not computed with this method
  Article3_runExp_manhattan(exp, 0.05, "refactor")

  Article3_runExp_plotB(exp, 0.05, "lfmmLasso")
  Article3_runExp_manhattan(exp, 0.05, "lfmmLasso")

})


test_that("Article3_GSE42861_methods on whole dataset", {

  skip("too long")
  G.file <- "~/Projects/Thesis/Data/GSE42861/betanormalized_metylationlvl.filtered.rds"
  X.file <- "~/Projects/Thesis/Data/GSE42861/X.rds"

  set.seed(3547)
  s <- TrueSampler(G.file = G.file,
                   X.file = X.file,
                   outlier.file = NULL,
                   n = 100,
                   L = 1000)
  dat <- sampl(s)
  dat$X <- dat$X[,1,drop=FALSE] ## keep only first covariate
  exp <- Article3_GSE42861(dat = dat,
                           dat.name = "betanormalized_metylationlvl.filtered.LMresidu.rds",
                           cluster.nb = NULL,
                           Ks = c(2,3,4),
                           lambdas = c(1e-4, 1e0, 1e10),
                           sparse.prop = c(0.1),
                           save = FALSE,
                           bypass = TRUE)


})

test_that("Article3_GSE42861_plot_method", {
  skip("too big")
  m <- finalLm()
  Article3_GSE42861_run_method(m, plot = TRUE, save.expr = FALSE,
                               s = sampl)

})
