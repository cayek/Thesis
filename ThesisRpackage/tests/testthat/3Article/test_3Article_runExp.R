library(testthat)
context("3Article_runExp")

test_that("Article3_runExp", {


  s <- NormalSampler2(n = 100,
                      L = 1000,
                      K = 3)


  exp <- Article3_runExp(s = s,
                  s.name = "NormalSampler2",
                  Ks = c(2,3,4),
                  lambdas = c(1e-5, 1e0, 1e5),
                  nb.rep = 1,
                  m = finalLfmmRdigeMethod(K = NULL,
                                           lambda = NULL,
                                           calibrate = TRUE),
                  m.name = m$nickname,
                  cluster.nb = NULL,
                  save = FALSE, bypass = TRUE)
  expect_equal(exp$name, "Article3_runExp")
  expect_equal(exp$description,
              "Article3_runExp with m.name=RidgeLfmm s.name=NormalSampler2 lambdas=1e-05|1|1e+05 Ks=2|3|4 ")


  plot(exp, threshold = 0.05)
})
