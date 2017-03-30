library(testthat)
context("3Article_withMissingValueResults")

test_that("3Article_withMissingValueResults with SparseMissingValueSampler", {
    s <- NormalSampler2(n = 100,
                        L = 1000,
                        K = 3,
                        prop.outlier = 0.10,
                        cs = c(0.9,0.3,0.0)) %>%
      SparseMissingValueSampler(missing.prop = 0.1, missing.prop.by.loci = 0.5)


    exp <- Article3_withMissingValueResults(s = s,
                                            s.name = "NormalSampler2",
                                            nb.rep = 1,
                                            lambda = 1e-1,
                                            K = 3 + 1,
                                            cluster.nb = NULL,
                                            fast.only = FALSE,
                                            save = FALSE, bypass = TRUE)
    plot(exp) ## prior impute => lfmm learn structure introduce by imputation !!

})

test_that("3Article_withMissingValueResults with MissingValueSampler", {
  s <- NormalSampler2(n = 100,
                      L = 1000,
                      K = 3,
                      prop.outlier = 0.10,
                      cs = c(0.9,0.3,0.0)) %>%
    MissingValueSampler(missing.prop = 0.25)


  exp <- Article3_withMissingValueResults(s = s,
                                          s.name = "NormalSampler2",
                                          nb.rep = 1,
                                          lambda = 1e-1,
                                          K = 3,
                                          cluster.nb = NULL,
                                          save = FALSE, bypass = TRUE)
  plot(exp)

})
