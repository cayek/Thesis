library(testthat)
library(Article3Package)
context("GenerativeTest")

test_that("Normal Sampler", {
  s <- NormalSampler(n = 50,
                     L = 1000,
                     K = 4,
                     prop.outlier = 0.2,
                     sigma = 0.2,
                     c = 0.6,
                     mean.B = 0.0,
                     sd.mu = 1.0,
                     mean.mu = 0.5)
  # print(s)
  expect_equal(class(s), c("NormalSampler", "Sampler"))
  dat <- sampl(s)
  plot(dat)
  # plot the variance
  variance <- dat$G %>%
    purrr::array_branch(2) %>%
    purrr::map_dbl(var)
  gplot_stat(variance,
             outlier = dat$outlier) +
    geom_point(aes(x = index, color = outlier, y = stat))
})


test_that("Logistic Sampler", {
  s <- LogisticSampler(n = 50,
                     L = 1000,
                     K = 4,
                     prop.outlier = 0.2,
                     c = 0.6,
                     mean.B = 0.0,
                     sd.mu = 0.0,
                     mean.mu = 0.0)
  # print(s)
  expect_equal(class(s), c("LogisticSampler", "Sampler"))
  dat <- sampl(s)
  plot(dat)
  # plot the variance
  variance <- dat$G %>%
    purrr::array_branch(2) %>%
    purrr::map_dbl(var)
  gplot_stat(variance,
             outlier = dat$outlier) +
    geom_point(aes(x = index, color = outlier, y = stat))
})
