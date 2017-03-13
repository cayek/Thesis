library(testthat)
library(Article3Package)
context("Gypsa seminar")

test_that("long_gypsa_simulation and plot no missing", {

  skip("run it manually")

  exp <- long_gypsa_simulation(outlier.prop = 0.1,
                               exp.rep = 2,
                               fast.only = TRUE,
                               n = NULL, L = 10000,
                               cluster.nb = NULL,
                               save = FALSE, bypass = TRUE)

  plot_gypsa_simulation(exp,
                        output.name = NULL,
                        ratio.width = 0.8,
                        ratio.height = 0.4,
                        dir = "~/Projects/Article3/Slides/GypsaSeminar/Images/",
                        save = FALSE,
                        dpi = 600,
                        plot.type = c("pvalue.grid", "precision.recall"))


  plot_gypsa_simulation(exp,
                        output.name = NULL,
                        ratio.width = 0.8,
                        ratio.height = 0.4,
                        dir = "~/Projects/Article3/Slides/GypsaSeminar/Images/",
                        save = FALSE,
                        dpi = 600,
                        plot.type = c("precision.recall"))




})


test_that("long_gypsa_simulation and plot with missing", {

  skip("run it manually")

  exp <- long_gypsa_simulation(outlier.prop = 0.15,
                               missing.prop = 0.2,
                               exp.rep = 1,
                               fast.only = TRUE,
                               n = NULL, L = 10000,
                               K = 3,
                               cs = c(0.6, 0.3, 0.0),
                               cluster.nb = NULL,
                               save = FALSE, bypass = TRUE)

  plot(exp, plot.type = "precision.recall", summary_bin = TRUE, geom = "point")

  plot_gypsa_simulation(exp,
                        output.name = NULL,
                        ratio.width = 0.8,
                        ratio.height = 0.4,
                        dir = "~/Projects/Article3/Slides/GypsaSeminar/Images/",
                        save = FALSE,
                        dpi = 600,
                        plot.type = c("pvalue.grid", "precision.recall"))


  plot_gypsa_simulation(exp,
                        output.name = NULL,
                        ratio.width = 0.8,
                        ratio.height = 0.4,
                        dir = "~/Projects/Article3/Slides/GypsaSeminar/Images/",
                        save = FALSE,
                        dpi = 600,
                        plot.type = c("precision.recall"))




})
