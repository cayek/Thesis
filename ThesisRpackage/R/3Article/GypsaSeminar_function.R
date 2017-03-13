################################################################################
# ** Simulation with $x%$ of outlier

#' @export
long_gypsa_simulation <- function(outlier.prop,
                                  missing.prop = NULL,
                                  exp.rep = 5,
                                  fast.only = FALSE,
                                  n = NULL, L = 10000,
                                  K = 4,
                                  cs = c(0.6, 0.3, 0.0, 0.0),
                                  cluster.nb = NULL,
                                  save = TRUE, bypass = FALSE){

  KrakTest(bypass)

  if (!is.null(cluster.nb)) {
    cl <- parallel::makeCluster(cluster.nb)
    doParallel::registerDoParallel(cl)
  }

  G.file <- "~/Projects/Data2016_2017/1000Genomes/Phase3Chrm22/European_Chrm22.rds"
  s <- FromTrueSampler(G.file = G.file,
                       n = n,
                       L = L,
                       K = K,
                       prop.outlier = outlier.prop,
                       rho = NULL,
                       cs = cs,
                       round = FALSE)

  bench <- finalBench(K = K,
                      lambda = 1e-1,
                      gif = FALSE,
                      with.missing = !is.null(missing.prop),
                      fast.only = fast.only)

  if (!is.null(missing.prop)) {
    s <- s %>%
      MissingValueSampler(missing.prop = missing.prop)
    bench <- bench[c("lfmmRidge",
                     "oracle",
                     "lfmm.ridge.impute.first")]
  }

  exp <- do.call(FDRControlExperiment,c(list(nb.rep = exp.rep, s = s), bench))
  exp <- runExperiment(exp)

  exp$description <- paste0("long_gypsa_simulation_missing with outlier.prop=",
                            outlier.prop, "|n=", n,"|L=", L, "|missing.prop=", missing.prop)

  # save exp
  if (save) {
    dumpExperiment(exp)
  }
  exp

}

#' @export
plot_gypsa_simulation <- function(exp,
                                  output.name,
                                  ratio.width = 0.9,
                                  ratio.height = 0.8,
                                  dir = "~/Projects/Article3/Slides/GypsaSeminar/Images/",
                                  save = TRUE,
                                  dpi = 600,
                                  plot.type = c("pvalue.grid", "precision.recall")) {


  if (plot.type == "pvalue.grid") {
    p <- ggplot(exp$result$df.pvalue ,
                aes(x = expected.fd, y = true.fd  - expected.fd )) +
      facet_grid(method ~ .) +
      layer(geom = "point", mapping = aes(color = method, group = as.factor(rep)),
            params = list(size = 0.1),
            stat = "identity", position = "identity") +
      stat_summary_bin(fun.y = median, geom = "point") +
      stat_summary_bin(fun.y = function(x) quantile(x,0.9), geom = "point", color = "blue4") +
      stat_summary_bin(fun.y = function(x) quantile(x,0.1), geom = "point", color = "blue4") +
      ylab("observed false positives – expected false positives") +
      xlab("expected false positives") +
      guides(colour = "none")
  } else if (plot.type == "precision.recall") {
    p <- ggplot(exp$result$df.pvalue, aes(x = true.power, y = 1 - true.fdr,
                                          color = method)) +
      geom_smooth() +
      ylab("1 - observed false positives rate") +
      xlab("observed power")
  }



  if (save) {
    save_plot(p, fig.width = ratio.width * Article3.globalVar$slide.width,
              fig.height = ratio.height * Article3.globalVar$slide.height,
              dir = dir, filename = output.name,
              units = Article3.globalVar$unit,
              dpi = dpi)
  }
  p
}

################################################################################
# ** Simulation with $x%$ of outlier cross validation

#' @export
long_gypsa_simulation_crossvalidation <- function(outlier.prop,
                                                  lambdas = c(1e-10, 1e0, 1e2, 1e10),
                                                  missing.prop = 0.3,
                                                  rep = 5,
                                                  fast.only = FALSE,
                                                  n = NULL, L = 10000,
                                                  K = 4,
                                                  cs = c(0.6, 0.3, 0.0, 0.0),
                                                  cluster.nb = NULL,
                                                  save = TRUE, bypass = FALSE){

  KrakTest(bypass)

  if (!is.null(cluster.nb)) {
    cl <- parallel::makeCluster(cluster.nb)
    doParallel::registerDoParallel(cl)
  }

  G.file <- "~/Projects/Data2016_2017/1000Genomes/Phase3Chrm22/European_Chrm22.rds"
  s <- FromTrueSampler(G.file = G.file,
                       n = n,
                       L = L,
                       K = K,
                       prop.outlier = outlier.prop,
                       rho = NULL,
                       cs = cs,
                       round = FALSE)

  dat <- sampl(s)

  m <-  finalLfmmRdigeMethod(K = K,
                             lambda = NULL)

  description <- paste0("long_gypsa_simulation_crossvalidation with K=", K,
                        "and lambdas = ",paste(lambdas,collapse = '|'))


  exp <- Experiment(name = "long_gypsa_simulation_crossvalidation", description = description)
  exp$crossvalidation.res <-  crossvalidation_kfold_missingvalue(m = m,
                                                                 dat = dat,
                                                                 rep = rep,
                                                                 missing.prop = missing.prop,
                                                                 lambdas = lambdas)
  # save exp
  if (save) {
    dumpExperiment(exp)
  }
  exp

}

################################################################################
# pca

#' @export
plot_gypsa_pca <- function(exp,
                           output.name,
                           ratio.width = 0.9,
                           ratio.height = 0.8,
                           dir = "~/Projects/Article3/Slides/GypsaSeminar/Images/",
                           save = TRUE,
                           dpi = 600) {


  p <- plot(exp) +
    xlab("PC indice") +
    ylab("singular value") +
    guides(size = FALSE,
      color = guide_legend(title = "abs(cor(Uk, X))"))

  if (save) {
    save_plot(p, fig.width = ratio.width * Article3.globalVar$slide.width,
              fig.height = ratio.height * Article3.globalVar$slide.height,
              dir = dir, filename = output.name,
              units = Article3.globalVar$unit,
              dpi = dpi)
  }
  p
}

################################################################################
# GSE42861


plot_gypsa_GSE42861_res <- function(exp,
                                    output.name,
                                    fdr.threshold = 0.05,
                                    ratio.width = 0.9,
                                    ratio.height = 0.8,
                                    dir = "~/Projects/Article3/Slides/GypsaSeminar/Images/",
                                    save = TRUE,
                                    dpi = 600) {

  assertthat::assert_that(exp$description == "long_GSE42861_lfmm_glm with K=6and lambdas = 10")

  ## calibration with locfdr
  score.lfmm <- (exp$df.res %>%
    dplyr::filter(variable.name == "score", method.name == "lfmm"))$estimate
  locfdr.lfmm <- locfdr::locfdr(score.lfmm)

  score.refactor <- (exp$df.res %>%
                   dplyr::filter(variable.name == "score", method.name == "refactor"))$estimate
  locfdr.refactor <- locfdr::locfdr(score.refactor)

  cat("cor(score.lfmm, score.refactor) =", cor(score.lfmm, score.refactor), "\n")

  ## compute pavlue with adjusted H0
  toplot <- exp$df.res
  ### lfmm
  adj.pvalue.lfmm <- score.lfmm %>%
    sapply(function(z) 2 * pnorm(abs(z),mean = locfdr.lfmm$fp0[3,1],
                                 sd = locfdr.lfmm$fp0[3,2],
                                 lower.tail = FALSE))
  #hist(adj.pvalue.lfmm)
  ### refactor
  adj.pvalue.refactor <- score.refactor %>%
    sapply(function(z) 2 * pnorm(abs(z),mean = locfdr.refactor$fp0[3,1],
                                 sd = locfdr.refactor$fp0[3,2],
                                 lower.tail = FALSE))
  #hist(adj.pvalue.refactor)

  toplot <- rbind(
    tibble(method.name = "refactor", pvalue = adj.pvalue.refactor,
                   index = 1:length(adj.pvalue.refactor),
                   fdr = locfdr.refactor$fdr),
    tibble(method.name = "lfmm", pvalue = adj.pvalue.lfmm,
           index = 1:length(adj.pvalue.lfmm),
           fdr = locfdr.lfmm$fdr)
  )

  p <- ggplot(toplot, aes(x = index, y = -log10(pvalue), color = fdr < fdr.threshold)) +
    geom_point() +
    facet_grid(method.name ~ .) +
    guides(color = guide_legend(title = paste0("fdr < ", fdr.threshold))) +
    xlab("locus")

  if (save) {
    save_plot(p, fig.width = ratio.width * Article3.globalVar$slide.width,
              fig.height = ratio.height * Article3.globalVar$slide.height,
              dir = dir, filename = output.name,
              units = Article3.globalVar$unit,
              dpi = dpi)
  }
  p

}


################################################################################
# HGDP

plot_gypsa_HGDP_res <- function(exp,
                                output.name,
                                fdr.threshold = 0.05,
                                ratio.width = 0.9,
                                ratio.height = 0.8,
                                dir = "~/Projects/Article3/Slides/GypsaSeminar/Images/",
                                save = TRUE,
                                dpi = 600) {

  assertthat::assert_that(exp$description == "ComparisonExperiment on Hgdp_Li.rds and X_tmp.rds with methods: RidgeLfmm|lambda=0.01|K=20|gif=1||")

  print(unique(exp$df.res$method.name))

  ## calibration with locfdr
  score.lfmm <- (exp$df.res %>%
                   dplyr::filter(variable.name == "pvalue1", method.name == "RidgeLfmm|lambda=0.01|K=20|gif=1"))$estimate

  ### lfmm
  qvalue <- score.lfmm %>%
      qvalue::qvalue(fdr.level = fdr.threshold)

  toplot <-
    exp$df.res %>%
    dplyr::filter(variable.name == "pvalue1", method.name == "RidgeLfmm|lambda=0.01|K=20|gif=1")

  p <- ggplot(toplot, aes(x = index, y = -log10(pvalue), color = qvalue$significant)) +
    geom_point() +
    facet_grid(method.name ~ .) +
    guides(color = guide_legend(title = paste0("fdr < ", fdr.threshold))) +
    xlab("locus")


  ## name of locus
  G.file <- "~/Projects/Data2016_2017/Hgdp_Li/Hgdp_Li.rds"
  X.file <- "~/Projects/Data2016_2017/Hgdp_Li/X_tmp.rds"
  s <- TrueSampler(G.file = G.file,
                   X.file = X.file,
                   outlier.file = NULL,
                   n = NULL,
                   L = NULL)


  if (save) {
    save_plot(p, fig.width = ratio.width * Article3.globalVar$slide.width,
              fig.height = ratio.height * Article3.globalVar$slide.height,
              dir = dir, filename = output.name,
              units = Article3.globalVar$unit,
              dpi = dpi)
  }
  p

}



