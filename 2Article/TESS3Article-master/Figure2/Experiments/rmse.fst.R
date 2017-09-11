DEBUG = FALSE
######################################
# Setup

# Install if not function
pkgTest <- function(x)
{
  if (!require(x,character.only = TRUE))
  {
    install.packages(x,dep=TRUE)
    if(!require(x,character.only = TRUE)) stop("Package not found")
  }
}


pkgTest("raster")
pkgTest("ggplot2")
pkgTest("reshape2")
pkgTest("dplyr")
pkgTest("gridExtra")
pkgTest("cowplot")
pkgTest("DescTools")
pkgTest("doParallel")
pkgTest("foreach")
pkgTest("devtools")
pkgTest("permute")
pkgTest("crayon")

# sNMF
if (!require("LEA")) {
  source("https://bioconductor.org/biocLite.R")
  biocLite("LEA")
  if(!require("LEA",character.only = TRUE)) stop("Package not found")
}

res.dir <- "./"
fig.dir <- "./"

# personal packages
#devtools::install_github("cayek/TESS3_encho_sen@experiment")
require("tess3rExperiment")

#devtools::install_github("cayek/TESS3_encho_sen@master")
require("tess3r")

cat(green(paste("DEBUG =", DEBUG,"\n")))
######################################
# Functions


fst.LEA <- function(project,run = 1, K, ploidy = 2){
  #require(LEA)
  ll = dim(LEA::G(project, K = K, run = run))[1]
  if (ploidy == 2) {freq = LEA::G(project, K = K, run = run)[seq(2,ll,by = 3),]/2 + LEA::G(project, K = K, run = run)[seq(3,ll,by = 3),] }
  else {freq = LEA::G(project, K = K, run = run)[seq(2,ll,by = 2),]}
  q = apply(LEA::Q(project, K = K, run = run), MARGIN = 2, mean)
  H.s = apply(freq*(1 - freq), MARGIN = 1, FUN = function(x) sum(q*x) )
  P.t = apply(freq, MARGIN = 1, FUN = function(x) sum(q*x) )
  return(1 - H.s/P.t/(1 - P.t))
}


tess3.wrapper <- function(data.list, K, method) {
  if (is.null(data.list$admixed.genotype)) {
    capture.output(res <- tess3rExperiment::tess3(X = data.list$X,
                                                  coord = data.list$coord,
                                                  K = K,
                                                  ploidy = data.list$ploidy,
                                                  lambda = 1.0,
                                                  W = data.list$W,
                                                  method = method,
                                                  max.iteration = 200,
                                                  tolerance = 1e-05,
                                                  openMP.core.num = 1,
                                                  Q.init = NULL,
                                                  mask = 0), file = "/dev/null")
  } else {
    capture.output(res <- tess3rExperiment::tess3(X = data.list$admixed.genotype,
                                                  coord = data.list$coord,
                                                  K = K,
                                                  ploidy = data.list$ploidy,
                                                  lambda = 1.0,
                                                  W = data.list$W,
                                                  method = method,
                                                  max.iteration = 200,
                                                  tolerance = 1e-05,
                                                  openMP.core.num = 1,
                                                  Q.init = NULL,
                                                  mask = 0), file = "/dev/null")
  }
  return(res)
}


snmf.wrapper <- function(data.list, K, alpha = 10) {
  file.geno <- paste0(tempfile(),".geno")
  if (is.null(data.list$admixed.genotype)) {
    LEA::write.geno(data.list$X, file.geno)
  } else {
    LEA::write.geno(data.list$admixed.genotype, file.geno)
  }
  capture.output(aux <- LEA::snmf(input.file = file.geno,
                                  K = K,
                                  project = "new",
                                  repetitions = 1,
                                  alpha = alpha,
                                  tolerance = 1e-05,
                                  entropy = FALSE,
                                  percentage = 0.05,
                                  I = 0,
                                  iterations = 200,
                                  ploidy = data.list$ploidy,
                                  seed = -1,
                                  CPU = 1,
                                  Q.input.file = ""), file = "/dev/null")
  snmf.run <- list(Q = LEA::Q(aux, K = K, run = 1),
                   G = LEA::G(aux, K = K, run = 1),
                   Fst = fst.LEA(aux, run = 1, K = K, ploidy = 1))
  return(snmf.run)
}


fig2.experiment <- function(simu.param, m.neutral, rep) {
  df <- foreach(m = m.neutral, .combine = 'rbind') %:%
    foreach(r = 1:rep, .combine = 'rbind') %dopar% {
      simu.param$m.neutral <- m
      boolFalse <- FALSE
      while (boolFalse == FALSE)
      {
        tryCatch({
          data.list <- sample.data(simu.param)
          boolFalse <- TRUE
        },error = function(e){
        },finally = {})
      }
      tess3.res <- tess3.wrapper(data.list, 2, "MCPA")
      snmf.res <- snmf.wrapper(data.list, 2)
      rbind( data.frame(rmseQ = tess3r::ComputeRmseWithBestPermutation(data.list$Q, tess3.res$Q),
                        rmseG = tess3r::ComputeRmseWithBestPermutation(data.list$Freq, GtoFreq(tess3.res$G, 1)),
                        method = "TESS3-APLS",
                        n = nrow(data.list$admixed.genotype),
                        L = ncol(data.list$admixed.genotype),
                        rep = r,
                        Fst = mean(data.list$Fst),
                        Fst.theorical = data.list$Fst.theorical,
                        m.neutral = m,
                        nsites.neutral = data.list$nsites.neutral,
                        migration.rate = 4 * m * simu.param$N0),
             data.frame(rmseQ = tess3r::ComputeRmseWithBestPermutation(data.list$Q, snmf.res$Q),
                        rmseG = tess3r::ComputeRmseWithBestPermutation(data.list$Freq, GtoFreq(snmf.res$G, 1)),
                        method = "sNMF",
                        n = nrow(data.list$admixed.genotype),
                        L = ncol(data.list$admixed.genotype),
                        rep = r,
                        Fst = mean(data.list$Fst),
                        Fst.theorical = data.list$Fst.theorical,
                        m.neutral = m,
                        nsites.neutral = data.list$nsites.neutral,
                        migration.rate = 4 * m * simu.param$N0))
    }
  return(df)
}


sample.data <- function(simu.param) {
  res <- tess3r::SampleGenoOFWithMs(n = simu.param$n,
                                    nsites.neutral = simu.param$nsites.neutral,
                                    nsites.selected = simu.param$nsites.selected,
                                    crossover.proba = simu.param$crossover.proba,
                                    m.neutral = simu.param$m.neutral,
                                    m.selected = simu.param$m.selected,
                                    mutation.rate.per.site = simu.param$mutation.rate.per.site,
                                    N0 = simu.param$N0,
                                    k = simu.param$k,
                                    min.maf = simu.param$min.maf,
                                    plot.debug = FALSE,
                                    tess3.ms = getOption("tess3.ms"))
  res$Fst.theorical <- 1 / (1 + 4 * simu.param$N0 * simu.param$m.neutral)
  return(res)
}


######################################
# Params

cat(green("== Test params\n"))
simu.param <- list(n = 500,
                   nsites.neutral = 1.2 * 1e5,
                   nsites.selected = 0,
                   crossover.proba = 0.25 * 1e-8,
                   m.neutral = 0.25 * 3 * 1e-6,
                   m.selected = NULL,
                   mutation.rate.per.site = 0.25 * 1e-7,
                   N0 = 1e6,
                   k = 0.5,
                   min.maf = 0.05)
data.list <- sample.data(simu.param)

plot(data.list$coord, col = rep(rainbow(2),each = data.list$n / 2))
mean(data.list$Fst)
dim(data.list$admixed.genotype)


barplot(t(data.list$Q), col = rainbow(2))
# data.list$W <- NULL
tess3.run <- tess3.wrapper(data.list, K = 2, method = "MCPA")
barplot(t(tess3.run$Q), col = rainbow(2))

snmf.run <- snmf.wrapper(data.list, K = 2)
barplot(t(snmf.run$Q), col = rainbow(2))

ComputeRmseWithBestPermutation(snmf.run$Q, data.list$Q)
ComputeRmseWithBestPermutation(tess3.run$Q, data.list$Q)

ComputeRmseWithBestPermutation(GtoFreq(snmf.run$G,1), data.list$Freq)
ComputeRmseWithBestPermutation(GtoFreq(tess3.run$G,1), data.list$Freq)


######################################
# Run experiments


simu.param <- list(n = 500,
                   nsites.neutral = 1.5 * 1e5,
                   nsites.selected = 0,
                   crossover.proba = 0.25 * 1e-8,
                   m.neutral = 0.25 * 3 * 1e-6,
                   m.selected = NULL,
                   mutation.rate.per.site = 0.25 * 1e-7,
                   N0 = 1e6,
                   k = 0.5,
                   min.maf = 0.05)

  cores = 16
  registerDoParallel(cores = cores)

  if (DEBUG) {
    m.neutral =  c(0.25 * 0.05 * 1e-6,
                   0.25 * 0.5 * 1e-6,
                   0.25 * 1 * 1e-6,
                   0.25 * 1.5 * 1e-6,
                   0.25 * 2 * 1e-6,
                   0.25 * 2.5 * 1e-6,
                   0.25 * 3 * 1e-6,
                   0.25 * 5 * 1e-6)
    rep <- 5
  } else {
    m.neutral =  c(0.25 * 0.05 * 1e-6,
                   0.25 * 0.5 * 1e-6,
                   0.25 * 1 * 1e-6,
                   0.25 * 1.5 * 1e-6,
                   0.25 * 2 * 1e-6,
                   0.25 * 2.5 * 1e-6,
                   0.25 * 3 * 1e-6,
                   0.25 * 5 * 1e-6)
    rep <- 5 #  do not work ... why?
  }

  df <- data.frame()

  # n = 50
  simu.param$n = 50
  ## L = 10k
  cat(green("== n = 50 & L = 10k \n"))
  simu.param$nsites.neutral = 1.5 * 1e4
  df <- rbind(fig2.experiment(simu.param, m.neutral = m.neutral, rep = rep), df)

  ## L = 200k
  cat(green("== n = 50 & L = 100k \n"))
  simu.param$nsites.neutral = 1.2 * 1e5
  df <- rbind(fig2.experiment(simu.param, m.neutral = m.neutral, rep = rep), df)

  # n = 500
  simu.param$n = 500
  ## L = 10k
  cat(green("== n = 500 & L = 10k \n"))
  simu.param$nsites.neutral = 1.5 * 1e4
  df <- rbind(fig2.experiment(simu.param, m.neutral = m.neutral, rep = rep), df)

  ## L = 100k
  cat(green("== n = 500 & L = 100k \n"))
  simu.param$nsites.neutral = 1.2 * 1e5
  df <- rbind(fig2.experiment(simu.param, m.neutral = m.neutral, rep = rep), df)

  cat(green("== Save result\n"))
  save(df, file = paste0(res.dir,"df.res"))
