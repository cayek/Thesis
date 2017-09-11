library(foreach)
library(doParallel)
#library(tess3r)
#library(LEA)
library(DescTools)

# if (!require(tess3rOldExperiment)) {
#   devtools::install_github("cayek/TESS3/tess3r@experiment")
# }
# 
# 
# if (!require(tess3rExperiment)) {
#   devtools::install_github("cayek/TESS3_encho_sen@experiment")
# }

##################################################
############## UTILS #############################
##################################################

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

##################################################
############## WRAPPER ###########################
##################################################


tess3old.wrapper <- function(data.list, K, alpha) {
  if (is.null(data.list$admixed.genotype)) {
    capture.output(aux <- tess3rOldExperiment::TESS3(X = data.list$X, 
                                                     coord = data.list$coord, 
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
                                                     Q.input.file = "", 
                                                     W = data.list$W), file = "/dev/null")
    
  } else {
    capture.output(aux <- tess3rOldExperiment::TESS3(X = data.list$admixed.genotype, 
                                                     coord = data.list$coord, 
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
                                                     Q.input.file = "", 
                                                     W = data.list$W), file = "/dev/null")
  }
  tess3.old.run <- list(Q = tess3rOldExperiment::Q(aux, K = K, run = 1),
                        G = tess3rOldExperiment::G(aux, K = K, run = 1), 
                        times = aux@times)
  # system("rm -rf sdggsg578354qcs54*")
  return(tess3.old.run)
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
  file.geno <- paste0(tempfile(pattern = "sdggsg578354qcs54", tmpdir = "."),".geno")
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
  # system("rm -rf sdggsg578354qcs54*")
  return(snmf.run)
}


beforeadmixture.wrapper <- function(data.list) {
  res <- list()
  res$Fst <- data.list$Fst
  return(res)
}

##################################################
############## SAMPLER ###########################
##################################################



sample.data <- function(simu.param) {
  return(tess3r::SampleGenoOFWithMs(n = simu.param$n, 
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
                                    tess3.ms = getOption("tess3.ms")))
}

sampler.from.data <- function(X, coord) {
  function(n, L) {
    sn <- sample(1:nrow(X),n)
    sL <- sample(1:ncol(X),L)
    return(list(X = X[sn, sL], coord = coord[sn,], ploidy = max(X)))
  }
} 

##################################################
############## FIG 3 EXP FUNC ####################
##################################################


dataframeFst <- function(fst, outlier, ...) {
  df <- data.frame(index = 1:length(fst), fst = fst, outlier = 1:length(fst) %in% outlier, ...)
  m <- length(fst)
  m_0 <- length(outlier)
  # sort fst
  df <- dplyr::arrange(df, desc(fst))
  # fdr and power computation
  df <- dplyr::mutate(df, i = 1:m )
  df <- dplyr::mutate(df, power = cumsum(outlier) / m_0 )
  df <- dplyr::mutate(df, fdr = cumsum(!outlier) / i )
  return(df)
}

AUC.precision.recall <- function(fst, outlier) {
  df <- data.frame(index = 1:length(fst), fst = fst, outlier = 1:length(fst) %in% outlier)
  m <- length(fst)
  m_0 <- length(outlier)
  # sort fst
  df <- dplyr::arrange(df, desc(fst))
  # fdr and power computation
  df <- dplyr::mutate(df, i = 1:m )
  df <- dplyr::mutate(df, power = cumsum(outlier) / m_0 )
  df <- dplyr::mutate(df, fdr = cumsum(!outlier) / i )
  
  return(list(auc = DescTools::AUC(x = df$power, y = 1 - df$fdr), df = df))
  
}


fig3.experiment <- function(simu.param, m.ms, rep) {
  df <- foreach(m = m.ms, .combine = 'rbind') %:% 
    foreach(r = 1:rep, .combine = 'rbind') %dopar% {
      simu.param$m.selected <- simu.param$m.neutral / m
      data.list <- sample.data(simu.param)
      
      tess3.res <- tess3.wrapper(data.list, 2, "MCPA")
      tess3.res$auc <- AUC.precision.recall(tess3.res$Fst, data.list$selected.locus.index)$auc
      snmf.res <- snmf.wrapper(data.list, 2)
      snmf.res$auc <- AUC.precision.recall(snmf.res$Fst, data.list$selected.locus.index)$auc
      before.res <- beforeadmixture.wrapper(data.list)
      before.res$auc <- AUC.precision.recall(before.res$Fst, data.list$selected.locus.index)$auc
      
      rbind( data.frame(method = "TESS3-APLS",
                        n = nrow(data.list$admixed.genotype),
                        L = ncol(data.list$admixed.genotype),
                        rep = r, 
                        Fst = mean(data.list$Fst),
                        m.ms = m, 
                        nsites = data.list$nsites.neutral,
                        prop.selected = length(data.list$selected.locus.index) / data.list$L,
                        auc = tess3.res$auc), 
             
             data.frame(method = "sNMF",
                        n = nrow(data.list$admixed.genotype),
                        L = ncol(data.list$admixed.genotype),
                        rep = r, 
                        Fst = mean(data.list$Fst),
                        m.ms = m, 
                        nsites = data.list$nsites.neutral,
                        prop.selected = length(data.list$selected.locus.index) / data.list$L,
                        auc = snmf.res$auc),
             
             data.frame(method = "before admixture",
                        n = nrow(data.list$admixed.genotype),
                        L = ncol(data.list$admixed.genotype),
                        rep = r, 
                        Fst = mean(data.list$Fst),
                        m.ms = m, 
                        nsites = data.list$nsites.neutral,
                        prop.selected = length(data.list$selected.locus.index) / data.list$L,
                        auc = before.res$auc)
             
             )
    }
  return(df)
}


##################################################
############## FIG 2 EXP FUNC ####################
##################################################



##################################################
############## FIG 1 EXP FUNC ####################
##################################################


fig1.exp.n <- function(simu.param, ns, rep) {
  df <- foreach(n = ns, .combine = 'rbind') %:% 
    foreach(r = 1:rep, .combine = 'rbind') %dopar% {
      simu.param$n <- n
      data.list <- sample.data(simu.param)
      tess3MCPA.res <- tess3.wrapper(data.list, 2, "MCPA")
      tess3OQA.res <- tess3.wrapper(data.list, 2, "OQA")
      tess3Old.res <- tess3old.wrapper(data.list, 2, alpha = 0.03)
      rbind( data.frame(rmseQ = tess3r::ComputeRmseWithBestPermutation(data.list$Q, tess3MCPA.res$Q), 
                        rmseG = tess3r::ComputeRmseWithBestPermutation(data.list$Freq, GtoFreq(tess3MCPA.res$G, 1)),
                        method = "APLS",
                        n = n,
                        L = ncol(data.list$admixed.genotype),
                        rep = r),
             data.frame(rmseQ = tess3r::ComputeRmseWithBestPermutation(data.list$Q, tess3OQA.res$Q),
                        rmseG = tess3r::ComputeRmseWithBestPermutation(data.list$Freq, GtoFreq(tess3OQA.res$G, 1)),
                        method = "AQP",
                        n = n,
                        L = ncol(data.list$admixed.genotype),
                        rep = r),
             data.frame(rmseQ = tess3r::ComputeRmseWithBestPermutation(data.list$Q, tess3Old.res$Q), 
                        rmseG = tess3r::ComputeRmseWithBestPermutation(data.list$Freq, GtoFreq(tess3Old.res$G, 1)),
                        method = "TESS3",
                        n = n,
                        L = ncol(data.list$admixed.genotype),
                        rep = r))
    }
  return(df)
}

fig1.exp.L <- function(simu.param, Ls, rep) {
  df <- foreach(L = Ls, .combine = 'rbind') %:% 
    foreach(r = 1:rep, .combine = 'rbind') %dopar% {
      simu.param$nsites.neutral <- L
      data.list <- sample.data(simu.param)
      tess3MCPA.res <- tess3.wrapper(data.list, 2, "MCPA")
      tess3OQA.res <- tess3.wrapper(data.list, 2, "OQA")
      tess3Old.res <- tess3old.wrapper(data.list, 2, alpha = 0.03)
      rbind( data.frame(rmseQ = tess3r::ComputeRmseWithBestPermutation(data.list$Q, tess3MCPA.res$Q), 
                        rmseG = tess3r::ComputeRmseWithBestPermutation(data.list$Freq, GtoFreq(tess3MCPA.res$G, 1)),
                        method = "APLS",
                        nsites.neutral = L,
                        L = ncol(data.list$admixed.genotype),
                        rep = r),
             data.frame(rmseQ = tess3r::ComputeRmseWithBestPermutation(data.list$Q, tess3OQA.res$Q),
                        rmseG = tess3r::ComputeRmseWithBestPermutation(data.list$Freq, GtoFreq(tess3OQA.res$G, 1)),
                        method = "AQP",
                        nsites.neutral = L,
                        L = ncol(data.list$admixed.genotype),
                        rep = r),
             data.frame(rmseQ = tess3r::ComputeRmseWithBestPermutation(data.list$Q, tess3Old.res$Q), 
                        rmseG = tess3r::ComputeRmseWithBestPermutation(data.list$Freq, GtoFreq(tess3Old.res$G, 1)),
                        method = "TESS3",
                        nsites.neutral = L,
                        L = ncol(data.list$admixed.genotype),
                        rep = r))
    }
  return(df)
}



##################################################
############## FIG 4 EXP FUNC ####################
##################################################

fig4.aux <- function(res, method, n, L, r) {
  data.frame(method = method,
             n = n,
             L = L,
             rep = r,
             it = sum(!is.na(res$times)),
             time.per.it.mean = mean(res$times, na.rm = TRUE), 
             time.per.it.sd = sd(res$times, na.rm = TRUE), 
             time.per.it.se = sd(res$times, na.rm = TRUE) / sqrt(sum(!is.na(res$times))))
}

fig4.exp.n <- function(sample.data, ns, rep, L, K, tess3Old.alpha = 0.03) {
  
  df <- foreach(n = ns, .combine = 'rbind') %:% 
    foreach(r = 1:rep, .combine = 'rbind') %dopar% {
      data.list <- sample.data(n, L)
      tess3OQA.res <- tess3.wrapper(data.list, K, "OQA")
      end <- 0
      while (tess3OQA.res$err & end < 1000) {
        data.list <- sample.data(n, L)
        tess3OQA.res <- tess3.wrapper(data.list, K, "OQA")
        end <- end + 1
      }
      if (tess3OQA.res$err) {
        stop("ERROR")
      }
      tess3MCPA.res <- tess3.wrapper(data.list, K, "MCPA")
      tess3Old.res <- tess3old.wrapper(data.list, K, alpha = tess3Old.alpha)
      rbind( fig4.aux(tess3MCPA.res, "APLS", n, ncol(data.list$X), r),
             fig4.aux(tess3OQA.res, "AQP", n, ncol(data.list$X), r),
             fig4.aux(tess3Old.res, "TESS3", n, ncol(data.list$X), r)
      )
    }
  return(df)
}

fig4.exp.L <- function(sample.data, Ls, rep, n, K, tess3Old.alpha = 0.03) {
  
  df <- foreach(L = Ls, .combine = 'rbind') %:% 
    foreach(r = 1:rep, .combine = 'rbind') %dopar% {
      data.list <- sample.data(n, L)
      tess3OQA.res <- tess3.wrapper(data.list, K, "OQA")
      end <- 0
      while (tess3OQA.res$err & end < 1000) {
        data.list <- sample.data(n, L)
        tess3OQA.res <- tess3.wrapper(data.list, K, "OQA")
        end <- end + 1
      }
      if (tess3OQA.res$err) {
        stop("ERROR")
      }
      
      tess3MCPA.res <- tess3.wrapper(data.list, K, "MCPA")
      tess3Old.res <- tess3old.wrapper(data.list, K, alpha = tess3Old.alpha)
      rbind( fig4.aux(tess3MCPA.res, "APLS", n, ncol(data.list$X), r),
             fig4.aux(tess3OQA.res, "AQP", n, ncol(data.list$X), r),
             fig4.aux(tess3Old.res, "TESS3", n, ncol(data.list$X), r)
      )
    }
  return(df)
}