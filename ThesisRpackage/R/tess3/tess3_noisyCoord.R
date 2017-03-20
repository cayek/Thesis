main_tess3_noisyCoord <- function(exp,
                                  n, nsites.neutral, m.neutral,
                                  noise.signal, r, compute.vario) {
  ## helpers
  vario <- function(dat, label) {
    if (compute.vario) {
      vario.gen <- tess3r::CalculateEmpiricalGenSemivariogram(dat$G,
                                                              dat$ploidy,
                                                              dat$coord)
      vario.gen %>%
        dplyr::mutate(label = label, rep = r,
                      n = dat$n,
                      L = dat$L,
                      m.neutral = dat$m.neutral,
                      Fst = mean(dat$Fst),
                      Fst.theorical = dat$Fst.theorical,
                      nsites.neutral = dat$nsites.neutral)
    } else {
      tibble()
    }

  }
  res <- function(m, dat, method, noise.signal, rmse.Q.snmf, rmse.G.snmf) {
    tibble(rmse.Q.snmf = rmse.Q.snmf,
           rmse.G.snmf = rmse.G.snmf,
           rmse.Q.tess3 = tess3r::ComputeRmseWithBestPermutation(m$Q,
                                                           dat$Q),
           rmse.G.tess3 = tess3r::ComputeRmseWithBestPermutation(tess3r::GtoFreq(m$G, dat$ploidy),
                                                           dat$Freq),
           noise.signal = noise.signal,
           rep = r,
           n = dat$n,
           L = dat$L,
           m.neutral = dat$m.neutral,
           Fst = mean(dat$Fst),
           Fst.theorical = dat$Fst.theorical,
           nsites.neutral = dat$nsites.neutral)
  }

  ## simulation
  s <- Tess3Sampler(n = n,
                    nsites.neutral = nsites.neutral,
                    nsites.selected = 0,
                    crossover.proba = 0.25 * 1e-8,
                    m.neutral = m.neutral,
                    m.selected = NULL,
                    mutation.rate.per.site = 0.25 * 1e-7,
                    N0 = 1e6,
                    k = 0.5,
                    min.maf = 0.05)
  dat <- sampl(s)
  sd.long <- sd(dat$coord[,1]) # only longitude is important

  ## method
  m.snmf <- sNMFMethod(K = 2)
  m.tess3 <- tess3Method(K = 2)

  ## run of snmf
  m.snmf <- fit(m.snmf, dat)
  rmse.Q.snmf = tess3r::ComputeRmseWithBestPermutation(m.snmf$Q,
                                                  dat$Q)
  rmse.G.snmf = tess3r::ComputeRmseWithBestPermutation(tess3r::GtoFreq(m.snmf$G, dat$ploidy),
                                                  dat$Freq)

  ## run of tess3 with random coord
  # dat.noisy <- dat
  # dat.noisy$coord[,1] <- rnorm(n, mean = 0.0, sd = sd.long)
  # exp$vario.gen <- vario(dat.noisy, label = "random coord") %>%
  #   rbind(exp$vario.gen)
  #
  # tess3.res <- fit(m.tess3, dat.noisy)
  #
  # exp$df.res <- res(m = tess3.res,
  #                   dat = dat.noisy,
  #                   method = "tess3+random coord", noise.signal = 0.0) %>%
  #   rbind(exp$df.res)


  ## run of tess3 with noisy coord
  exp$df.res <- foreach(ns = noise.signal, .combine = 'rbind') %dopar%
  {
    dat.noisy <- dat
    dat.noisy$coord[,1] <- dat.noisy$coord[,1] +
      rnorm(n, mean = 0.0, sd = ns * sd.long)
    exp$vario.gen <- vario(dat.noisy, label = paste0("ns=",ns)) %>%
      rbind(exp$vario.gen)

    tess3.res <- fit(m.tess3, dat.noisy)

    res(m = tess3.res,
        dat = dat.noisy,
        noise.signal = ns,
        rmse.Q.snmf = rmse.Q.snmf,
        rmse.G.snmf = rmse.G.snmf)
  } %>%
    rbind(exp$df.res)
  exp
}


#' @export
long_tess3_noisyCoord <- function(ns = c(50, 500),
                                  nsites.neutral = c(1.5 * 1e4,
                                                     1.2 * 1e5),
                                  m.neutral =  c(0.25 * 0.05 * 1e-6,
                                                 0.25 * 0.5 * 1e-6,
                                                 0.25 * 1 * 1e-6,
                                                 0.25 * 1.5 * 1e-6,
                                                 0.25 * 2 * 1e-6,
                                                 0.25 * 2.5 * 1e-6,
                                                 0.25 * 3 * 1e-6,
                                                 0.25 * 5 * 1e-6),
                                  noise.signal = c(0.0, 0.2, 0.5,0.8, 1.0, 2.0, 3.0),
                                  nb.rep = 5,
                                  compute.vario = FALSE,
                                  cluster.nb = NULL,
                                  save = TRUE, bypass = FALSE) {
  ## init
  long_init(cluster.nb = cluster.nb,
            bypass = bypass)

  ## experiment
  exp <- Experiment(name = "long_tess3_noisyCoord",
                    description = make_description("Run of tess3 on simulation with noisy coord",
                                                   ns = ns,
                                                   noise.signal = noise.signal,
                                                   nb.rep = nb.rep,
                                                   nsites.neutral = nsites.neutral,
                                                   m.neutral = m.neutral))
  class(exp) <- c("long_tess3_noisyCoord", class(exp))

  exp$df.res <- tibble()
  exp$vario.gen <- tibble()

  ## main
  for (r in 1:nb.rep) {
    for (n in ns) {
      for (n.n in nsites.neutral) {
        for (m.n in m.neutral) {
          DebugMessage(paste0("run r=",r, " n=", n, "n.n=",n.n, "m.n=",m.n))
          exp <- main_tess3_noisyCoord(exp, n = n,
                                       nsites.neutral = n.n,
                                       m.neutral = m.n,
                                       noise.signal, r, compute.vario)
        }
      }
    }

  }

  ## return
  # save exp
  if (save) {
    dumpExperiment(exp)
  }
  exp
}

#' @export
plot_tess3_noisyCoord <- function(exp) {

  assertthat::assert_that(class(exp)[1] == "long_tess3_noisyCoord")


  ## compute rel.diff.rmse = rmse_snmf - rmse_tess3 / rmse_snmf
  toplot.aux <- exp$df.res %>%
    group_by(n, Fst.theorical, nsites.neutral, noise.signal) %>%
    dplyr::mutate(## Q
      rel.diff.rmse.Q = (rmse.Q.snmf - rmse.Q.tess3) / rmse.Q.snmf,

      ## G
      rel.diff.rmse.G = (rmse.G.snmf - rmse.G.tess3) / rmse.Q.snmf
    ) %>%
    ungroup()


  toplot <- toplot.aux %>%
    group_by(n, Fst.theorical, nsites.neutral, noise.signal) %>%
    summarise(rel.diff.rmse.Q.mean = mean(rel.diff.rmse.Q),
              rel.diff.rmse.Q.mean.se = sd(rel.diff.rmse.Q) / sqrt(length(rel.diff.rmse.Q)),

              rel.diff.rmse.G.mean = mean(rel.diff.rmse.G),
              rel.diff.rmse.G.mean.se = sd(rel.diff.rmse.G) / sqrt(length(rel.diff.rmse.G))
    )  %>%
    ungroup()

  ## main plot
  pl <- ggplot(toplot, aes(x = noise.signal, y = rel.diff.rmse.Q.mean,
                           color = as.factor(Fst.theorical))) +
    geom_point() +
    geom_line() +
    facet_grid(nsites.neutral ~ n)

  pl
}


#' @export
plot_tess3_noisyCoord_vario <- function(exp, n, nsites.neutral, m.neutral) {

  assertthat::assert_that(class(exp)[1] == "long_tess3_noisyCoord")

  stop("TODO")

  ## variogram
  if (nrow(exp$vario.gen) != 0) {
    variogram.pl <- ggplot(exp$vario.gen, aes(x = h, y = semi.variance, size = size)) +
      geom_point()  +
      labs(y = "Semivariance",
           x = "Geographic distance") +
      theme_gray(base_size = 12) +
      scale_size_continuous(range = c(1,3)) +
      guides(size = guide_legend(title = "Bin size")) +
      facet_grid(label~.)
    print(variogram.pl)
  }

  ## main plot
  snmf.rmse.Q <- mean(exp$df.res$rmse.Q[exp$df.res$method == "snmf"])
  tess3.random.rmse.Q <- mean(exp$df.res$rmse.Q[exp$df.res$method == "tess3+random coord"])
  toplot <- exp$df.res %>%
    dplyr::filter(method == "tess3")
  pl <- ggplot(toplot, aes(x = noise.signal, y = rmse.Q)) +
    geom_point() +
    geom_smooth() +
    geom_hline(yintercept = snmf.rmse.Q, colour = "red") +
    geom_hline(yintercept = tess3.random.rmse.Q, colour = "green")
  print(pl)
}
