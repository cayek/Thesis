main_tess3_noisyCoord <- function(exp, n, noise.signal, r, compute.vario) {
  ## helpers
  vario <- function(dat, label) {
    if (compute.vario) {
      vario.gen <- tess3r::CalculateEmpiricalGenSemivariogram(dat$G,
                                                              dat$ploidy,
                                                              dat$coord)
      vario.gen %>%
        dplyr::mutate(label = label, rep = r)
    } else {
      tibble()
    }

  }
  res <- function(m, dat, method, noise.signal) {
    tibble(rmse.Q = tess3r::ComputeRmseWithBestPermutation(m$Q,
                                                           dat$Q),
           rmse.G = tess3r::ComputeRmseWithBestPermutation(tess3r::GtoFreq(m$G, dat$ploidy),
                                                           dat$Freq),
           method = method,
           noise.signal = noise.signal,
           rep = r)
  }

  ## simulation
  s <- Tess3Sampler(n = n,
                    nsites.neutral = 1.5 * 1e4,
                    nsites.selected = 0,
                    crossover.proba = 0.25 * 1e-8,
                    m.neutral = 0.25 * 0.5 * 1e-6,
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
  exp$df.res <- res(m = m.snmf,
                    dat = dat,
                    method = "snmf", noise.signal = 0.0) %>%
    rbind(exp$df.res)

  ## run of tess3 with random coord
  dat.noisy <- dat
  dat.noisy$coord[,1] <- rnorm(n, mean = 0.0, sd = sd.long)
  exp$vario.gen <- vario(dat.noisy, label = "random coord") %>%
    rbind(exp$vario.gen)

  tess3.res <- fit(m.tess3, dat.noisy)

  exp$df.res <- res(m = tess3.res,
                    dat = dat.noisy,
                    method = "tess3+random coord", noise.signal = 0.0) %>%
    rbind(exp$df.res)


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
        method = "tess3", noise.signal = ns)
  } %>%
    rbind(exp$df.res)
  exp
}


#' @export
long_tess3_noisyCoord <- function(n = 500,
                                  noise.signal = c(0.0, 0.5, 1.0, 2.0, 3.0),
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
                                                   n = n, noise.signal = noise.signal, nb.rep = nb.rep))
  class(exp) <- c("long_tess3_noisyCoord", class(exp))

  exp$df.res <- tibble()
  exp$vario.gen <- tibble()

  ## main
  for (r in 1:nb.rep) {
    exp <- main_tess3_noisyCoord(exp, n, noise.signal, r, compute.vario)
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
