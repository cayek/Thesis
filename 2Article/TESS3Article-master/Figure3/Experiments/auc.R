source("setup.R")

simu.param <- list(n = 100,
                   nsites.neutral = 1 * 1e5,
                   nsites.selected = 1 * 1e2,
                   crossover.proba = 0.25 * 1e-8,
                   m.neutral = 0.25 * 10 * 1e-6,
                   m.selected = 0.25 * 0.1 * 1e-6,
                   mutation.rate.per.site = 0.25 * 1e-7,
                   N0 = 1e6,
                   k = 0.5,
                   min.maf = 0.05)

# Experiment

  cores = 16
  registerDoParallel(cores = cores)

  m.ms <- c(10, 40, 80, 100, 150)

  df <- data.frame()
  df <- rbind(fig3.experiment(simu.param, m.ms = m.ms, rep = 10), df)

  save(df, file = paste0(res.dir,"auc.RData"))
