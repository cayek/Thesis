source("setup.R")

# Experiment
simu.param <- list(n = 100,
                   nsites.neutral = 1e4,
                   nsites.selected = 0,
                   crossover.proba = 0.25 * 10 ^ -8,
                   m.neutral = 0.25 * 10 ^ -6,
                   m.selected = NULL,
                   mutation.rate.per.site = 0.25 * 10 ^ -7,
                   N0 = 10 ^ 6,
                   k = 0.5,
                   min.maf = 0.05)

  cores = 16
  registerDoParallel(cores = cores)

  simu.param$nsites.neutral <- 1e5

  simu.param$n <- 200
  df.L <- fig1.exp.L(simu.param, Ls = c(1e4, 2.5e4, 5e4, 1e5, 2e5, 3e5), rep = 5)
  save(df.L, file = paste0(res.dir,"L.rmse.R"))
