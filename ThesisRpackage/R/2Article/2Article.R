Article2.env <- new.env()

#' Retrun Article2 env functions
#'
#' @export
get_Article2 <- function() {
  Article2.env
}



## back up of results
Article2_bk_results <- function() {

  stop("DO NOT RUN IT !!!")


  ## Figure1
  res.dir <- "~/Projects/TESS3Article/Figure1/Experiments/Results/"
  exp <- Experiment(name = "2Article_figure1",
                    description = "BK of 2Article_figure1")
  load(paste0(res.dir,"L.rmse.RData"))
  load(paste0(res.dir,"n.rmse.RData"))
  exp$df.L <- df.L
  exp$df.n <- df.n
  dumpExperiment(exp)
  printBenchmarkDb()


  ## Figure2
  res.dir <- "~/Projects/TESS3Article/Figure2/Experiments/Results/"
  exp <- Experiment(name = "2Article_figure2",
                    description = "BK of 2Article_figure2")
  load(paste0(res.dir,"rmse.fst.RData"))
  exp$df <- df
  dumpExperiment(exp)
  printBenchmarkDb()

  ## Figure3
  res.dir <- "~/Projects/TESS3Article/Figure3/Experiments/Results/"
  exp <- Experiment(name = "2Article_figure3",
                    description = "BK of 2Article_figure3")
  load(paste0(res.dir,"auc.RData"))
  exp$df <- df
  dumpExperiment(exp)
  printBenchmarkDb()


  ## Figure4
  res.dir <- "~/Projects/TESS3Article/Figure4/Experiments/Results/"
  exp <- Experiment(name = "2Article_figure4",
                    description = "BK of 2Article_figure4")
  load(paste0(res.dir,"runtimes.RData"))
  exp$df.L <- df.L
  exp$df.n <- df.n
  dumpExperiment(exp)
  printBenchmarkDb()


  ## Figure5
  res.dir <- "~/Projects/TESS3Article/Figure5/Experiments/Results/TAIR9/"
  exp <- Experiment(name = "2Article_figure5_TAIR9",
                    description = "BK of 2Article_figure4 TAIR9")
  load(paste0(res.dir,"err.K110.rep5.RData"))
  load(paste0(res.dir,"snpsTAIR9vepTAIR10.RData"))
  load(paste0(res.dir,"variogram.RData"))
  load(paste0(res.dir,"snmfK6.RData"))
  load(paste0(res.dir,"tess3K6.sigmaHeat1.5.RData"))
  exp$err.df <- err.df
  exp$vario.gen <- vario.gen
  exp$vep.res <- vep.res
  exp$tess3Main.obj <- tess3Main.obj
  exp$snmf.obj <- snmf.obj
  dumpExperiment(exp)
  printBenchmarkDb()

}
