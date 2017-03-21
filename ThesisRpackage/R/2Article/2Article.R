Article2.env <- new.env()

#' Retrun Article2 env functions
#'
#' @export
get_Article2 <- function() {
  Article2.env
}



## back up of results
Article2_bk_results <- function() {
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

}
