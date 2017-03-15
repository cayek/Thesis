#' @export
Article3_pcaExp <- function(s,
                            s.name,
                            cluster.nb = NULL,
                            save = TRUE, bypass = FALSE) {

  long_init(cluster.nb = cluster.nb,
            bypass = bypass)

  exp <- PCAExperiment(s = s)
  exp$description <- paste0(exp$description, " on ", s.name)
  exp$name <- "Article3_pcaExp"

  exp <- runExperiment(exp)

  # save exp
  if (save) {
    dumpExperiment(exp)
  }
  exp
}
