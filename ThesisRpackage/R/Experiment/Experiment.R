#' Experiment class
#'
#'
#' @export
Experiment <- function(name = "Default",
                       date = as.character(Sys.time()),
                       seed = sample.int(.Machine$integer.max, 1),
                       machine = as.character(Sys.info()[4]),
                       runtime = 0,
                       dumpfile = "NULL",
                       description = "NULL",
                       benchmakdir = getOption("thesis.dump.file")) {
  if (!is.null(benchmakdir)) {
    benchmakdir = normalizePath(benchmakdir)
  }
  structure(as.list(environment()), class = "Experiment")
}

################################################################################
# Method

#' Run the experiment
#'
#' @export
runExperiment <- function(exp){
  UseMethod("runExperiment")
}

#' Title
#'
#'
#' @export
print.Experiment <- function(x) {
  str(x)
}

#' dumb the object (only for experiment object)
#'
#' @export
dumpExperiment <- function(exp){
  UseMethod("dumpExperiment")
}

#' @export
dumpExperiment.Experiment <- function(exp) {

  # dump result
  exp$dumpfile <- tempfile(tmpdir = exp$benchmakdir, fileext = ".rds")
  saveRDS(exp, file = exp$dumpfile)

  row <- data.frame(name = I(exp$name),
                    date = I(exp$date),
                    seed = I(exp$seed),
                    machine = I(exp$machine),
                    runtime = I(exp$runtime),
                    description = I(exp$description),
                    dumpfile = I(exp$dumpfile))
  bench.file <- paste0(exp$benchmakdir,"/benchmark.sqlite3")
  add_to_table(bench.file, row)
}

################################################################################
# Functions

#' @export
initBenchmark <- function(dir = getwd()) {

  # Create benchmark dir
  bench.dir <- paste0(dir,"/ExperimentDump/")
  tryCatch({
    dir.create(bench.dir)},
  warning = function(w) {
    stop(w)
  })

  # create sqlite db
  db.file <- paste0(bench.dir, "benchmark.sqlite3")
  db <- RSQLite::dbConnect( RSQLite::SQLite() , dbname = db.file )
  res <- RSQLite::dbSendQuery(conn = db,
                              "CREATE TABLE experiment(
                  name CHARACTER,
                  date CHARACTER,
                  seed INTEGER,
                  machine CHARACTER,
                  runtime FLOAT,
                  description CHARACTER,
                  dumpfile CHARACTER
                  )")
  RSQLite::dbClearResult(res)
  RSQLite::dbDisconnect(db)

  # add to .Rprofile
  flog.info(paste0("Add to .Rprofile: \n
      options( thesis.dump.file = \"",bench.dir,"\")", sep = ""))
  bench.dir
}

#' @export
getBenchmarkDb <- function(bench.dir = getOption("thesis.dump.file")) {
  bench.file <- paste0(bench.dir,"/benchmark.sqlite3")
  con <- RSQLite::dbConnect( RSQLite::SQLite() , dbname = bench.file )
  data <- RSQLite::dbReadTable(conn = con,name = "experiment")
  RSQLite::dbDisconnect(con)
  as_tibble(data)
}

#' @export
printBenchmarkDb <- function(bench.dir = getOption("thesis.dump.file")) {
  bench.file <- paste0(bench.dir,"/benchmark.sqlite3")
  con <- RSQLite::dbConnect( RSQLite::SQLite() , dbname = bench.file )
  data <- RSQLite::dbReadTable(conn = con,name = "experiment")
  RSQLite::dbDisconnect(con)
  df <- as_tibble(data)
  # n <- nrow(df)
  # df %>%
  #   dplyr::mutate(n.row = 1:n) %>%
  #   dplyr::select(n.row, name) %>%
  #   print(n = n)
  getBenchmarkDb()$description
}

#' @export
retrieveExperiment <- function(id, bench.dir = getOption("thesis.dump.file")) {
  bench.tbl <- getBenchmarkDb(bench.dir = bench.dir)
  readRDS(paste0(bench.dir,"/",basename(bench.tbl$dumpfile[id])))
}

#' @export
removeExperiment <- function(id, bench.dir = getOption("thesis.dump.file")) {

  bench.tbl <- getBenchmarkDb(bench.dir = bench.dir)
  bench.file <- paste0(bench.dir,"/benchmark.sqlite3")
  ## remove file
  dumpfile <- bench.tbl$dumpfile[id]
  file.remove(paste0(bench.dir,"/",basename(dumpfile)))
  ## remove from db
  remove_from_table(db.file = bench.file,
                    condition = paste0("dumpfile = ","\'",dumpfile,"'"))
}

################################################################################
# helpers

add_to_table <- function(db.file, rows, tablename = "experiment") {
  con <- RSQLite::dbConnect( RSQLite::SQLite() , dbname = db.file )
  res <- RSQLite::dbSendQuery(conn = con, "PRAGMA foreign_keys = ON")
  RSQLite::dbClearResult(res)
  RSQLite::dbWriteTable(conn = con, name = tablename, value = rows, append = TRUE)
  RSQLite::dbDisconnect(con)
}


remove_from_table <- function(db.file, condition, tablename = "experiment") {
  con <- RSQLite::dbConnect( RSQLite::SQLite() , dbname = db.file )
  res <- RSQLite::dbSendQuery(conn = con, "PRAGMA foreign_keys = ON")
  RSQLite::dbClearResult(res)
  res <- RSQLite::dbSendQuery(conn = con, paste("DELETE FROM", tablename, "WHERE", condition))
  RSQLite::dbClearResult(res)
  RSQLite::dbDisconnect(con)
}

