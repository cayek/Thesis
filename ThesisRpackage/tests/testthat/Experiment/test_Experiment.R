library(testthat)
context("experiment")

test_that("Experiment", {

  exp <- Experiment()
  #exp
})


test_that("Benchmark", {

  # init
  dir <- tempdir()
  bench.dir <- paste0(dir,"/BenchmarkDump/")
  unlink(bench.dir, recursive = TRUE, force = TRUE) #remove if it exist
  bench.dir <- initBenchmark(dir = tempdir())

  # get table
  bench.tbl <- getBenchmarkDb(bench.dir = bench.dir)
  bench.tbl
  expect_equal(dim(bench.tbl), c(0,7))

  ##  add an exp
  exp <- Experiment(benchmakdir = bench.dir)
  dumpExperiment(exp)

  # get table
  bench.tbl <- getBenchmarkDb(bench.dir = bench.dir)
  bench.tbl
  expect_equal(dim(bench.tbl), c(1,7))

  ## retrieve it
  exp.retrieve <- retrieveExperiment(id = 1, bench.dir = bench.dir)
  expect_equal(exp.retrieve$seed, exp$seed)
})

