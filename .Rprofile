library(tidyverse)
library(testthat)
futile.logger::flog.threshold(futile.logger::TRACE, name = "ThesisRpackage")
futile.logger::flog.threshold(futile.logger::INFO, name = "console")
if (Sys.info()["nodename"] != "patator.imag.fr") {
  library(devtools)
}

options(thesis.dump.file = "/home/cayek/Projects/Thesis/OUTPUT/ExperimentDump/")
options(ThesisRpackage.debug = "TRUE")
options(tess3.ms = "~/BiocompSoftware/msdir/ms")
