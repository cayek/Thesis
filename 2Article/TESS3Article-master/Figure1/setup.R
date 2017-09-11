################################################################################
# Setup
library(tess3r)
library(raster)
library(ggplot2)
library(reshape2)
library(dplyr)
library(foreach)
library(doParallel)
#library(tess3r)
#library(LEA)
library(DescTools)
source("../functions.R")
source("../figureParams.R")

################################################################################
# Functions

################################################################################
# dir
res.dir <- "./Experiments/Results/"
fig.dir <- "./Figures/"
data.dir <- "../Data/"
