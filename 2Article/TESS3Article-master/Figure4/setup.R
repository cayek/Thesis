################################################################################
# Setup
library(raster)
library(ggplot2)
library(reshape2)
library(dplyr)
library(tikzDevice)
library(gridExtra)
library(cowplot)
source("../functions.R")
source("../figureParams.R")

################################################################################
# dir
res.dir <- "./Experiments/Results/"
fig.dir <- "./Figures/"
data.dir <- "../Data/"

